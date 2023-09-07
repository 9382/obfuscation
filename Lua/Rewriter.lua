
--
-- Minify.lua
--
-- A compilation of all of the neccesary code to Minify a source file, all into one single
-- script for usage on Roblox. Needed to deal with Roblox' lack of `require`.
--

local function lookupify(tb)
	for _, v in pairs(tb) do
		tb[v] = true
	end
	return tb
end

local function CountTable(tb)
	local c = 0
	for _ in pairs(tb) do c = c + 1 end
	return c
end

local function PrintTable(tb, atIndent)
	if tb.Print then
		return tb.Print()
	end
	atIndent = atIndent or 0
	local useNewlines = (CountTable(tb) > 1)
	local baseIndent = string.rep('		', atIndent+1)
	local out = "{"..(useNewlines and '\n' or '')
	for k, v in pairs(tb) do
		if type(v) ~= 'function' then
			out = out..(useNewlines and baseIndent or '')
			if type(k) == 'number' then
				--nothing to do --I disagree
				out = out.."["..k.."] = "
			elseif type(k) == 'string' and k:match("^[A-Za-z_][A-Za-z0-9_]*$") then 
				out = out..k.." = "
			elseif type(k) == 'string' then
				out = out.."[\""..k.."\"] = "
			else
				out = out.."["..tostring(k).."] = "
			end
			if type(v) == 'string' then
				out = out.."\""..v.."\""
			elseif type(v) == 'number' then
				out = out..v
			elseif type(v) == 'table' then
				out = out..PrintTable(v, atIndent+(useNewlines and 1 or 0))
			else
				out = out..tostring(v)
			end
			if next(tb, k) then
				out = out..","
			end
			if useNewlines then
				out = out..'\n'
			end
		end
	end
	out = out..(useNewlines and string.rep('		', atIndent) or '').."}"
	return out
end

local WhiteChars = lookupify{' ', '\n', '\t', '\r'}
local EscapeLookup = {['\r'] = '\\r', ['\n'] = '\\n', ['\t'] = '\\t', ['"'] = '\\"', ["'"] = "\\'"}
local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local HexDigits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
														'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f'}

local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}

local Keywords = lookupify{
		'and', 'break', 'continue', 'do', 'else', 'elseif',
		'end', 'false', 'for', 'function', 'goto', 'if',
		'in', 'local', 'nil', 'not', 'or', 'repeat',
		'return', 'then', 'true', 'until', 'while',
};

local function LexLua(src)
	--token dump
	local tokens = {}

	local st, err = pcall(function()
		--line / char / pointer tracking
		local p = 1
		local line = 1
		local char = 1

		--get / peek functions
		local function get()
			local c = src:sub(p,p)
			if c == '\n' then
				char = 1
				line = line + 1
			else
				char = char + 1
			end
			p = p + 1
			return c
		end
		local function peek(n)
			n = n or 0
			return src:sub(p+n,p+n)
		end
		local function consume(chars)
			local c = peek()
			for i = 1, #chars do
				if c == chars:sub(i,i) then return get() end
			end
		end

		--shared stuff
		local function generateError(err)
			return error(">> :"..line..":"..char..": "..err, 0)
		end

		local function tryGetLongString()
			local start = p
			if peek() == '[' then
				local equalsCount = 0
				while peek(equalsCount+1) == '=' do
					equalsCount = equalsCount + 1
				end
				if peek(equalsCount+1) == '[' then
					--start parsing the string. Strip the starting bit
					for _ = 0, equalsCount+1 do get() end

					--get the contents
					local contentStart = p
					while true do
						--check for eof
						if peek() == '' then
							generateError("Expected `]"..string.rep('=', equalsCount).."]` near <eof>.", 3)
						end

						--check for the end
						local foundEnd = true
						if peek() == ']' then
							for i = 1, equalsCount do
								if peek(i) ~= '=' then foundEnd = false end
							end 
							if peek(equalsCount+1) ~= ']' then
								foundEnd = false
							end
						else
							foundEnd = false
						end
						--
						if foundEnd then
							break
						else
							get()
						end
					end

					--get the interior string
					local contentString = src:sub(contentStart, p-1)

					--found the end. Get rid of the trailing bit
					for i = 0, equalsCount+1 do get() end

					--get the exterior string
					local longString = src:sub(start, p-1)

					--return the stuff
					return contentString, longString
				else
					return nil
				end
			else
				return nil
			end
		end

		--main token emitting loop
		while true do
			--get leading whitespace. The leading whitespace will include any comments 
			--preceding the token. This prevents the parser needing to deal with comments 
			--separately.
			local leadingWhite = ''
			while true do
				local c = peek()
				if WhiteChars[c] then
					--whitespace
					leadingWhite = leadingWhite..get()
				elseif c == '-' and peek(1) == '-' then
					--comment
					get();get()
					leadingWhite = leadingWhite..'--'
					local _, wholeText = tryGetLongString()
					if wholeText then
						leadingWhite = leadingWhite..wholeText
					else
						while peek() ~= '\n' and peek() ~= '' do
							leadingWhite = leadingWhite..get()
						end
					end
				else
					break
				end
			end

			--get the initial char
			local thisLine = line
			local thisChar = char
			local errorAt = ":"..line..":"..char..":> "
			local c = peek()

			--symbol to emit
			local toEmit = nil

			--branch on type
			if c == '' then
				--eof
				toEmit = {Type = 'Eof'}

			elseif UpperChars[c] or LowerChars[c] or c == '_' then
				--ident or keyword
				local start = p
				repeat
					get()
					c = peek()
				until not (UpperChars[c] or LowerChars[c] or Digits[c] or c == '_')
				local dat = src:sub(start, p-1)
				if Keywords[dat] then
					toEmit = {Type = 'Keyword', Data = dat}
				else
					toEmit = {Type = 'Ident', Data = dat}
				end

			elseif Digits[c] or (peek() == '.' and Digits[peek(1)]) then
				--number const
				local start = p
				if c == '0' and peek(1) == 'x' then
					get();get()
					while HexDigits[peek()] do get() end
					if consume('Pp') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				else
					while Digits[peek()] do get() end
					if consume('.') then
						while Digits[peek()] do get() end
					end
					if consume('Ee') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				end
				toEmit = {Type = 'Number', Data = src:sub(start, p-1)}

			elseif c == '\'' or c == '\"' then
				local start = p
				--string const
				local delim = get()
				local contentStart = p
				while true do
					local c = get()
					if c == '\\' then
						get() --get the escape char
					elseif c == delim then
						break
					elseif c == '' then
						generateError("Unfinished string near <eof>")
					end
				end
				local content = src:sub(contentStart, p-2)
				local constant = src:sub(start, p-1)
				toEmit = {Type = 'String', Data = constant, Constant = content}

			elseif c == '[' then
				local content, wholetext = tryGetLongString()
				if wholetext then
					toEmit = {Type = 'String', Data = wholetext, Constant = content}
				else
					get()
					toEmit = {Type = 'Symbol', Data = '['}
				end

			elseif consume('>=<') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = c..'='}
				else
					toEmit = {Type = 'Symbol', Data = c}
				end

			elseif consume('~') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = '~='}
				else
					generateError("Unexpected symbol `~` in source.", 2)
				end

			elseif consume('.') then
				if consume('.') then
					if consume('.') then
						toEmit = {Type = 'Symbol', Data = '...'}
					else
						toEmit = {Type = 'Symbol', Data = '..'}
					end
				else
					toEmit = {Type = 'Symbol', Data = '.'}
				end

			elseif consume(':') then
				if consume(':') then
					toEmit = {Type = 'Symbol', Data = '::'}
				else
					toEmit = {Type = 'Symbol', Data = ':'}
				end

			elseif Symbols[c] then
				get()
				toEmit = {Type = 'Symbol', Data = c}

			else
				local contents, all = tryGetLongString()
				if contents then
					toEmit = {Type = 'String', Data = all, Constant = contents}
				else
					generateError("Unexpected Symbol `"..c.."` in source.", 2)
				end
			end

			--add the emitted symbol, after adding some common data
			toEmit.LeadingWhite = leadingWhite
			toEmit.Line = thisLine
			toEmit.Char = thisChar
			toEmit.Print = function()
				return "<"..(toEmit.Type..string.rep(' ', 7-#toEmit.Type)).."	"..(toEmit.Data or '').." >"
			end
			tokens[#tokens+1] = toEmit

			--halt after eof has been emitted
			if toEmit.Type == 'Eof' then break end
		end
	end)
	if not st then
		return false, err
	end

	--public interface:
	local tok = {}
	local savedP = {}
	local p = 1

	--getters
	function tok:Peek(n)
		n = n or 0
		return tokens[math.min(#tokens, p+n)]
	end
	function tok:Get()
		local t = tokens[p]
		p = math.min(p + 1, #tokens)
		return t
	end
	function tok:Is(t)
		return tok:Peek().Type == t
	end

	--save / restore points in the stream
	function tok:Save()
		savedP[#savedP+1] = p
	end
	function tok:Commit()
		savedP[#savedP] = nil
	end
	function tok:Restore()
		p = savedP[#savedP]
		savedP[#savedP] = nil
	end

	--either return a symbol if there is one, or return true if the requested
	--symbol was gotten.
	function tok:ConsumeSymbol(symb)
		local t = self:Peek()
		if t.Type == 'Symbol' then
			if symb then
				if t.Data == symb then
					self:Get()
					return true
				else
					return nil
				end
			else
				self:Get()
				return t
			end
		else
			return nil
		end
	end

	function tok:ConsumeKeyword(kw)
		local t = self:Peek()
		if t.Type == 'Keyword' and t.Data == kw then
			self:Get()
			return true
		else
			return nil
		end
	end

	function tok:IsKeyword(kw)
		local t = tok:Peek()
		return t.Type == 'Keyword' and t.Data == kw
	end

	function tok:IsSymbol(s)
		local t = tok:Peek()
		return t.Type == 'Symbol' and t.Data == s
	end

	function tok:IsEof()
		return tok:Peek().Type == 'Eof'
	end

	return true, tok
end


local function ParseLua(src)
	local st, tok = LexLua(src)
	if not st then
		return false, tok
	end
	--
	local function GenerateError(msg)
		local err = ">> :"..tok:Peek().Line..":"..tok:Peek().Char..": "..msg.."\n"
		--find the line
		local lineNum = 0
		for line in src:gmatch("[^\n]*\n?") do
			if line:sub(-1,-1) == '\n' then line = line:sub(1,-2) end
			lineNum = lineNum+1
			if lineNum == tok:Peek().Line then
				err = err..">> `"..line:gsub('\t','		').."`\n"
				for i = 1, tok:Peek().Char do
					local c = line:sub(i,i)
					if c == '\t' then 
						err = err..'		'
					else
						err = err..' '
					end
				end
				err = err.."	 ^---"
				break
			end
		end
		return err
	end
	--
	local VarUid = 0
	local GlobalVarGetMap = {}
	local VarDigits = {
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
		'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
		's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
		'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
		'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
	}
	local function CreateScope(parent)
		local scope = {}
		scope.Parent = parent
		scope.LocalList = {}
		scope.LocalMap = {}
		function scope:RenameVars()
			for _, var in pairs(scope.LocalList) do
				local id;
				VarUid = 0
				repeat
					VarUid = VarUid + 1
					local varToUse = VarUid
					id = ''
					while varToUse > 0 do
						local d = varToUse % #VarDigits
						varToUse = (varToUse - d) / #VarDigits
						id = id..VarDigits[d+1]
					end
				until not GlobalVarGetMap[id] and not parent:GetLocal(id) and not scope.LocalMap[id] and not Keywords[id]
				var.Name = id
				scope.LocalMap[id] = var
			end
		end
		function scope:GetLocal(name)
			--first, try to get my variable 
			local my = scope.LocalMap[name]
			if my then return my end

			--next, try parent
			if scope.Parent then
				local par = scope.Parent:GetLocal(name)
				if par then return par end
			end

			return nil
		end
		function scope:CreateLocal(name)
			--create my own var
			local my = {}
			my.Scope = scope
			my.Name = name
			my.CanRename = true
			--
			scope.LocalList[#scope.LocalList+1] = my
			scope.LocalMap[name] = my
			--
			return my
		end
		local r = math.random(1e6,1e7-1)
		scope.Print = function() return "<Scope " .. r .. ">" end
		return scope
	end

	local ParseExpr;
	local ParseStatementList;

	local function ParseFunctionArgsAndBody(scope)
		local funcScope = CreateScope(scope)
		if not tok:ConsumeSymbol('(') then
			return false, GenerateError("`(` expected.")
		end

		--arg list
		local argList = {}
		local isVarArg = false
		while not tok:ConsumeSymbol(')') do
			if tok:Is('Ident') then
				local arg = funcScope:CreateLocal(tok:Get().Data)
				argList[#argList+1] = arg
				if not tok:ConsumeSymbol(',') then
					if tok:ConsumeSymbol(')') then
						break
					else
						return false, GenerateError("`)` expected.")
					end
				end
			elseif tok:ConsumeSymbol('...') then
				isVarArg = true
				if not tok:ConsumeSymbol(')') then
					return false, GenerateError("`...` must be the last argument of a function.")
				end
				break
			else
				return false, GenerateError("Argument name or `...` expected")
			end
		end

		--body
		local st, body = ParseStatementList(funcScope)
		if not st then return false, body end

		--end
		if not tok:ConsumeKeyword('end') then
			return false, GenerateError("`end` expected after function body")
		end

		local nodeFunc = {}
		nodeFunc.AstType = 'Function'
		nodeFunc.Scope = funcScope
		nodeFunc.Arguments = argList
		nodeFunc.Body = body
		nodeFunc.VarArg = isVarArg
		--
		return true, nodeFunc
	end


	local function ParsePrimaryExpr(scope)
		if tok:ConsumeSymbol('(') then
			local st, ex = ParseExpr(scope)
			if not st then return false, ex end
			if not tok:ConsumeSymbol(')') then
				return false, GenerateError("`)` Expected.")
			end
			--save the information about parenthesized expressions somewhere
			ex.ParenCount = (ex.ParenCount or 0) + 1
			return true, ex

		elseif tok:Is('Ident') then
			local id = tok:Get()
			local var = scope:GetLocal(id.Data)
			if not var then
				GlobalVarGetMap[id.Data] = true
			end
			--
			local nodePrimExp = {}
			nodePrimExp.AstType = 'VarExpr'
			nodePrimExp.Name = id.Data
			nodePrimExp.Local = var
			--
			return true, nodePrimExp
		else
			return false, GenerateError("primary expression expected")
		end
	end


	local function ParseSuffixedExpr(scope, onlyDotColon)
		--base primary expression
		local st, prim = ParsePrimaryExpr(scope)
		if not st then return false, prim end
		--
		while true do
			if tok:IsSymbol('.') or tok:IsSymbol(':') then
				local symb = tok:Get().Data
				if symb == ":" then
					scope:CreateLocal("self")
				end
				if not tok:Is('Ident') then
					return false, GenerateError("<Ident> expected.")
				end
				local id = tok:Get()
				local nodeIndex = {}
				nodeIndex.AstType = 'MemberExpr'
				nodeIndex.Base = prim
				nodeIndex.Indexer = symb
				nodeIndex.Ident = id
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('[') then
				local st, ex = ParseExpr(scope)
				if not st then return false, ex end
				if not tok:ConsumeSymbol(']') then
					return false, GenerateError("`]` expected.")
				end
				local nodeIndex = {}
				nodeIndex.AstType = 'IndexExpr'
				nodeIndex.Base = prim
				nodeIndex.Index = ex
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('(') then
				local args = {}
				while not tok:ConsumeSymbol(')') do
					local st, ex = ParseExpr(scope)
					if not st then return false, ex end
					args[#args+1] = ex
					if not tok:ConsumeSymbol(',') then
						if tok:ConsumeSymbol(')') then
							break
						else
							return false, GenerateError("`)` Expected.")
						end
					end
				end
				local nodeCall = {}
				nodeCall.AstType = 'CallExpr'
				nodeCall.Base = prim
				nodeCall.Arguments = args
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:Is('String') then
				--string call
				local nodeCall = {}
				nodeCall.AstType = 'StringCallExpr'
				nodeCall.Base = prim
				nodeCall.Arguments	= {tok:Get()}
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:IsSymbol('{') then
				--table call
				local st, ex = ParseExpr(scope)
				if not st then return false, ex end
				local nodeCall = {}
				nodeCall.AstType = 'TableCallExpr'
				nodeCall.Base = prim
				nodeCall.Arguments = {ex}
				--
				prim = nodeCall

			else
				break
			end
		end
		return true, prim
	end


	local function ParseSimpleExpr(scope)
		if tok:Is('Number') then
			local nodeNum = {}
			nodeNum.AstType = 'NumberExpr'
			nodeNum.Value = tok:Get()
			return true, nodeNum

		elseif tok:Is('String') then
			local nodeStr = {}
			nodeStr.AstType = 'StringExpr'
			nodeStr.Value = tok:Get()
			return true, nodeStr

		elseif tok:ConsumeKeyword('nil') then
			local nodeNil = {}
			nodeNil.AstType = 'NilExpr'
			return true, nodeNil

		elseif tok:IsKeyword('false') or tok:IsKeyword('true') then
			local nodeBoolean = {}
			nodeBoolean.AstType = 'BooleanExpr'
			nodeBoolean.Value = (tok:Get().Data == 'true')
			return true, nodeBoolean

		elseif tok:ConsumeSymbol('...') then
			local nodeDots = {}
			nodeDots.AstType = 'DotsExpr'
			return true, nodeDots

		elseif tok:ConsumeSymbol('{') then
			local v = {}
			v.AstType = 'ConstructorExpr'
			v.EntryList = {}
			--
			while true do
				if tok:IsSymbol('[') then
					--key
					tok:Get()
					local st, key = ParseExpr(scope)
					if not st then 
						return false, GenerateError("Key Expression Expected")
					end
					if not tok:ConsumeSymbol(']') then
						return false, GenerateError("`]` Expected")
					end
					if not tok:ConsumeSymbol('=') then
						return false, GenerateError("`=` Expected")
					end
					local st, value = ParseExpr(scope)
					if not st then
						return false, GenerateError("Value Expression Expected")
					end
					v.EntryList[#v.EntryList+1] = {
						Type = 'Key';
						Key = key;
						Value = value;
					}

				elseif tok:Is('Ident') then
					--value or key
					local lookahead = tok:Peek(1)
					if lookahead.Type == 'Symbol' and lookahead.Data == '=' then
						--we are a key
						local key = tok:Get() 
						if not tok:ConsumeSymbol('=') then
							return false, GenerateError("`=` Expected")
						end
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Expression Expected")
						end
						v.EntryList[#v.EntryList+1] = {
							Type = 'KeyString';
							Key = key.Data;
							Value = value; 
						}

					else
						--we are a value
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Exected")
						end
						v.EntryList[#v.EntryList+1] = {
							Type = 'Value';
							Value = value;
						}

					end
				elseif tok:ConsumeSymbol('}') then
					break

				else
					--value
					local st, value = ParseExpr(scope)
					v.EntryList[#v.EntryList+1] = {
						Type = 'Value';
						Value = value;
					}
					if not st then
						return false, GenerateError("Value Expected")
					end
				end

				if tok:ConsumeSymbol(';') or tok:ConsumeSymbol(',') then
					--all is good
				elseif tok:ConsumeSymbol('}') then
					break
				else
					return false, GenerateError("`}` or table entry Expected")
				end
			end
			return true, v

		elseif tok:ConsumeKeyword('function') then
			local st, func = ParseFunctionArgsAndBody(scope)
			if not st then return false, func end
			--
			func.IsLocal = true
			return true, func

		else
			return ParseSuffixedExpr(scope)
		end
	end


	local unops = lookupify{'-', 'not', '#'}
	local unopprio = 8
	local priority = {
		['+'] = {6,6};
		['-'] = {6,6};
		['%'] = {7,7};
		['/'] = {7,7};
		['*'] = {7,7};
		['^'] = {10,9};
		['..'] = {5,4};
		['=='] = {3,3};
		['<'] = {3,3};
		['<='] = {3,3};
		['~='] = {3,3};
		['>'] = {3,3};
		['>='] = {3,3};
		['and'] = {2,2};
		['or'] = {1,1};
	}
	local function ParseSubExpr(scope, level)
		--base item, possibly with unop prefix
		local st, exp
		if unops[tok:Peek().Data] then
			local op = tok:Get().Data
			st, exp = ParseSubExpr(scope, unopprio)
			if not st then return false, exp end
			local nodeEx = {}
			nodeEx.AstType = 'UnopExpr'
			nodeEx.Rhs = exp
			nodeEx.Op = op
			exp = nodeEx
		else
			st, exp = ParseSimpleExpr(scope)
			if not st then return false, exp end
		end

		--next items in chain
		while true do
			local prio = priority[tok:Peek().Data]
			if prio and prio[1] > level then
				local op = tok:Get().Data
				local st, rhs = ParseSubExpr(scope, prio[2])
				if not st then return false, rhs end
				local nodeEx = {}
				nodeEx.AstType = 'BinopExpr'
				nodeEx.Lhs = exp
				nodeEx.Op = op
				nodeEx.Rhs = rhs
				--
				exp = nodeEx
			else
				break
			end
		end

		return true, exp
	end


	ParseExpr = function(scope)
		return ParseSubExpr(scope, 0)
	end


	local function ParseStatement(scope)
		local stat = nil
		if tok:ConsumeKeyword('if') then
			--setup
			local nodeIfStat = {}
			nodeIfStat.AstType = 'IfStatement'
			nodeIfStat.Clauses = {}

			--clauses
			repeat
				local st, nodeCond = ParseExpr(scope)
				if not st then return false, nodeCond end
				if not tok:ConsumeKeyword('then') then
					return false, GenerateError("`then` expected.")
				end
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
				nodeIfStat.Clauses[#nodeIfStat.Clauses+1] = {
					Condition = nodeCond;
					Body = nodeBody;
				}
			until not tok:ConsumeKeyword('elseif')

			--else clause
			if tok:ConsumeKeyword('else') then
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
				nodeIfStat.Clauses[#nodeIfStat.Clauses+1] = {
					Body = nodeBody;
				}
			end

			--end
			if not tok:ConsumeKeyword('end') then
				return false, GenerateError("`end` expected.")
			end

			stat = nodeIfStat

		elseif tok:ConsumeKeyword('while') then
			--setup
			local nodeWhileStat = {}
			nodeWhileStat.AstType = 'WhileStatement'

			--condition
			local st, nodeCond = ParseExpr(scope)
			if not st then return false, nodeCond end

			--do
			if not tok:ConsumeKeyword('do') then
				return false, GenerateError("`do` expected.")
			end

			--body
			local st, nodeBody = ParseStatementList(scope)
			if not st then return false, nodeBody end

			--end
			if not tok:ConsumeKeyword('end') then
				return false, GenerateError("`end` expected.")
			end

			--return
			nodeWhileStat.Condition = nodeCond
			nodeWhileStat.Body = nodeBody
			stat = nodeWhileStat

		elseif tok:ConsumeKeyword('do') then
			--do block
			local st, nodeBlock = ParseStatementList(scope)
			if not st then return false, nodeBlock end
			if not tok:ConsumeKeyword('end') then
				return false, GenerateError("`end` expected.")
			end

			local nodeDoStat = {}
			nodeDoStat.AstType = 'DoStatement'
			nodeDoStat.Body = nodeBlock
			stat = nodeDoStat

		elseif tok:ConsumeKeyword('for') then
			--for block
			if not tok:Is('Ident') then
				return false, GenerateError("<ident> expected.")
			end
			local baseVarName = tok:Get()
			if tok:ConsumeSymbol('=') then
				--numeric for
				local forScope = CreateScope(scope)
				local forVar = forScope:CreateLocal(baseVarName.Data)
				--
				local st, startEx = ParseExpr(scope)
				if not st then return false, startEx end
				if not tok:ConsumeSymbol(',') then
					return false, GenerateError("`,` Expected")
				end
				local st, endEx = ParseExpr(scope)
				if not st then return false, endEx end
				local st, stepEx;
				if tok:ConsumeSymbol(',') then
					st, stepEx = ParseExpr(scope)
					if not st then return false, stepEx end
				end
				if not tok:ConsumeKeyword('do') then
					return false, GenerateError("`do` expected")
				end
				--
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end') then
					return false, GenerateError("`end` expected")
				end
				--
				local nodeFor = {}
				nodeFor.AstType = 'NumericForStatement'
				nodeFor.Scope = forScope
				nodeFor.Variable = forVar
				nodeFor.Start = startEx
				nodeFor.End = endEx
				nodeFor.Step = stepEx
				nodeFor.Body = body
				stat = nodeFor
			else
				--generic for
				local forScope = CreateScope(scope)
				--
				local varList = {forScope:CreateLocal(baseVarName.Data)}
				while tok:ConsumeSymbol(',') do
					if not tok:Is('Ident') then
						return false, GenerateError("for variable expected.")
					end
					varList[#varList+1] = forScope:CreateLocal(tok:Get().Data)
				end
				if not tok:ConsumeKeyword('in') then
					return false, GenerateError("`in` expected.")
				end
				local generators = {}
				local st, firstGenerator = ParseExpr(scope)
				if not st then return false, firstGenerator end
				generators[#generators+1] = firstGenerator
				while tok:ConsumeSymbol(',') do
					local st, gen = ParseExpr(scope)
					if not st then return false, gen end
					generators[#generators+1] = gen
				end
				if not tok:ConsumeKeyword('do') then
					return false, GenerateError("`do` expected.")
				end
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end') then
					return false, GenerateError("`end` expected.")
				end
				--
				local nodeFor = {}
				nodeFor.AstType = 'GenericForStatement'
				nodeFor.Scope = forScope
				nodeFor.VariableList = varList
				nodeFor.Generators = generators
				nodeFor.Body = body
				stat = nodeFor
			end

		elseif tok:ConsumeKeyword('repeat') then
			local st, body = ParseStatementList(scope)
			if not st then return false, body end
			--
			if not tok:ConsumeKeyword('until') then
				return false, GenerateError("`until` expected.")
			end
			--
			local st, cond = ParseExpr(scope)
			if not st then return false, cond end
			--
			local nodeRepeat = {}
			nodeRepeat.AstType = 'RepeatStatement'
			nodeRepeat.Condition = cond
			nodeRepeat.Body = body
			stat = nodeRepeat

		elseif tok:ConsumeKeyword('function') then
			if not tok:Is('Ident') then
				return false, GenerateError("Function name expected")
			end
			local st, name = ParseSuffixedExpr(scope, true) --true => only dots and colons
			if not st then return false, name end
			--
			local st, func = ParseFunctionArgsAndBody(scope)
			if not st then return false, func end
			--
			func.IsLocal = false
			func.Name = name
			stat = func

		elseif tok:ConsumeKeyword('local') then
			if tok:Is('Ident') then
				local varList = {tok:Get().Data}
				while tok:ConsumeSymbol(',') do
					if not tok:Is('Ident') then
						return false, GenerateError("local var name expected")
					end
					varList[#varList+1] = tok:Get().Data
				end

				local initList = {}
				if tok:ConsumeSymbol('=') then
					repeat
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
						initList[#initList+1] = ex
					until not tok:ConsumeSymbol(',')
				end

				--now patch var list
				--we can't do this before getting the init list, because the init list does not
				--have the locals themselves in scope.
				for i, v in pairs(varList) do
					varList[i] = scope:CreateLocal(v)
				end

				local nodeLocal = {}
				nodeLocal.AstType = 'LocalStatement'
				nodeLocal.LocalList = varList
				nodeLocal.InitList = initList
				--
				stat = nodeLocal

			elseif tok:ConsumeKeyword('function') then
				if not tok:Is('Ident') then
					return false, GenerateError("Function name expected")
				end
				local name = tok:Get().Data
				local localVar = scope:CreateLocal(name)
				--	
				local st, func = ParseFunctionArgsAndBody(scope)
				if not st then return false, func end
				--
				func.Name = localVar
				func.IsLocal = true
				stat = func

			else
				return false, GenerateError("local var or function def expected")
			end

		elseif tok:ConsumeKeyword('return') then
			local exList = {}
			if not tok:IsKeyword('end') then
				local st, firstEx = ParseExpr(scope)
				if st then 
					exList[1] = firstEx
					while tok:ConsumeSymbol(',') do
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
						exList[#exList+1] = ex
					end
				end
			end

			local nodeReturn = {}
			nodeReturn.AstType = 'ReturnStatement'
			nodeReturn.Arguments = exList
			stat = nodeReturn

		elseif tok:ConsumeKeyword('break') then
			local nodeBreak = {}
			nodeBreak.AstType = 'BreakStatement'
			stat = nodeBreak

		elseif tok:ConsumeKeyword('continue') then
			local nodeBreak = {}
			nodeBreak.AstType = 'ContinueStatement'
			stat = nodeBreak

		else
			--statementParseExpr
			local st, suffixed = ParseSuffixedExpr(scope)
			if not st then return false, suffixed end

			--assignment or call?
			if tok:IsSymbol(',') or tok:IsSymbol('=') then
				--check that it was not parenthesized, making it not an lvalue
				if (suffixed.ParenCount or 0) > 0 then
					return false, GenerateError("Can not assign to parenthesized expression, is not an lvalue")
				end

				--more processing needed
				local lhs = {suffixed}
				while tok:ConsumeSymbol(',') do
					local st, lhsPart = ParseSuffixedExpr(scope)
					if not st then return false, lhsPart end
					lhs[#lhs+1] = lhsPart
				end

				--equals
				if not tok:ConsumeSymbol('=') then
					return false, GenerateError("`=` Expected.")
				end

				--rhs
				local rhs = {}
				local st, firstRhs = ParseExpr(scope)
				if not st then return false, firstRhs end
				rhs[1] = firstRhs
				while tok:ConsumeSymbol(',') do
					local st, rhsPart = ParseExpr(scope)
					if not st then return false, rhsPart end
					rhs[#rhs+1] = rhsPart
				end

				--done
				local nodeAssign = {}
				nodeAssign.AstType = 'AssignmentStatement'
				nodeAssign.Lhs = lhs
				nodeAssign.Rhs = rhs
				stat = nodeAssign

			elseif suffixed.AstType == 'CallExpr' or 
						 suffixed.AstType == 'TableCallExpr' or 
						 suffixed.AstType == 'StringCallExpr' 
			then
				--it's a call statement
				local nodeCall = {}
				nodeCall.AstType = 'CallStatement'
				nodeCall.Expression = suffixed
				stat = nodeCall
			else
				return false, GenerateError("Assignment Statement Expected")
			end
		end

		stat.HasSemicolon = tok:ConsumeSymbol(';')
		return true, stat
	end


	local statListCloseKeywords = lookupify{'end', 'else', 'elseif', 'until'}
	ParseStatementList = function(scope)
		local nodeStatlist = {}
		nodeStatlist.Scope = CreateScope(scope)
		nodeStatlist.AstType = 'Statlist'
		--
		local stats = {}
		--
		while not statListCloseKeywords[tok:Peek().Data] and not tok:IsEof() do
			local st, nodeStatement = ParseStatement(nodeStatlist.Scope)
			if not st then return false, nodeStatement end
			stats[#stats+1] = nodeStatement
		end
		--
		nodeStatlist.Body = stats
		return true, nodeStatlist
	end


	local function mainfunc()
		local topScope = CreateScope()
		return ParseStatementList(topScope)
	end

	local st, main = mainfunc()
	--print("Last Token: "..PrintTable(tok:Peek()))
	return st, main
end

--== BEGIN REWRITER ==--

local RewriterOptions = {
	--== IndentCharacter ==--
	-- The character used for indenting
	-- Ignored if code is one-lined
	IndentCharacter = "\t",

	--== UseNewlines ==--
	-- Whether or not the output should be nicely formatted with newlines
	UseNewlines = true,

	--== UseSemicolons ==--
	-- Whether or not to use a semicolon at the end of each expression
	-- Could help fix "ambiguous syntax" issues
	-- Recommended to use with UseNewlines to avoid potential problems
	UseSemicolons = false,

	--== AddExtraSpacing ==--
	-- Adds extra spacing in commas and expressions (E.g. ", " vs ",")
	AddExtraSpacing = true,

	--== ObscureVariableNames ==--
	-- Replaces local variable names with complete garbage. The variable must be localised
	ObscureVariableNames = true,

	--== ObscureNumbers / ObscureStrings / ObscureGlobals ==--
	-- Turns normal expressions into complex ones
	-- ObscureGlobals will only obscure globals normally found in _G (E.g. print or table)
	-- It will not effect script-made globals for the sake of getfenv()
	ObscureNumbers = false, --SOMEWHAT IMPLEMENTED
	ObscureStrings = false, --NOT YET IMPLEMENTED
	ObscureGlobals = false, --NOT YET IMPLEMENTED

	--== UseShortCallExprs ==--
	-- This turns statements like print("Test") into print"Test"
	UseShortCallExprs = false,

	--== AddJunkCode ==--
	-- This adds code that serves no purpose functionally
	AddJunkCode = true,

	--== JunkCodeChance ==--
	-- The chance at each statement that junk code is added if enabled
	JunkCodeChance = 3/3,
}

-- RewriterOptions helper functions
local CommaSplitter = (RewriterOptions.AddExtraSpacing and ", " or ",")
local EqualsSplitter = (RewriterOptions.AddExtraSpacing and " = " or "=")
local function ConsiderSpacingOperator(op)
	if RewriterOptions.AddExtraSpacing then
		return " " .. op .. " "
	else
		return op
	end
end
local function ConsiderSemicolon()
	if RewriterOptions.UseSemicolons then
		return ";"
	else
		return ""
	end
end

local function GenerateRandomString(P1, P2)
	local ValidCharacters = "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	if P2 then --Min, Max
		P1 = P2 - math.random(0, P2-P1)
	elseif P1 then --Exact
		-- Do nothing
	else --No input, use default
		P1 = math.random(20, 25)
	end
	local out = ""
	for i = 1,P1 do
		local max = #ValidCharacters
		if i == 1 then
			max = max - 10
		end
		local choice = math.random(1,max)
		out = out .. string.sub(ValidCharacters,choice,choice)
	end
	return out
end

local function CreateExecutionScope(parent)
	local scope = {Parent=parent, Locals={}}
	function scope:GetLocal(name)
		local my = self.Locals[name]
		if my then return my end

		if self.Parent then
			local par = self.Parent:GetLocal(name)
			if par then return par end
		end

		error("GetLocal fail?? " .. tostring(name))
		return nil
	end
	function scope:MakeLocal(name, newName)
		if RewriterOptions.ObscureVariableNames and not newName then
			self.Locals[name] = GenerateRandomString()
		else
			self.Locals[name] = newName or name
		end
		return self.Locals[name]
	end
	return scope
end

local function CompileWithFormattingData(Lines)
	if RewriterOptions.UseNewlines then
		return table.concat(Lines,"\n")
	else
		return table.concat(Lines," ")
	end
end

local WriteStatList
local function WriteExpression(Expression, Scope)
	if Expression.AstType == "Function" then
		local SubScope = CreateExecutionScope(Scope)
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = SubScope:MakeLocal(Argument.Name)
		end
		if Expression.VarArg then
			NewArguments[#NewArguments+1] = "..."
		end
		local Lines = {"function(" .. table.concat(NewArguments, CommaSplitter) .. ")"}
		local Body = WriteStatList(Expression.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Expression.AstType == "VarExpr" then
		if Expression.Local then
			return Scope:GetLocal(Expression.Name)
		else
			return Expression.Name
		end

	elseif Expression.AstType == "MemberExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		return Base .. Expression.Indexer .. Expression.Ident.Data

	elseif Expression.AstType == "IndexExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		return Base .. "[" .. WriteExpression(Expression.Index, Scope) .. "]"

	elseif Expression.AstType == "CallExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		if Expression.Base.AstType == "Function" then --Special case for anonymous function calling
			Base = "(" .. Base .. ")"
		end
		if RewriterOptions.UseShortCallExprs and #Expression.Arguments == 1 then
			local Arg1 = Expression.Arguments[1]
			if Arg1.AstType == "StringExpr" or Arg1.AstType == "ConstructorExpr" then
				return Base .. WriteExpression(Arg1, Scope)
			end
		end
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = WriteExpression(Argument, Scope)
		end
		return Base .. "(" .. table.concat(NewArguments, CommaSplitter) .. ")"

	elseif Expression.AstType == "StringCallExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		if Expression.Base.AstType == "Function" then --Special case for anonymous function calling
			Base = "(" .. Base .. ")"
		end
		local Argument = WriteExpression(Expression.Arguments[1], Scope)
		if not RewriterOptions.UseShortCallExprs then
			Argument = "(" .. Argument .. ")"
		end
		return Base .. Argument

	elseif Expression.AstType == "TableCallExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		if Expression.Base.AstType == "Function" then --Special case for anonymous function calling
			Base = "(" .. Base .. ")"
		end
		local Argument = WriteExpression(Expression.Arguments[1], Scope)
		if not RewriterOptions.UseShortCallExprs then
			Argument = "(" .. Argument .. ")"
		end
		return Base .. Argument

	elseif Expression.AstType == "NumberExpr" then
		local NumberValue = tonumber(Expression.Value.Data)
		if RewriterOptions.ObscureNumbers and tostring(NumberValue) == Expression.Value.Data then
			local offset = math.random(-10,10)
			local mode = math.random(1,3)
			if mode == 1 then
				return "(" .. tostring(NumberValue-offset) .. "+" .. offset .. ")"
			elseif mode == 2 then
				offset = math.abs(offset)
				return "(" .. tostring(NumberValue-offset) .. "+#\"" .. GenerateRandomString(offset) .. "\")"
			elseif mode == 3 then
				return "(" .. tostring(NumberValue-1) .. "+#{\"" .. GenerateRandomString() .. "\"})"
			end
		else
			return Expression.Value.Data
		end

	elseif Expression.AstType == "StringExpr" then
		return Expression.Value.Data

	elseif Expression.AstType == "NilExpr" then
		return "nil" --woah!

	elseif Expression.AstType == "BooleanExpr" then
		return tostring(Expression.Value)

	elseif Expression.AstType == "DotsExpr" then
		return "..." --so complex!

	elseif Expression.AstType == "ConstructorExpr" then
		local Parts = {}
		for i,Entry in ipairs(Expression.EntryList) do
			if Entry.Type == "Value" then
				Parts[#Parts+1] = WriteExpression(Entry.Value, Scope)
			elseif Entry.Type == "Key" then
				Parts[#Parts+1] = "[" .. WriteExpression(Entry.Key, Scope) .. "]" .. EqualsSplitter .. WriteExpression(Entry.Value, Scope)
			else--if Entry.Type == "Keystring" then
				Parts[#Parts+1] = Entry.Key .. EqualsSplitter .. WriteExpression(Entry.Value, Scope)
			end
		end
		return "{" .. table.concat(Parts, CommaSplitter) .. "}"

	-- Excess brackets on all operators because calculating BODMAS is lame and requires effort and im lazy
	elseif Expression.AstType == "UnopExpr" then
		-- Don't really care about spacing for a unary operator, it makes it less clear if anything
		if Expression.Op == "not" then -- Unless its a bloody not statement
			return "(" .. Expression.Op .. " " .. WriteExpression(Expression.Rhs, Scope) .. ")"
		end
		return "(" .. Expression.Op .. WriteExpression(Expression.Rhs, Scope) .. ")"

	elseif Expression.AstType == "BinopExpr" then
		return "(" .. WriteExpression(Expression.Lhs, Scope) .. ConsiderSpacingOperator(Expression.Op) .. WriteExpression(Expression.Rhs, Scope) .. ")"

	end
	error("We didn't return on a expression!? " .. tostring(Expression))
end

local function WriteStatement(Statement, Scope)
	if Statement.AstType == "Function" then
		local start
		if Statement.IsLocal then
			start = "local function " .. Scope:MakeLocal(Statement.Name.Name)
		else
			start = "function " .. WriteExpression(Statement.Name, Scope)
		end
		local SubScope = CreateExecutionScope(Scope)
		local NewArguments = {}
		if Statement.Name.AstType == "MemberExpr" and Statement.Name.Indexer == ":" then
			NewArguments[1] = SubScope:MakeLocal("self", "self")
		end
		for i,Argument in ipairs(Statement.Arguments) do
			NewArguments[i] = SubScope:MakeLocal(Argument.Name)
		end
		if Statement.VarArg then
			NewArguments[#NewArguments+1] = "..."
		end
		local Lines = {start .. "(" .. table.concat(NewArguments, CommaSplitter) .. ")"}
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "IfStatement" then
		local Lines = {}
		for i,Clause in ipairs(Statement.Clauses) do
			if i == 1 then
				Lines[#Lines+1] = "if " .. WriteExpression(Clause.Condition, Scope) .. " then"
			elseif Clause.Condition then
				Lines[#Lines+1] = "elseif " .. WriteExpression(Clause.Condition, Scope) .. " then"
			else
				Lines[#Lines+1] = "else"
			end
			local SubScope = CreateExecutionScope(Scope)
			local Body = WriteStatList(Clause.Body, SubScope)
			for i = 1,#Body do
				Lines[#Lines+1] = Body[i]
			end
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "WhileStatement" then
		local Lines = {"while " .. WriteExpression(Statement.Condition, Scope) .. " do"}
		local SubScope = CreateExecutionScope(Scope)
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "DoStatement" then
		local Lines = {"do"}
		local SubScope = CreateExecutionScope(Scope)
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "NumericForStatement" then
		local SubScope = CreateExecutionScope(Scope)
		local Variable = SubScope:MakeLocal(Statement.Variable.Name)
		local Start = WriteExpression(Statement.Start, Scope)
		local End = WriteExpression(Statement.End, Scope)
		local Elements = {Start, End, Statement.Step and WriteExpression(Statement.Step, Scope)}
		local Lines = {"for " .. Variable .. EqualsSplitter .. table.concat(Elements, CommaSplitter) .. " do"}
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "GenericForStatement" then
		local SubScope = CreateExecutionScope(Scope)
		local NewVariables = {}
		for i,Variable in ipairs(Statement.VariableList) do
			NewVariables[i] = SubScope:MakeLocal(Variable.Name)
		end
		local NewGenerators = {}
		for i,Generator in ipairs(Statement.Generators) do
			NewGenerators[i] = WriteExpression(Generator, Scope)
		end
		local Lines = {"for " .. table.concat(NewVariables, CommaSplitter) .. " in " .. table.concat(NewGenerators, CommaSplitter) .. " do"}
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "RepeatStatement" then
		local Lines = {"repeat"}
		local SubScope = CreateExecutionScope(Scope)
		local Body = WriteStatList(Statement.Body, SubScope)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "until " .. WriteExpression(Statement.Condition, Scope)
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "LocalStatement" then
		local NewLocals = {}
		for i,Local in ipairs(Statement.LocalList) do
			NewLocals[i] = Scope:MakeLocal(Local.Name)
		end
		local NewValues = {}
		for i,Value in ipairs(Statement.InitList) do
			NewValues[i] = WriteExpression(Value, Scope)
		end
		if #NewValues > 0 then
			return "local " .. table.concat(NewLocals, CommaSplitter) .. EqualsSplitter .. table.concat(NewValues, CommaSplitter)
		else
			return "local " .. table.concat(NewLocals, CommaSplitter)
		end

	elseif Statement.AstType == "ReturnStatement" then
		local NewArguments = {}
		for i,Argument in ipairs(Statement.Arguments) do
			NewArguments[i] = WriteExpression(Argument, Scope)
		end
		return "return " .. table.concat(NewArguments, CommaSplitter)

	elseif Statement.AstType == "BreakStatement" then
		return "break"

	elseif Statement.AstType == "ContinueStatement" then
		return "continue"

	elseif Statement.AstType == "AssignmentStatement" then
		local NewLhs = {}
		for i,Value in ipairs(Statement.Lhs) do
			NewLhs[i] = WriteExpression(Value, Scope)
		end
		local NewRhs = {}
		for i,Value in ipairs(Statement.Rhs) do
			NewRhs[i] = WriteExpression(Value, Scope)
		end
		return table.concat(NewLhs, CommaSplitter) .. EqualsSplitter .. table.concat(NewRhs, CommaSplitter)

	elseif Statement.AstType == "CallStatement" then
		return WriteExpression(Statement.Expression, Scope)

	end
	error("We didn't return on a statement!? " .. tostring(Statement))
end

--[[
We can write junk statements in any formatting style we want
since we use ParseLua to make it into reliable AST data
--]]
local JunkVars = {}
for i = 1,25 do
	JunkVars[i] = GenerateRandomString()
end
local function GetJunkVar()
	return JunkVars[math.random(1,#JunkVars)]
end
local JunkStatements = {
	function()
		local var = GetJunkVar()
		return "if " .. var .. " then " .. var .. "() end"
	end,
	function()
		return "local " .. GetJunkVar()
	end,
	function()
		local arg = GetJunkVar()
		return "local function " .. GetJunkVar() .. "("..arg..",...) return "..arg.."(...) end"
	end,
	function()
		local arg = GetJunkVar()
		local f1 = GetJunkVar()
		local f2 = GetJunkVar()
		return "local function "..GetJunkVar().."("..arg..") local "..arg.."="..arg.." return "..f1.."("..arg..") or "..f2.."("..arg..") end"
	end,
	function()
		return "local " .. GetJunkVar() .. " = " .. math.random(-10,10)
	end,
	function()
		return "local " .. GetJunkVar() .. " = \"\""
	end,
	function()
		return "local " .. GetJunkVar() .. " = {}"
	end,
	function()
		local var = GetJunkVar()
		return "while " .. var .. " do " .. var .. " = " .. var .. "() end"
	end
}
local function GenerateJunkCode()
	local s,r = ParseLua(JunkStatements[math.random(1,#JunkStatements)]())
	return r.Body[1]
end

local function StringSplit(str, splitter)
	local out = {}
	local s,e,f = string.find(str, "(.-)"..splitter)
	if not s then
		return {str}
	end
	while true do
		out[#out+1] = f
		local ns,ne,nf = string.find(str, "(.-)"..splitter, e+1)
		if not ns then
			out[#out+1] = string.sub(str, e+1)
			return out
		else
			s,e,f = ns,ne,nf
		end
	end
end

--cringe string split magic but its needed, trust
WriteStatList = function(StatList, Scope, DontIndent)
	local out = {}
	for _,Statement in ipairs(StatList.Body) do
		if RewriterOptions.AddJunkCode and math.random() < RewriterOptions.JunkCodeChance then
			local JunkText = StringSplit(WriteStatement(GenerateJunkCode(), Scope) .. ConsiderSemicolon(), "\n")
			for i = 1,#JunkText do
				out[#out+1] = JunkText[i]
			end
		end
		local StatementText = StringSplit(WriteStatement(Statement, Scope) .. ConsiderSemicolon(), "\n")
		for i = 1,#StatementText do
			out[#out+1] = StatementText[i]
		end
	end
	if RewriterOptions.UseNewlines and not DontIndent then
		for i = 1,#out do
			out[i] = RewriterOptions.IndentCharacter .. out[i]
		end
	end
	return out
end

print((function(C)
	local s,p = ParseLua(C)
	if not s then
		print("Failed to parse the lua - "..p)
		return false,p
	end

	if not RewriterOptions.UseNewlines and not RewriterOptions.UseSemicolons then
		print("WARNING: Semicolons should really be used when no newlines are present")
	end

	local result = WriteStatList(p, CreateExecutionScope(), true)
	return true, table.concat(result,"\n")
end)([==[
return(function(a)local b,c,d,e=string.sub,string.byte,string.rep,string.char;local f,g=math.floor,math.log;local h,i,j,k,l,m,n,o,p=type,ipairs,select,unpack,getfenv,error,tonumber,tostring,assert;local q,r,s=true,false,nil;local t;local u;local function v(w)local x={P=w,L={}}function x:GL(y)local z=x.L[y]if z then return z end;if x.P then local A=x.P:GL(y)if A then return A end end;return s end;function x:SL(y,B)local C=x:GL(y)if not C then m("Bad SL "..o(y))end;C[16]=B end;function x:ML(y,B)local z={}z[0]=y;z[16]=B;x.L[y]=z;return z end;return x end;local D=l()local E={}local function F(G,...)local H=E[G]or{1,1}local I={...}local J=H[1]local K=j("#",...)if K>1 then for L=J,H[2]do G[L]=s end end;for L=1,K do G[J]=I[L]J=J+1 end;E[G]={H[1]+1,J}end;local function M(G,N)local O=E[G]if O then if not N then E[G]=nil end;return k(G,1,O[2]-1)else return k(G)end end;u=function(P,x,Q)local R=P[7]if R==2 then if Q then return P,q else if P[17]then local S=x:GL(P[0])if not S then m("Expected '"..o(P[0]).."' was missing")else return S[16]end end;return D[P[0]]end elseif R==8 then return n(P[16][29])elseif R==9 then return P[16][29]elseif R==11 then return P[16]elseif R==10 then return s elseif R==15 then local T=P[12]local U=u(P[8],x)if T==14 then return U and u(P[9],x)elseif T==15 then return U or u(P[9],x)end;local V=u(P[9],x)if T==1 then return U+V elseif T==2 then return U-V elseif T==3 then return U%V elseif T==4 then return U/V elseif T==5 then return U*V elseif T==6 then return U^V elseif T==7 then return U..V elseif T==8 then return U==V elseif T==9 then return U<V elseif T==10 then return U<=V elseif T==11 then return U~=V elseif T==12 then return U>V elseif T==13 then return U>=V end elseif R==14 then local V=u(P[9],x)local T=P[12]if T==1 then return-V elseif T==2 then return not V elseif T==3 then return#V end elseif R==12 then return M(x:GL(-1)[16],q)elseif R==5 or R==7 or R==6 then local W={}for X,Y in i(P[3])do if R==6 then W={Y[29]}else F(W,u(Y,x))end end;return u(P[5],x)(M(W))elseif R==4 then if Q then return P,q else return u(P[5],x)[u(P[2],x)]end elseif R==3 then if Q then return P,q else if P[6]==r then return u(P[5],x)[P[4][29]]elseif P[6]==q then local Z=u(P[5],x)local _=Z[P[4][29]]if h(_)=="function"then return function(...)return _(Z,...)end else return _ end end end elseif R==1 then return function(...)local a0=v(x)local a1={...}for L=1,#P[3]do local Y=P[3][L]if Y then a0:ML(Y[0],a1[L])end end;if P[14]then local a2={}for L=#P[3]+1,j("#",...)do F(a2,a1[L])end;a0:ML(-1,a2)end;local a3=t(P[1],a0)if not a3 then return elseif a3.T==1 then return M(a3.D)else local a4=a3.T==2 and"break"or"continue"m("Illegal attempt to "..a4 .." the current scope")end end elseif R==13 then local _={}for X,a5 in i(P[13])do if a5[27]==0 then _[u(a5[28],x)]=u(a5[16],x)elseif a5[27]==1 then _[a5[28]]=u(a5[16],x)end end;for X,a5 in i(P[13])do if a5[27]==2 then F(_,u(a5[16],x))end end;return _ end end;local a6=function(a4,x)local R=a4[7]if R==12 then local _={}for L=1,#a4[9]do F(_,u(a4[9][L],x))end;for L=1,#a4[8]do local U,a7=u(a4[8][L],x,q)local V=_[L]if a7 then if U[7]==2 then if U[17]then x:SL(U[0],V)else D[U[0]]=V end elseif U[7]==3 then local Z=u(U[5],x)Z[U[4][29]]=V else local Z=u(U[5],x)Z[u(U[2],x)]=V end end end elseif R==13 then u(a4[21],x)elseif R==8 then local _={}for L=1,#a4[15]do F(_,u(a4[15][L],x))end;for L=1,#a4[18]do local C=a4[18][L]x:ML(C[0],_[L])end elseif R==2 then for X,a8 in i(a4[11])do if not a8[10]or u(a8[10],x)then return t(a8[1],v(x))end end elseif R==3 then while u(a4[10],x)do local a3=t(a4[1],v(x))if a3 then if a3.T==2 then return elseif a3.T==1 then return a3 end end end elseif R==4 then return t(a4[1],v(x))elseif R==9 then local a9={}for X,Y in i(a4[3])do F(a9,u(Y,x))end;return a9 elseif R==10 then return q elseif R==11 then return r elseif R==7 then repeat local a3=t(a4[1],v(x))if a3 then if a3.T==2 then return elseif a3.T==1 then return a3 end end until u(a4[10],x)elseif R==1 then local y=a4[0]if y[7]==3 then local Z=u(y[5],x)if y[6]==r then local aa=u(a4,x)Z[y[4][29]]=aa elseif y[6]==q then local aa=u(a4,x,q)Z[y[4][29]]=aa end else local aa=u(a4,x)if a4[22]then x:ML(y[0],aa)else D[y[0]]=aa end end elseif R==6 then local ab,ac,ad;local ae=a4[19]if not ae[2]then ab,ac,ad=u(ae[1],x)else ab=u(ae[1],x)ac=u(ae[2],x)if ae[3]then ad=u(ae[3],x)end end;while q do local a0=v(x)local W={ab(ac,ad)}ad=W[1]if ad==s then break end;for L=1,#W do a0:ML(a4[20][L][0],W[L])end;local a3=t(a4[1],a0)if a3 then if a3.T==2 then return elseif a3.T==1 then return a3 end end end elseif R==5 then local af=n(u(a4[23],x))local ag=n(u(a4[24],x))local ah=a4[25]and n(u(a4[25],x))or 1;while ah>0 and af<=ag or ah<=0 and af>=ag do local a0=v(x)a0:ML(a4[26][0],af)local a3=t(a4[1],a0)if a3 then if a3.T==2 then return elseif a3.T==1 then return a3 end end;af=af+ah end end end;t=function(ai,x)for X,aj in i(ai[1])do local _=a6(aj,x)if h(_)=="table"then if not _.P then return{P=q,T=1,D=_}else return _ end elseif h(_)=="boolean"then return{P=q,T=_==q and 2 or 3}end end end;return function()a=(function(ak)local function al(am,an,ao)return d(ao,an-#am)..am end;local function ap(aq)return n(aq,2)end;local function ar(an,as)p(f(an)==an)if an==0 then return al("0",as or 1,"0")end;local at=f(g(an,2))local au=""while at>=0 do if an>=2^at then an=an-2^at;au=au.."1"else au=au.."0"end;at=at-1 end;return al(au,as or 1,"0")end;local function av(aw)local at=0;local ax=0;for ay=1,#aw do local az=b(aw,ay,ay)if az=="1"then ax=ax+2^at end;at=at-1 end;return ax end;local aA=1;local aB=""local function aC(aD)for L=1,f((aD-#aB-1)/6)+1 do aB=aB..b(ar(c(ak,aA,aA),8),3,-1)aA=aA+1 end end;local function aE(aD)aC(aD)local aF=b(aB,1,aD)aB=b(aB,aD+1)return aF end;local function aG(aD)return ap(aE(aD))end;local function aH()return e(aG(8))end;local function aI()local aJ,aK,aL=aG(1),aG(11),aE(52)aJ,aK=aJ==0 and 1 or-1,2^(aK-1023)return aJ*aK*av("1"..aL)end;local aM=0;local aN=1;local aO=2;local aP=3;local aQ=4;local aR=5;local aS=6;local aT=7;local aU=3;local function aV(aW)if not aW then p(aG(aU)==aM,"Invalid SD")end;local aX={}local aY=s;local function aZ(a_)if aY then aX[aY]=a_;aY=s else aY=a_ end end;while q do local b0=aG(aU)if b0==aN then return aX elseif b0==aM then aZ(aV(q))elseif b0==aO then local b1=""while q do local b2=aH()if b2=="\0"then aZ(b1)break elseif b2=="\\"then b1=b1 ..aH()else b1=b1 ..b2 end end elseif b0==aP then aZ(aI())elseif b0==aQ then aZ(aG(1)==1)elseif b0==aS then aZ(aG(3))elseif b0==aR then aZ(aG(5))elseif b0==aT then aZ(aG(8))end end end;return aV()end)(a)local a3=t(a,v())if not a3 then return elseif a3.T==1 then return M(a3.D)else local a4=a3.T==2 and"break"or"continue"m("Illegal attempt to "..a4 .." the current scope")end end end)([=[]=])()
]==]))

return function(C)
	local s,p = ParseLua(C)
	if not s then
		print("Failed to parse the lua - "..p)
		return false,p
	end

	if not RewriterOptions.UseNewlines and not RewriterOptions.UseSemicolons then
		print("WARNING: Semicolons should really be used when no newlines are present")
	end

	local result = WriteStatList(p, CreateExecutionScope(), true)
	return true, table.concat(result,"\n")
end
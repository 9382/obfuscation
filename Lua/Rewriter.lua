
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

--[[ Control Flow Flattener
Flattens the control into a straight-line format, with each statement being split up

Steps:
1) Collect locals and define them at the top of the control flow
2) Turn each statement into a seperate number index for flattening

Some special rules to consider:
statements that end or direct the flow will need special logic, E.g.
A continue statement must go to the start of the loop
A break statement must go to the first statement after the loop
A return statement may not need rewriting?

Notes/warnings about implementation:
I consider this to be a "weak flattening" implementation
By this, I mean that if statements are treated like functions instead of being flattened into the current control flow
I intend to possibly make a strong flattening version, but only once weak is made so I have a point I can return to if I screw up
Note that defining a local twice (local x; local x) can cause some unexpected behaviour and may produce wrong code

This entire process of "control flattening" is very difficult to do in lua, considering scopes are very closed
Most languages bring in new scopes per function, but we get new scopes per any sort of Body entry (Like an If statement)
This forces us to do some quite ugly and unreliable self-management
--]]
local function FlattenControlFlow(ast)
	local function PerformFlattening(Body)
		local NewAST = {}
		--Step 1: Variable collection
		local Variables = {}
		local InstructionPointer = "__ins" .. math.random(1e4,1e5-1) .. "__"
		local InstructionExpression = {AstType="VarExpr", Name=InstructionPointer, Local={CanRename=true, Scope=Body.Scope, Name=InstructionPointer}}
		Variables[1] = InstructionPointer
		for _,Statement in ipairs(Body) do
			if Statement.AstType == "LocalStatement" then
				for _,Name in next,Statement.LocalList do
					Variables[#Variables+1] = Name.Name
				end
			elseif Statement.AstType == "Function" then
				if Statement.IsLocal then
					Variables[#Variables+1] = Statement.Name.Name
				end
			end
		end
		for i,Variable in next,Variables do
			Variables[i] = {CanRename=true, Scope=Body.Scope, Name=Variable}
		end
		NewAST[1] = {
			AstType = "LocalStatement",
			LocalList = Variables,
			InitList = {{AstType="NumberExpr", Value={Data="1"}}},
		}

		--Step 2: Compile the statements
		local CollectedStatements = {}
		local function CreateInstructionCheck(index)
			return {
				AstType="BinopExpr", Op="==",
				Rhs={AstType="NumberExpr", Value={Data=tostring(index)}},
				Lhs=InstructionExpression,
			}
		end
		local function StandardProcedure(Statement, index)
			return {
				Body = {AstType="Statlist", Scope=Body.Scope, Body={
					Statement,
					{AstType="AssignmentStatement", Lhs={InstructionExpression}, Rhs={{AstType="NumberExpr", Value={Data=tostring(index+1)}}}}
				}},
				Condition = CreateInstructionCheck(index)
			}
		end
		for _,Statement in ipairs(Body) do
			local index = #CollectedStatements+1

			--Special ones
			if Statement.AstType == "LocalStatement" then
				Statement.AstType = "AssignmentStatement"
				Statement.Lhs = Statement.LocalList
				for i,Lhs in ipairs(Statement.Lhs) do
					Statement.Lhs[i] = {AstType="VarExpr", Name=Lhs.Name, Local={CanRename=true, Scope=Body.Scope, Name=Lhs.Name}}
				end
				Statement.Rhs = Statement.InitList
				if #Statement.Rhs == 0 then
					Statement.Rhs = {{AstType="NilExpr"}}
				end
				Statement.InitList = nil
				Statement.LocalList = nil
				CollectedStatements[index] = StandardProcedure(Statement, index)

			elseif Statement.AstType == "Function" and Statement.IsLocal then
				Statement.IsLocal = false
				local FuncName = Statement.Name.Name
				Statement.Body.Body = PerformFlattening(Statement.Body.Body)
				CollectedStatements[index] = StandardProcedure({
					AstType = "AssignmentStatement",
					Lhs = {{AstType="VarExpr", Name=FuncName, Local={CanRename=true, Scope=Body.Scope, name=FuncName}}},
					Rhs = {Statement},
				}, index)

			elseif Statement.AstType == "ReturnStatement" then
				local out = StandardProcedure(Statement, index)
				out.Body.Body[2] = nil
				CollectedStatements[index] = out

			elseif Statement.AstType == "IfStatement" then --THIS IS WEAK
				--If this was strong, each condition would set __ins__ to a reserved ID section that had the if's code
				for i,Clause in ipairs(Statement.Clauses) do
					Clause.Body.Body = PerformFlattening(Clause.Body.Body)
					-- Clause.Body.Body[#Clause.Body.Body+1] = {AstType="AssignmentStatement", Lhs={InstructionExpression}, Rhs={{AstType="NumberExpr", Value={Data=tostring(index+1)}}}}
				end
				local out = StandardProcedure(Statement, index)
				-- out.Body.Body[2] = nil
				CollectedStatements[index] = out

			elseif Statement.AstType == "WhileStatement" then --THIS IS WEAK
				--If this was strong, the condition would be implemented like a statement and flow would split from there
				Statement.Body.Body = PerformFlattening(Statement.Body.Body)
				CollectedStatements[index] = StandardProcedure(Statement, index)

			elseif Statement.AstType == "RepeatStatement" then --THIS IS WEAK
				--If this was strong, the condition would be implemented like a statement and flow would split from there
				Statement.Body.Body = PerformFlattening(Statement.Body.Body)
				CollectedStatements[index] = StandardProcedure(Statement, index)

			elseif Statement.AstType == "DoStatement" then --THIS IS WEAK
				--If this was strong, it would just ignore the do (note that we would need advanced handling of locals to flatten safely! especially considering do is used for local reasons)
				local out = StandardProcedure(false, index)
				Statement = PerformFlattening(Statement.Body.Body)
				local outBody = out.Body.Body
				outBody[3] = outBody[2]
				outBody[1], outBody[2] = Statement[1], Statement[2]
				CollectedStatements[index] = out

			elseif Statement.AstType == "CallStatement" then
				local Base = Statement.Expression.Base
				if Base.AstType == "Function" then
					Base.Body.Body = PerformFlattening(Base.Body.Body)
				end
				CollectedStatements[index] = StandardProcedure(Statement, index)

			elseif Statement.AstType == "NumericForStatement" or Statement.AstType == "GenericForStatement" then --THIS IS WEAK
				Statement.Body.Body = PerformFlattening(Statement.Body.Body)
				CollectedStatements[index] = StandardProcedure(Statement, index)

			--Else, normal stuff
			else
				CollectedStatements[index] = StandardProcedure(Statement, index)
			end
		end

		--Step 3: Shuffle the statement order for obscurity
		--TODO: Actually change instruction numbers too for extra obscurity
		local ToShuffle = {}
		for a,b in next,CollectedStatements do
			ToShuffle[a] = b
		end
		CollectedStatements={}
		while #ToShuffle>0 do
			CollectedStatements[#CollectedStatements+1] = table.remove(ToShuffle, math.random(1, #ToShuffle))
		end

		--Wrap it up
		NewAST[2] = {
			AstType = "WhileStatement",
			Condition = {
				AstType = "BinopExpr",
				Op = "<=",
				Rhs = {AstType="NumberExpr", Value={Data=tostring(#Body)}},
				Lhs = InstructionExpression,
			},
			Body = {
				AstType = "Statlist",
				Scope = Body.Scope,
				Body = {{
					AstType = "IfStatement",
					Clauses = CollectedStatements
				}}
			}
		}

		--Done!
		return NewAST
	end
	ast.Body = PerformFlattening(ast.Body)
end

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
	-- Recommended to use if UseNewlines is off to avoid potential problems
	UseSemicolons = false,

	--== AddExtraSpacing ==--
	-- Adds extra spacing in commas and expressions (E.g. ", " vs ",")
	AddExtraSpacing = true,

	--== ObscureVariableNames ==--
	-- Replaces local variable names with complete garbage. The variable must be localised
	ObscureVariableNames = false,

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
	AddJunkCode = false,

	--== JunkCodeChance ==--
	-- The chance at each statement that junk code is added if enabled
	JunkCodeChance = 3/3,

	--== PerformCodeFlattening ==--
	-- Performs code flattening to help obscure the normal flow of the function
	-- Note: Current flattening is weak and potentially unreliable, use with caution
	PerformCodeFlattening = false,
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
	Expression.ParenCount = Expression.ParenCount or 0
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
		if Expression.Base.AstType == "Function" or Expression.Base.AstType == "NilExpr" then --Special case for anonymous function calling
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
		if Expression.ParenCount > 0 then
			return "(" .. Base .. "(" .. table.concat(NewArguments, CommaSplitter) .. "))" --subtle truncation
		else
			return Base .. "(" .. table.concat(NewArguments, CommaSplitter) .. ")"
		end

	elseif Expression.AstType == "StringCallExpr" or Expression.AstType == "TableCallExpr" then
		local Base = WriteExpression(Expression.Base, Scope)
		if Expression.Base.AstType == "Function" then --Special case for anonymous function calling
			Base = "(" .. Base .. ")"
		end
		local Argument
		if Expression.AstType == "StringCallExpr" then
			Argument = Expression.Arguments[1].Data
		else
			Argument = WriteExpression(Expression.Arguments[1], Scope)
		end
		if not RewriterOptions.UseShortCallExprs then
			Argument = "(" .. Argument .. ")"
		end
		if Expression.ParenCount > 0 then
			return "(" .. Base .. Argument .. ")"
		else
			return Base .. Argument
		end

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
		if Expression.ParenCount > 0 then
			return "(...)" --subtle truncation
		else
			return "..." --so complex!
		end

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
	error("We didn't return on a expression!? " .. tostring(Expression) .. " " .. (type(Expression)=="table" and tostring(Expression.AstType) or "<no AST>"))
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
		if #Statement.Arguments == 0 then
			return "return"
		end
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

	if RewriterOptions.PerformCodeFlattening then
		FlattenControlFlow(p)
	end

	local result = WriteStatList(p, CreateExecutionScope(), true)
	return true, table.concat(result,"\n")
end)([==[
print("Test")
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

-- Due to complications and laziness, this file does not use the modular LuaParser
-- To resolve this, fix the difference in string processing

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

local BackslashEscaping = {
	a="\a", b="\b", f="\f", n="\n", r="\r", t="\t", v="\v",
	["\\"]="\\", ['"']='"', ["'"]="'", ["["]="[", ["]"]="]"
}

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
				local content = ""
				local contentStart = p
				while true do
					local c = get()
					if c == '\\' then
						local next = get()
						local replacement = BackslashEscaping[next]
						if replacement then
							content = content .. replacement
						else
							if next == "x" then
								local n1 = get()
								if n1 == "" or n1 == delim or not HexDigits[n1] then
									generateError("invalid escape sequence near '"..delim.."'")
								end
								local n2 = get()
								if n2 == "" or n2 == delim or not HexDigits[n2] then
									generateError("invalid escape sequence near '"..delim.."'")
								end
								content = content .. string.char(tonumber(n1 .. n2, 16))
							elseif Digits[next] then
								local num = next
								while #num < 3 and Digits[peek()] do
									num = num .. get()
								end
								content = content .. string.char(tonumber(num))
							else
								generateError("invalid escape sequence near '"..delim.."'")
							end
						end
					elseif c == delim then
						break
					elseif c == '' then
						generateError("Unfinished string near <eof>")
					else
						content = content .. c
					end
				end
				toEmit = {Type = 'String', Data = delim .. content .. delim, Constant = content}

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
	local function CreateScope(parent, canBreak)
		local scope = {}
		if canBreak ~= nil then
			scope.CanBreak = canBreak
		else
			scope.CanBreak = parent.CanBreak
		end
		scope.Parent = parent
		scope.LocalList = {}
		scope.LocalMap = {}
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
		local funcScope = CreateScope(scope, false)
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
			local st, nodeBody = ParseStatementList(scope, true)
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
				local forScope = CreateScope(scope, true)
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
				local forScope = CreateScope(scope, true)
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
			local st, body = ParseStatementList(scope, true)
			if not st then return false, body end
			--
			if not tok:ConsumeKeyword('until') then
				return false, GenerateError("`until` expected.")
			end
			--
			local st, cond = ParseExpr(body.Scope) --use the scope of the body
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
			if not scope.CanBreak then
				return false, GenerateError("Can not break a non-looping statement")
			end
			local nodeBreak = {}
			nodeBreak.AstType = 'BreakStatement'
			stat = nodeBreak

		elseif tok:ConsumeKeyword('continue') then
			if not scope.CanBreak then
				return false, GenerateError("Can not continue a non-looping statement")
			end
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
	ParseStatementList = function(scope, canBreak)
		local nodeStatlist = {}
		nodeStatlist.Scope = CreateScope(scope, canBreak)
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
		local topScope = CreateScope(nil, false)
		return ParseStatementList(topScope)
	end

	local st, main = mainfunc()
	--print("Last Token: "..PrintTable(tok:Peek()))
	return st, main
end

local bitmanager = (function()
	local log2 = math.log(2)
	local function padleft(s,n,p)
		return string.rep(p,n-#s)..s
	end
	local function padright(s,n,p)
		return s..string.rep(p,n-#s)
	end
	local function ToNum(b) --Easy shorthand
		return tonumber(b,2)
	end
	local function ToBit(n,pad)
		assert(n%1 == 0,"Can't convert a non-int to regular bit format")
		if n == 0 then
			if pad then
				return padleft("0",pad,"0")
			else
				return "0"
			end
		end
		local pow = math.floor(math.log(n)/log2)
		local final = ""
		while true do
			if n >= 2^pow then
				n = n - 2^pow
				final = final .. "1"
			else
				final = final .. "0"
			end
			pow = pow - 1
			if pow < 0 then
				if pad then
					return padleft(final,pad,"0")
				else
					return final
				end
			end
		end
	end
	local function DecToBit(d,pad)
		assert(math.abs(d) < 1,"Can't convert non-zero integral decimals to decimal bit")
		assert(type(pad)=="number","DecToBit requires a valid pad length")
		local result = ""
		local iterations = 0
		while true do
			local NextNum = d * 2
			if NextNum > 1 then
				result = result .. "1"
			elseif NextNum == 1 then
				return padright(result .. "1",pad,"0")
			else
				result = result .. "0"
			end
			d = NextNum - math.floor(NextNum)
			iterations = iterations + 1
			if iterations >= pad then
				return result
			end
		end
	end
	local function NormalizeScientific(bits)
		local raw = string.gsub(bits,"%.","")
		local NotationOffset = string.find(raw,"1")
		local Normalized = string.sub(raw,NotationOffset,NotationOffset).."."..string.sub(raw,NotationOffset+1)
		local Exponent = string.find(bits,"%.")-2-(NotationOffset-1)
		return Normalized,Exponent
	end

	local BaseBitWriter = {Data=""}
	function BaseBitWriter:Write(digit,strictWidth)
		local BitRepresentation = ToBit(digit,strictWidth)
		if strictWidth then
			assert(#BitRepresentation <= strictWidth,"Digit width passed provided limit of "..strictWidth)
		end
		self.Data = self.Data .. BitRepresentation
	end
	function BaseBitWriter:WriteString(str)
		for _,Character in next,{string.byte(str,1,-1)} do
			self.Data = self.Data .. ToBit(Character,8)
		end
	end

	function BaseBitWriter:WriteDouble(double)
		if double == 0 then
			self.Data = self.Data .. string.rep("0",64)
			return
		end
		local sign = (double < 0 and "1") or "0"
		double = math.abs(double)
		local integral,fractional = math.modf(double)
		local RequiredBuffer = 0
		if fractional ~= 0 then
			RequiredBuffer = math.max(math.floor(math.log(1/fractional)/log2),0)
		end
		--Buffer is required should the default 53 bits not be enough data due to a large shift when normalizing the scientific.
		--AKA: If 1 does not appear as early as 0.1[...] then normalization fails due to lack of data - that bad, so generate more
		local IntegralBits,FractionalBits = ToBit(integral),DecToBit(fractional,53+RequiredBuffer)
		local NormalizedBits,Exponent = NormalizeScientific(IntegralBits.."."..FractionalBits)
		NormalizedBits = string.sub(NormalizedBits,3,54)
		if #NormalizedBits~=52 then
			print("[bitmanager] Precision lost during handling of double, missing",52-#NormalizedBits,"bits\nFractional:",fractional)
			NormalizedBits = padright(NormalizedBits,52,"0")
		end
		Exponent = ToBit(Exponent+1023,11)
		self.Data = self.Data .. sign .. Exponent .. NormalizedBits
	end
	function BaseBitWriter:ToString()
		local final = ""
		local Data = self.Data
		local DataScanner = 1
		while true do
			local NextByte = "01" .. string.sub(Data,DataScanner,DataScanner+5)
			if #NextByte < 8 then
				final = final .. string.char(ToNum(padright(NextByte,8,"0")))
				break
			else
				final = final .. string.char(ToNum(NextByte))
				DataScanner = DataScanner + 6
			end
		end
		return final
	end

	local function L1Copy(t,b)
		local b = b or {}
		for x,y in next,t do
			b[x] = y
		end
		return b
	end
	local function NewBitWriter(PresetData)
		local BitWriter = L1Copy(BaseBitWriter,{Data=PresetData or ""})
		return BitWriter
	end

	return NewBitWriter
end)()

local serializer = (function()
	local bitmanager = bitmanager

	local TYPE_TABLE_START=0
	local TYPE_TABLE_END=1
	local TYPE_STRING=2
	local TYPE_NUMBER=3
	local TYPE_BOOLEAN=4
	local TYPE_NUMBER_BASIC=5
	local TYPE_NUMBER_SUPERBASIC=6
	local TYPE_NUMBER_SIMPLE=7

	local TYPE_WIDTH=3

	local function Serialize(t,raw)
		local Output = bitmanager()
		Output:Write(TYPE_TABLE_START,TYPE_WIDTH)
		local function HandleType(obj)
			if type(obj) == "table" then
				Output.Data = Output.Data .. Serialize(obj,true)
			elseif type(obj) == "string" then
				Output:Write(TYPE_STRING,TYPE_WIDTH)
				obj = string.gsub(obj,"\\","\\\\")
				obj = string.gsub(obj,"%z","\\\0") --Escape non-terminators
				Output:WriteString(obj)
				Output:Write(0,8) --Null terminator
			elseif type(obj) == "number" then
				if obj%1 == 0 and obj >= 0 and obj < 256 then
					if obj < 8 then
						Output:Write(TYPE_NUMBER_SUPERBASIC,TYPE_WIDTH)
						Output:Write(obj,3)
					elseif obj < 32 then
						Output:Write(TYPE_NUMBER_BASIC,TYPE_WIDTH)
						Output:Write(obj,5)
					else
						Output:Write(TYPE_NUMBER_SIMPLE,TYPE_WIDTH)
						Output:Write(obj,8)
					end
				else
					Output:Write(TYPE_NUMBER,TYPE_WIDTH)
					Output:WriteDouble(obj)
				end
			elseif type(obj) == "boolean" then
				Output:Write(TYPE_BOOLEAN,TYPE_WIDTH)
				Output:Write((obj==true and 1) or 0) --Simple enough
			elseif type(obj) == "function" then
				error("Serializing a function? Yeah no, lets not")
			else
				error("Object of type "..type(obj).." can't be processed by the serializer")
			end
		end
		for a,b in next,t do
			HandleType(a)
			HandleType(b)
		end
		Output:Write(TYPE_TABLE_END,TYPE_WIDTH)
		if raw then
			return Output.Data
		else
			return Output:ToString()
		end
	end

	return Serialize
end)()

local ExpressionSet = {
	--[["Function", ]]"VarExpr", "MemberExpr", "IndexExpr", "CallExpr", "StringCallExpr",
	"TableCallExpr", "NumberExpr", "StringExpr", "NilExpr", "BooleanExpr",
	"DotsExpr", "ConstructorExpr", "UnopExpr", "BinopExpr",
}
local StatementSet = {
	--[["Function", ]]"IfStatement", "WhileStatement", "DoStatement", "NumericForStatement",
	"GenericForStatement", "RepeatStatement", "LocalStatement", "ReturnStatement",
	"BreakStatement", "ContinueStatement", "AssignmentStatement", "CallStatement",
}

local AstTypeToID = {
	Statlist=nil, Function=1,

	--[[Function=1, ]]VarExpr=2, MemberExpr=3, CallExpr=4, NumberExpr=5, StringExpr=6,
	NilExpr=7, BooleanExpr=8, DotsExpr=9, ConstructorExpr=10, UnopExpr=11, BinopExpr=12,

	--[[Function=1, ]]IfStatement=2, WhileStatement=3, DoStatement=4, NumericForStatement=5,
	GenericForStatement=6, RepeatStatement=7, LocalStatement=8, ReturnStatement=9,
	BreakStatement=10, ContinueStatement=11, AssignmentStatement=12, CallStatement=13,
}
--We can actually duplicate the IDs for statements vs expressions
--Since we always know when one leads to the other, meaning theres no concern there
--We don't even need an ID for a Statlist, so lets just nil it!

local BinOpToID = {
	["+"]=1, ["-"]=2, ["%"]=3, ["/"]=4, ["*"]=5, ["^"]=6, [".."]=7, ["=="]=8,
	["<"]=9, ["<="]=10, ["~="]=11, [">"]=12, [">="]=13, ["and"]=14, ["or"]=15
}
local UnOpToID = {
	["-"]=1, ["not"]=2, ["#"]=3
}

local function AssignKey(t,k,n)
	if t[k] ~= nil then
		if t[n] ~= nil then
			print("[WARNING] Overriding",n,"from",t[n],"to",t[k],"despite not expecting to")
		end
		t[n] = t[k]
		t[k] = nil
	end
end
local uniqueLocals = {self=0}
local nextUniqueLocal = 1 --ID 0 is reserved for the local "self", which has to be manually inserted by the executor in some situations, so begin from 1.
local function GetUniqueLocal(l)
	local n = uniqueLocals[l]
	if not n then
		uniqueLocals[l] = nextUniqueLocal
		nextUniqueLocal = nextUniqueLocal + 1
		return nextUniqueLocal - 1
	else
		return n
	end
end
local checked = {}
local function deepModify(t, firstCall)
	if firstCall then
		uniqueLocals = {self=0}
		nextUniqueLocal = 1
		checked = {}
	end
	--Remove irrelevant data
	t.Scope = nil
	t.Char = nil
	t.Position = nil
	t.Line = nil
	t.CanRename = nil
	t.Print = nil
	t.LeadingWhite = nil
	local ParenCount = t.ParenCount or 0
	t.ParenCount = nil
	t.HasSemicolon = nil

	--Merge similar expressions that have identical functionality
	if t.AstType == "TableCallExpr" then
		t.AstType = "CallExpr" --trivial conversion
	elseif t.AstType == "StringCallExpr" then
		t.AstType = "CallExpr"
		t.Arguments[1] = {AstType="StringExpr", Value=t.Arguments[1]}
	end
	if t.AstType == "IndexExpr" then
		t.AstType = "MemberExpr"
		t.Indexer = "."
		t.Ident = t.Index
		t.Index = nil
	elseif t.AstType == "MemberExpr" and t.Indexer == "." then
		--this is butchering the normal structure a bit since we dont expect an expression but whatever
		t.Ident = {AstType="StringExpr", Value={Data=t.Ident.Data}} --MORE STUPID DATA STUFF WHY DO WE HAVE THIS?
	end

	--Fix table:func() assignment issues before runtime
	if t.AstType == "Function" and t.Name and t.Name.Indexer == ":" then
		--Make room for a "self" arg
		for i = #t.Arguments,1,-1 do
			t.Arguments[i+1] = t.Arguments[i]
		end
		t.Arguments[1] = {Name="self"}
	end

	--Optimise names of locals to be numerical rather than strings
	if t.AstType == "LocalStatement" then --Defining locals
		for _,Local in next,t.LocalList do
			if type(Local.Name) ~= "number" then
				Local.Name = GetUniqueLocal(Local.Name)
			end
		end
	elseif t.AstType == "Function" then --function(locals)
		for _,Local in next,t.Arguments do
			if type(Local.Name) ~= "number" then
				Local.Name = GetUniqueLocal(Local.Name)
			end
		end
	elseif t.AstType == "NumericForStatement" then --for local in whatever do
		if type(t.Variable.Name) ~= "number" then
			t.Variable.Name = GetUniqueLocal(t.Variable.Name)
		end
	elseif t.AstType == "GenericForStatement" then --for locals in whatever do
		for _,Local in next,t.VariableList do
			if type(Local.Name) ~= "number" then
				Local.Name = GetUniqueLocal(Local.Name)
			end
		end
	end
	--subtle truncation via parenthesis
	if ({CallExpr=1, StringCallExpr=1, TableCallExpr=1})[t.AstType] then
		t[3] = ParenCount > 0
	elseif t.AstType == "DotsExpr" then
		t[0] = ParenCount > 0
	end
	if t.IsLocal and t.Name then --Functions
		--Somehow ParseSimpleExpr can generate a nameless but local function. /shrug
		if type(t.Name.Name) ~= "number" then
			t.Name.Name = GetUniqueLocal(t.Name.Name)
		end
	end
	if t.Local then --VarExpr
		if type(t.Name) ~= "number" then
			t.Name = GetUniqueLocal(t.Name)
		end
	end

	--Simplify values
	local HasAstType = type(t.AstType) == "string"
	if t.Local then
		t.Local = true
	end
	if t.Op then
		if t.AstType == "BinopExpr" then
			t.Op = BinOpToID[t.Op]
		elseif t.AstType == "UnopExpr" then
			t.Op = UnOpToID[t.Op]
		end
	end
	if t.Indexer then
		t[2] = (t.Indexer == ":" and true or false)
		t.Indexer = nil
	end
	if t.AstType then
		t[5] = AstTypeToID[t.AstType] or math.random(1,15) --If it doesnt matter, just have fun
		t.AstType = nil
	end
	if t.Type then
		local v = t.Type
		t.Type = nil
		if v == "Key" then
			t[2] = 0
		elseif v == "KeyString" then
			t[2] = 1
		elseif v == "Value" then
			t[2] = 2
		end
	end

	--Numerical naming (it's nicer on the serializer's size)
	--Everything is applied in context, so a lot of keys here copy each other (See the relevant theory txt)
	AssignKey(t,"Name",0)
	AssignKey(t,"Body",1)
	AssignKey(t,"Index",1)
	AssignKey(t,"Arguments",2)
	AssignKey(t,"Ident",1)
	AssignKey(t,"Base",0) --old 5
	--Indexer = 2
	--AstType = 5
	AssignKey(t,"Lhs",1)
	AssignKey(t,"Rhs",0)
	AssignKey(t,"Condition",0) --old 10
	AssignKey(t,"Clauses",0)
	AssignKey(t,"Op",2)
	AssignKey(t,"EntryList",1)
	AssignKey(t,"VarArg",3)
	AssignKey(t,"InitList",0) --old 15
	AssignKey(t,"Value",1)
	AssignKey(t,"Local",2)
	AssignKey(t,"LocalList",1)
	AssignKey(t,"Generators",0)
	AssignKey(t,"VariableList",2) --old 20
	AssignKey(t,"Expression",0)
	AssignKey(t,"IsLocal",4)
	AssignKey(t,"Start",0)
	AssignKey(t,"End",2)
	AssignKey(t,"Step",3) --old 25
	AssignKey(t,"Variable",4)
	--Type = 2
	AssignKey(t,"Key",3)
	if t.Constant ~= nil then --Override string form containing surrounding quotes with the constant
		AssignKey(t,"Constant",0)
		t.Data = nil
	else
		AssignKey(t,"Data",0)
	end

	--Fake data
	if math.random(1,12) == 1 and HasAstType then
		--Do not add fake data if no AstType is present, as this could screw a Pairs check
		for i = math.random(0,1),5 do
			if t[i] == nil then
				t[i] = false
				break
			end
		end
	end

	--Check subtables
	for a,b in next,t do
		if type(b) == "table" and not checked[b] then
			checked[b] = true
			if b.AstType == "Statlist" then
				deepModify(b)
				t[a] = b[1]
			else
				deepModify(b)
			end
		end
	end
end

--[[ quick in-script way of testing
print((function(C)
  local s,p = ParseLua(C)
	if not s then
		print("Failed to parse the lua - "..p)
		return false,p
	end

	-- print(PrintTable(p))
	deepModify(p.Body, true)
	-- print(PrintTable(p.Body))
	return true, serializer(p.Body)
end)([====[
]====]))
--]]

return function(C)
	local s,p = ParseLua(C)
	if not s then
		print("Failed to parse the lua - "..p)
		return false,p
	end

	deepModify(p.Body, true)
	-- print(PrintTable(p.Body))
	return true, serializer(p.Body)
end
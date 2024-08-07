-- This script is the executor, which executes lua code turned into a serialized ("obfuscated") string by Serializer.lua
-- ExecutorMinified.lua is an alternative form of the executor made using luamin

local function CreateExecutionLoop(ast)

	local stringsub,  stringbyte,  stringrep,  stringchar
		= string.sub, string.byte, string.rep, string.char

	local mathfloor,  mathlog
		= math.floor, math.log

	local Type, iPairs, Select, Unpack, Getfenv, Tonumber, Assert
		= type, ipairs, select, unpack, getfenv, tonumber, assert

	local True, False, Nil
		= true, false, nil

	local executeStatlist
	local executeExpression

	local function CreateExecutionScope(parent)
		local scope = {P=parent, L={}}
		function scope:GL(name)
			local my = scope.L[name]
			if my then return my end

			if scope.P then
				local par = scope.P:GL(name)
				if par then return par end
			end

			return Nil
		end
		function scope:SL(name, value)
			scope:GL(name)[1] = value
		end
		function scope:ML(name, value)
			--create my own var
			local my = {}
			my[0] = name
			my[1] = value
			scope.L[name] = my
			return my
		end
		return scope
	end

	local FunctionEnvironment = Getfenv()
	local AmbiguityTracker = {}
	local function GetVarargsInfo(...)
		return {...}, Select("#", ...)
	end
	local function EvaluateExpressionList(ExpressionList, Container, scope)
		--Evaluates expressions in a way that handles lua's value truncation, E.g.
		---- < local a,b,c,d,e = (function()return 1,2,3,4,5 end)(), (function()return 3 end)(); print(a,b,c,d,e)
		---- > 1 3 nil nil nil
		local ELLength = #ExpressionList
		if ELLength == 0 then
			return 0
		end
		--All non-last values are truncated
		for i = 1,ELLength-1 do
			Container[i] = executeExpression(ExpressionList[i], scope) --Auto-truncates
		end
		--And the last one should give all of its values
		local Out, OutLength = GetVarargsInfo(executeExpression(ExpressionList[ELLength], scope))
		for i = 1,OutLength do
			Container[ELLength+i-1] = Out[i]
		end
		AmbiguityTracker[Container] = {Nil,ELLength+OutLength}
		return ELLength+OutLength-1
	end
	local function SafeUnpack(t, dontClear) --Unpack while considering the real length of the table (see above)
		local TData = AmbiguityTracker[t]
		if TData then
			if not dontClear then --For "..."
				AmbiguityTracker[t] = Nil --Clear memory, since we won't need it afterwards
			end
			return Unpack(t,1,TData[2]-1)
		else
			return Unpack(t)
		end
	end

	executeExpression = function(expr, scope, SpecialState)
		local AstType = expr[5]
		if AstType == 1 then --Function
			return function(...)
				local childScope = CreateExecutionScope(scope)
				local inputArgs = {...}
				local standardArgs = #expr[2]
				for i = 1, standardArgs do
					local arg = expr[2][i]
					if arg then
						childScope:ML(arg[0], inputArgs[i])
					end
				end
				if expr[3] then
					local inputArgCount = Select("#", ...)
					local varargs = {}
					for i = standardArgs+1, inputArgCount do
						varargs[i-standardArgs] = inputArgs[i]
					end
					AmbiguityTracker[varargs] = {Nil, inputArgCount-#expr[2]+1}
					childScope:ML(-1, varargs) -- -1 is the reserved LocalID for local "..."
				end
				local ReturnData = executeStatlist(expr[1], childScope)
				if not ReturnData then --No return statement to handle
					return
				else--if ReturnData.T == 1 then --Get the return data
					return SafeUnpack(ReturnData.D)
				--else --Uh oh!
					--This else should not be needed as the parser now determines if this is valid behaviour
				end
			end

		elseif AstType == 2 then --VarExpr
			if SpecialState then
				return expr, True
			else
				if expr[2] then
					return scope:GL(expr[0])[1]
				end
				return FunctionEnvironment[expr[0]]
			end

		elseif AstType == 3 then --MemberExpr (Includes IndexExpr)
			if SpecialState then
				return expr, True
			else
				if expr[2] == False then
					return executeExpression(expr[0], scope)[executeExpression(expr[1], scope)]
				elseif expr[2] == True then --Account for namecall calls of functions by forcing in a self
					local Container = executeExpression(expr[0], scope)
					local out = Container[expr[1][0]]
					if Type(out) == "function" then
						return function(...)
							return out(Container, ...)
						end
					else
						return out
					end
				end
			end

		elseif AstType == 4 then --CallExpr (Includes all short-hand call expressions)
			local args = {}
			EvaluateExpressionList(expr[2], args, scope)
			if expr[3] then --subtle truncation via extra parenthesis
				return ({executeExpression(expr[0], scope)(SafeUnpack(args))})[1] --damn luamin doesn't let me make this a subtle truncation itself
			else
				return executeExpression(expr[0], scope)(SafeUnpack(args))
			end

		elseif AstType == 5 then --NumberExpr
			return Tonumber(expr[1][0])

		elseif AstType == 6 then --StringExpr
			return expr[1][0]

		elseif AstType == 7 then --NilExpr
			return Nil

		elseif AstType == 8 then --BooleanExpr
			return expr[1]

		elseif AstType == 9 then --DotsExpr
			-- -1 is the reserved LocalID for local "..."
			if expr[0] then
				return scope:GL(-1)[1][1]
			else
				return SafeUnpack(scope:GL(-1)[1], True)
			end

		elseif AstType == 10 then --ConstructorExpr
			local out = {}
			--Process all key'd entries first
			local unkeyed = {}
			for _, entry in iPairs(expr[1]) do
				if entry[2] == 0 then
					out[executeExpression(entry[3], scope)] = executeExpression(entry[1], scope)
				elseif entry[2] == 1 then
					out[entry[3]] = executeExpression(entry[1], scope)
				else--if entry[2] == 2 then
					unkeyed[#unkeyed+1] = entry[1]
				end
			end
			--And then do the unkey'd ones
			EvaluateExpressionList(unkeyed, out, scope)
			return out

		elseif AstType == 11 then --UnopExpr
			local Rhs = executeExpression(expr[0], scope)
			local op = expr[2]
			if op == 1 then
				return -Rhs
			elseif op == 2 then
				return not Rhs
			elseif op == 3 then
				return #Rhs
			end

		elseif AstType == 12 then --BinopExpr
			local op = expr[2]
			local Lhs = executeExpression(expr[1], scope)
			--The RHS should only be evaluated for and/or if the LHS doesn't complete the condition
			if op == 14 then
				return Lhs and executeExpression(expr[0], scope)
			elseif op == 15 then
				return Lhs or executeExpression(expr[0], scope)
			end
			local Rhs = executeExpression(expr[0], scope)
			if op == 1 then
				return Lhs + Rhs
			elseif op == 2 then
				return Lhs - Rhs
			elseif op == 3 then
				return Lhs % Rhs
			elseif op == 4 then
				return Lhs / Rhs
			elseif op == 5 then
				return Lhs * Rhs
			elseif op == 6 then
				return Lhs ^ Rhs
			elseif op == 7 then
				return Lhs .. Rhs
			elseif op == 8 then
				return Lhs == Rhs
			elseif op == 9 then
				return Lhs < Rhs
			elseif op == 10 then
				return Lhs <= Rhs
			elseif op == 11 then
				return Lhs ~= Rhs
			elseif op == 12 then
				return Lhs > Rhs
			elseif op == 13 then
				return Lhs >= Rhs
			end

		end
	end

	local executeStatement = function(statement, scope)
		local AstType = statement[5]
		if AstType == 1 then --Function
			local name = statement[0]
			if name[5] == 3 then
				local Container = executeExpression(name[0], scope)
				if name[2] == False then
					Container[executeExpression(name[1], scope)] = executeExpression(statement, scope)
				elseif name[2] == True then --Special flag call to ensure self logic
					local f = executeExpression(statement, scope, True)
					Container[name[1][0]] = f
				end

			else
				local f = executeExpression(statement, scope)
				if statement[4] then
					scope:ML(name[0], f)
				elseif name[2] then
					scope:SL(name[0], f)
				else
					FunctionEnvironment[name[0]] = f
				end
			end

		elseif AstType == 2 then --IfStatement
			for _, Clause in iPairs(statement[0]) do
				if not Clause[0] or executeExpression(Clause[0], scope) then
					return executeStatlist(Clause[1], CreateExecutionScope(scope))
				end
			end

		elseif AstType == 3 then --WhileStatement
			while executeExpression(statement[0], scope) do
				local ReturnData = executeStatlist(statement[1], CreateExecutionScope(scope))
				if ReturnData then
					if ReturnData.T == 2 then --Break, get out
						return
					elseif ReturnData.T == 1 then
						return ReturnData --Return, propogate
					end --Else: A continue, just keep going
				end
			end

		elseif AstType == 4 then --DoStatement
			return executeStatlist(statement[1], CreateExecutionScope(scope))

		elseif AstType == 5 then --NumericForStatement
			local var = Tonumber(executeExpression(statement[0], scope))
			local limit = Tonumber(executeExpression(statement[2], scope))
			local step = statement[3] and Tonumber(executeExpression(statement[3], scope)) or 1

			while (step > 0 and var <= limit) or (step <= 0 and var >= limit) do
				local childScope = CreateExecutionScope(scope)
				childScope:ML(statement[4][0], var)
				local ReturnData = executeStatlist(statement[1], childScope)
				if ReturnData then
					if ReturnData.T == 2 then --Break, get out
						return
					elseif ReturnData.T == 1 then
						return ReturnData --Return, propogate
					end --Else: A continue, just keep going
				end
				var = var + step
			end

		elseif AstType == 6 then --GenericForStatement
			local gen1, gen2, gen3
			local generators = statement[0]
			if not generators[2] then
				gen1, gen2, gen3 = executeExpression(generators[1], scope)
			else
				gen1 = executeExpression(generators[1], scope)
				gen2 = executeExpression(generators[2], scope)
				if generators[3] then
					gen3 = executeExpression(generators[3], scope)
				end
			end
			while True do
				local childScope = CreateExecutionScope(scope)
				local args = {gen1(gen2, gen3)}
				--We aren't gonna use HandleReturnAmbiguity here, it just isnt worth it as far as im concerned
				gen3 = args[1]
				if gen3 == Nil then
					break
				end
				--Define for-loop locals
				for i = 1, #statement[2] do
					childScope:ML(statement[2][i][0], args[i])
				end
				local ReturnData = executeStatlist(statement[1], childScope)
				if ReturnData then
					if ReturnData.T == 2 then --Break, get out
						return
					elseif ReturnData.T == 1 then
						return ReturnData --Return, propogate
					end --Else: A continue, just keep going
				end
			end

		elseif AstType == 7 then --ReoeatStatement
			local subScope = CreateExecutionScope(scope)
			repeat
				local ReturnData = executeStatlist(statement[1], subScope)
				if ReturnData then
					if ReturnData.T == 2 then --Break, get out
						return
					elseif ReturnData.T == 1 then
						return ReturnData --Return, propogate
					end --Else: A continue, just keep going
				end
			until executeExpression(statement[0], subScope)

		elseif AstType == 8 then --LocalStatement
			local out = {}
			EvaluateExpressionList(statement[0], out, scope)
			for i = 1, #statement[1] do
				local l = statement[1][i]
				scope:ML(l[0], out[i])
			end

		elseif AstType == 9 then --ReturnStatement
			local arguments = {}
			EvaluateExpressionList(statement[2], arguments, scope)
			return arguments

		elseif AstType == 10 then --BreakStatement
			return True --This just works, ok?

		elseif AstType == 11 then --ContinueStatement
			return False --This too

		elseif AstType == 12 then --AssignmentStatement
			local FinalRhs
			local function HandleTruncation(i)
				if i >= #statement[0] then
					if FinalRhs == Nil then
						FinalRhs = {executeExpression(statement[0][#statement[0]], scope)}
					end
					return FinalRhs[i-#statement[0]+1]
				else
					return ({executeExpression(statement[0][i], scope)})[1]
				end
			end
			for i = 1, #statement[1] do
				local Lhs, wasExprExit = executeExpression(statement[1][i], scope, True)
				if wasExprExit then
					if Lhs[5] == 2 then
						if Lhs[2] then
							scope:SL(Lhs[0], HandleTruncation(i))
						else
							FunctionEnvironment[Lhs[0]] = HandleTruncation(i)
						end

					else--if Lhs[5] == 3 then
						local Container = executeExpression(Lhs[0], scope)
						Container[executeExpression(Lhs[1], scope)] = HandleTruncation(i)

					--It will always be one of the above types. If it's not, thats a serializer error
					end
				end
			end

		elseif AstType == 13 then --CallStatement
			executeExpression(statement[0], scope)

		end
	end

	executeStatlist = function(statList, scope)
		--A type of 1 is a return
		--A type of 2 is a break
		--A type of 3 is a continue
		for _, stat in iPairs(statList) do
			local out = executeStatement(stat, scope)
			if Type(out) == "table" then
				if not out.P then --Create an internal token
					return {P = True, T = 1, D = out}
				else --Pass on an internal token
					return out
				end
			elseif Type(out) == "boolean" then --The statement was escaped via a break or continue
				return {P = True, T = (out==True and 2 or 3)}
			end
		end
	end

	return (function()
		ast = (function(x)
			local log2 = mathlog(2)
			local function padleft(s,n,p)
				return stringrep(p,n-#s)..s
			end
			local function ToNum(b)
				return Tonumber(b,2)
			end
			local function ToBit(n,pad)
				--Assert(n%1 == 0,"Can't convert non-int")
				if n == 0 then
					return padleft("0",pad or 1,"0")
				end
				local pow = mathfloor(mathlog(n)/log2)
				local final = ""
				while pow >= 0 do
					local p2 = 2^pow
					if n >= p2 then
						n = n - p2
						final = final .. "1"
					else
						final = final .. "0"
					end
					pow = pow - 1
				end
				return padleft(final,pad or 1,"0")
			end
			local function DBitToNum(dbits)
				local pow = 0
				local result = 0
				for index = 1,#dbits do
					local bit = stringsub(dbits,index,index)
					if bit == "1" then
						result = result + 2^pow
					end
					pow = pow - 1
				end
				return result
			end

			local BufferPoint = 1
			local BitData = ""
			local function BufferSanityCheck(len)
				for i = 1, (len-#BitData-1)/6+1 do
					BitData = BitData .. ToBit(stringbyte(x,BufferPoint,BufferPoint)%64,6)
					BufferPoint = BufferPoint + 1
				end
			end
			local function ReadRaw(len)
				BufferSanityCheck(len)
				local RequestedData = stringsub(BitData,1,len)
				BitData = stringsub(BitData,len+1)
				return RequestedData
			end
			local function Read(len)
				return ToNum(ReadRaw(len))
			end
			local function ReadByte()
				return stringchar(Read(8))
			end
			local function ReadDouble()
				local sign,Exponent,Mantissa = Read(1),Read(11),ReadRaw(52)
				sign,Exponent = (sign==0 and 1 or -1),2^(Exponent-1023)
				return sign * Exponent * DBitToNum("1"..Mantissa)
			end

			local TYPE_TABLE_START=0
			local TYPE_TABLE_END=1
			local TYPE_STRING=2
			local TYPE_NUMBER=3
			local TYPE_BOOLEAN=4
			local TYPE_NUMBER_BASIC=5
			local TYPE_NUMBER_SUPERBASIC=6
			local TYPE_NUMBER_SIMPLE=7

			local TYPE_WIDTH=3

			local function Deserialize(NoAssert)
				if not NoAssert then
					Assert(Read(TYPE_WIDTH) == TYPE_TABLE_START,"Invalid SD") --Most serializer errors get caught here
				end
				local Output = {}
				local Saved = Nil
				local function HandleKVSorting(Data)
					if Saved then
						Output[Saved] = Data
						Saved = Nil
					else
						Saved = Data
					end
				end
				while True do
					local ObjType = Read(TYPE_WIDTH)
					if ObjType == TYPE_TABLE_END then
						return Output
					elseif ObjType == TYPE_TABLE_START then
						HandleKVSorting(Deserialize(True))
					elseif ObjType == TYPE_STRING then
						local Result = ""
						while True do
							local NextByte = ReadByte()
							if NextByte == "\0" then
								HandleKVSorting(Result)
								break
							elseif NextByte == "\\" then
								Result = Result .. ReadByte()
							else
								Result = Result .. NextByte
							end
						end
					elseif ObjType == TYPE_NUMBER then
						HandleKVSorting(ReadDouble())
					elseif ObjType == TYPE_BOOLEAN then
						HandleKVSorting(Read(1) == 1) --Simple enough
					elseif ObjType == TYPE_NUMBER_SUPERBASIC then
						HandleKVSorting(Read(3))
					elseif ObjType == TYPE_NUMBER_BASIC then
						HandleKVSorting(Read(5))
					elseif ObjType == TYPE_NUMBER_SIMPLE then
						HandleKVSorting(Read(8))
					end
				end
			end
			return Deserialize()
		end)(ast)
		local ReturnData = executeStatlist(ast, CreateExecutionScope())
		if not ReturnData then --No return statement to handle
			return
		else--if ReturnData.T == 1 then --Get the return data
			return SafeUnpack(ReturnData.D)
		--else --Uh oh!
			--This else should not be needed as the parser now determines if this is valid behaviour
		end
	end)
end

local t = CreateExecutionLoop([=[]=])()
print(t)
local RewriterOptions

local CommaSplitter, EqualsSplitter
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

local _ValidCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
local function GenerateRandomString(P1, P2)
	if P2 then --Min, Max
		P1 = P2 - math.random(0, P2-P1)
	elseif P1 then --Exact
		-- Do nothing
	else --No input, use default
		P1 = math.random(20, 25)
	end
	local out = ""
	for i = 1,P1 do
		local max = #_ValidCharacters
		if i == 1 then
			max = max - 10
		end
		local choice = math.random(1,max)
		out = out .. string.sub(_ValidCharacters,choice,choice)
	end
	return out
end

local LastVar = {}
local function GenerateVariableName()
	if not RewriterOptions.MinifyVariableNames then
		return GenerateRandomString()
	end
	for i = #LastVar, 1, -1 do
		if LastVar[i] < #_ValidCharacters and (i ~= 1 or LastVar[i] < #_ValidCharacters-10) then
			LastVar[i] = LastVar[i] + 1
			local out = ""
			for _,j in next,LastVar do
				out = out .. _ValidCharacters:sub(j, j)
			end
			if out == "if" or out == "do" or out == "in" or out == "as" then -- safety
				return GenerateVariableName()
			end
			return out
		else
			LastVar[i] = 1
		end
	end
	-- Reached loop end, restart and become bigger
	for i = 1, #LastVar + 1 do
		LastVar[i] = 1
	end
	return string.rep(_ValidCharacters:sub(1, 1), #LastVar)
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
		if (RewriterOptions.ObscureVariableNames or RewriterOptions.MinifyVariableNames) and not newName then
			self.Locals[name] = GenerateVariableName()
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
		if Expression.Base.AstType == "ConstructorExpr" then
			Base = "(" .. Base .. ")"
		end
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
	error("We didn't return on an expression!? " .. tostring(Expression) .. " " .. (type(Expression)=="table" and tostring(Expression.AstType) or "<no AST>"))
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
		local NewValues = {}
		for i,Value in ipairs(Statement.InitList) do
			NewValues[i] = WriteExpression(Value, Scope)
		end
		local NewLocals = {}
		for i,Local in ipairs(Statement.LocalList) do
			NewLocals[i] = Scope:MakeLocal(Local.Name)
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

return function(AST, Options)
	RewriterOptions = Options
	CommaSplitter = (RewriterOptions.AddExtraSpacing and ", " or ",")
	EqualsSplitter = (RewriterOptions.AddExtraSpacing and " = " or "=")
	return WriteStatList(AST, CreateExecutionScope(), true)
end
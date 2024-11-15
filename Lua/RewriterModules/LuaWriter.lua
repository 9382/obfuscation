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

local function CompileWithFormattingData(Lines)
	if RewriterOptions.UseNewlines then
		return table.concat(Lines,"\n")
	else
		return table.concat(Lines," ")
	end
end

-- Expressions that don't require bracketing to count as valid prefix expressions
-- prefixexp ::= var | functioncall | `(´ exp `)´
local StandardPrefixExpressions = {VarExpr=true, MemberExpr=true, IndexExpr=true, CallExpr=true, StringCallExpr=true, TableCallExpr=true}
local UnaryPrecedence = 8
local OperatorPrecedence = { -- From the LuaParser
	["+"] = {6,6},
	["-"] = {6,6},
	["%"] = {7,7},
	["/"] = {7,7},
	["*"] = {7,7},
	["^"] = {10,9},
	[".."] = {5,4},
	["=="] = {3,3},
	["<"] = {3,3},
	["<="] = {3,3},
	["~="] = {3,3},
	[">"] = {3,3},
	[">="] = {3,3},
	["and"] = {2,2},
	["or"] = {1,1},
}

local WriteStatlist
local function WriteExpression(Expression)
	Expression.ParenCount = Expression.ParenCount or 0
	if Expression.AstType == "Function" then
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = Expression.Arguments[i].Name
		end
		if Expression.VarArg then
			NewArguments[#NewArguments+1] = "..."
		end
		local Lines = {"function(" .. table.concat(NewArguments, CommaSplitter) .. ")"}
		local Body = WriteStatlist(Expression.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Expression.AstType == "VarExpr" then
		if Expression.Local then
			return Expression.Local.Name
		else
			return Expression.Name
		end

	elseif Expression.AstType == "MemberExpr" then
		local Base = WriteExpression(Expression.Base)
		local BaseAst = Expression.Base.AstType
		if not StandardPrefixExpressions[BaseAst] then --Special case for non-standard prefix expressions
			Base = "(" .. Base .. ")"
		end
		return Base .. Expression.Indexer .. Expression.Ident.Data

	elseif Expression.AstType == "IndexExpr" then
		local Base = WriteExpression(Expression.Base)
		local BaseAst = Expression.Base.AstType
		if not StandardPrefixExpressions[BaseAst] then --Special case for non-standard prefix expressions
			Base = "(" .. Base .. ")"
		end
		return Base .. "[" .. WriteExpression(Expression.Index) .. "]"

	elseif Expression.AstType == "CallExpr" then
		local Base = WriteExpression(Expression.Base)
		local BaseAst = Expression.Base.AstType
		if not StandardPrefixExpressions[BaseAst] then --Special case for non-standard prefix expressions
			Base = "(" .. Base .. ")"
		end
		if RewriterOptions.UseShortCallExprs and #Expression.Arguments == 1 then
			local Arg1 = Expression.Arguments[1]
			if Arg1.AstType == "StringExpr" or Arg1.AstType == "ConstructorExpr" then
				return Base .. WriteExpression(Arg1)
			end
		end
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = WriteExpression(Argument)
		end
		if Expression.ParenCount > 0 then
			return "(" .. Base .. "(" .. table.concat(NewArguments, CommaSplitter) .. "))" --subtle truncation
		else
			return Base .. "(" .. table.concat(NewArguments, CommaSplitter) .. ")"
		end

	elseif Expression.AstType == "StringCallExpr" or Expression.AstType == "TableCallExpr" then
		local Base = WriteExpression(Expression.Base)
		local BaseAst = Expression.Base.AstType
		if not StandardPrefixExpressions[BaseAst] then --Special case for non-standard prefix expressions
			Base = "(" .. Base .. ")"
		end
		local Argument
		if Expression.AstType == "StringCallExpr" then
			Argument = Expression.Arguments[1].Data
		else
			Argument = WriteExpression(Expression.Arguments[1])
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
		return Expression.Value.Data

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
				Parts[#Parts+1] = WriteExpression(Entry.Value)
			elseif Entry.Type == "Key" then
				Parts[#Parts+1] = "[" .. WriteExpression(Entry.Key) .. "]" .. EqualsSplitter .. WriteExpression(Entry.Value)
			else--if Entry.Type == "Keystring" then
				Parts[#Parts+1] = Entry.Key .. EqualsSplitter .. WriteExpression(Entry.Value)
			end
		end
		return "{" .. table.concat(Parts, CommaSplitter) .. "}"

	elseif Expression.AstType == "UnopExpr" then
		local Rhs = Expression.Rhs
		local RhsOut = WriteExpression(Rhs)
		local precedence
		if Rhs.AstType == "BinopExpr" then
			precedence = OperatorPrecedence[Rhs.Op][2]
		elseif Rhs.AstType == "UnopExpr" then
			precedence = UnaryPrecedence
		end
		if precedence and precedence < UnaryPrecedence then
			RhsOut = "(" .. RhsOut .. ")"
		end
		-- Don't really care about spacing for a unary operator, it makes it less clear if anything
		if Expression.Op == "not" then -- Unless its a bloody not statement
			return Expression.Op .. " " .. RhsOut
		end
		return Expression.Op ..RhsOut

	elseif Expression.AstType == "BinopExpr" then
		local Lhs, Rhs = Expression.Lhs, Expression.Rhs
		local LhsOut, RhsOut = WriteExpression(Lhs), WriteExpression(Rhs)
		local precedence
		if Lhs.AstType == "BinopExpr" then
			precedence = OperatorPrecedence[Lhs.Op][2]
		elseif Lhs.AstType == "UnopExpr" then
			precedence = UnaryPrecedence
		end
		if precedence and precedence < OperatorPrecedence[Expression.Op][1] then
			LhsOut = "(" .. LhsOut .. ")"
		end
		local precedence
		if Rhs.AstType == "BinopExpr" then
			precedence = OperatorPrecedence[Rhs.Op][1]
		elseif Rhs.AstType == "UnopExpr" then
			precedence = UnaryPrecedence
		end
		if precedence and precedence <= OperatorPrecedence[Expression.Op][2] then
			RhsOut = "(" .. RhsOut .. ")"
		end
		if Expression.Op == "or" or Expression.Op == "and" then
			return LhsOut .. " " .. Expression.Op .. " " .. RhsOut
		end
		return LhsOut .. ConsiderSpacingOperator(Expression.Op) .. RhsOut

	end
	error("We didn't return on an expression!? " .. tostring(Expression) .. " " .. (type(Expression)=="table" and tostring(Expression.AstType) or "<no AST>"))
end

local function WriteStatement(Statement)
	if Statement.AstType == "Function" then
		local start
		if Statement.IsLocal then
			start = "local function " .. Statement.Name.Name
		else
			start = "function " .. WriteExpression(Statement.Name)
		end
		local NewArguments = {}
		for i,Argument in ipairs(Statement.Arguments) do
			NewArguments[i] = Argument.Name
		end
		if Statement.VarArg then
			NewArguments[#NewArguments+1] = "..."
		end
		local Lines = {start .. "(" .. table.concat(NewArguments, CommaSplitter) .. ")"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "IfStatement" then
		local Lines = {}
		for i,Clause in ipairs(Statement.Clauses) do
			if i == 1 then
				Lines[#Lines+1] = "if " .. WriteExpression(Clause.Condition) .. " then"
			elseif Clause.Condition then
				Lines[#Lines+1] = "elseif " .. WriteExpression(Clause.Condition) .. " then"
			else
				Lines[#Lines+1] = "else"
			end
			local Body = WriteStatlist(Clause.Body)
			for i = 1,#Body do
				Lines[#Lines+1] = Body[i]
			end
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "WhileStatement" then
		local Lines = {"while " .. WriteExpression(Statement.Condition) .. " do"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "DoStatement" then
		local Lines = {"do"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "NumericForStatement" then
		local Variable = Statement.Variable.Name
		local Start = WriteExpression(Statement.Start)
		local End = WriteExpression(Statement.End)
		local Elements = {Start, End, Statement.Step and WriteExpression(Statement.Step)}
		local Lines = {"for " .. Variable .. EqualsSplitter .. table.concat(Elements, CommaSplitter) .. " do"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "GenericForStatement" then
		local NewVariables = {}
		for i,Variable in ipairs(Statement.VariableList) do
			NewVariables[i] = Variable.Name
		end
		local NewGenerators = {}
		for i,Generator in ipairs(Statement.Generators) do
			NewGenerators[i] = WriteExpression(Generator)
		end
		local Lines = {"for " .. table.concat(NewVariables, CommaSplitter) .. " in " .. table.concat(NewGenerators, CommaSplitter) .. " do"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "end"
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "RepeatStatement" then
		local Lines = {"repeat"}
		local Body = WriteStatlist(Statement.Body)
		for i = 1,#Body do
			Lines[#Lines+1] = Body[i]
		end
		Lines[#Lines+1] = "until " .. WriteExpression(Statement.Condition)
		return CompileWithFormattingData(Lines)

	elseif Statement.AstType == "LocalStatement" then
		local NewValues = {}
		for i,Value in ipairs(Statement.InitList) do
			NewValues[i] = WriteExpression(Value)
		end
		local NewLocals = {}
		for i,Local in ipairs(Statement.LocalList) do
			NewLocals[i] = Local.Name
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
			NewArguments[i] = WriteExpression(Argument)
		end
		return "return " .. table.concat(NewArguments, CommaSplitter)

	elseif Statement.AstType == "BreakStatement" then
		return "break"

	elseif Statement.AstType == "ContinueStatement" then
		return "continue"

	elseif Statement.AstType == "AssignmentStatement" then
		local NewLhs = {}
		for i,Value in ipairs(Statement.Lhs) do
			NewLhs[i] = WriteExpression(Value)
		end
		local NewRhs = {}
		for i,Value in ipairs(Statement.Rhs) do
			NewRhs[i] = WriteExpression(Value)
		end
		return table.concat(NewLhs, CommaSplitter) .. EqualsSplitter .. table.concat(NewRhs, CommaSplitter)

	elseif Statement.AstType == "CallStatement" then
		return WriteExpression(Statement.Expression)

	end
	error("We didn't return on a statement!? " .. tostring(Statement) .. " " .. tostring(type(Statement) == "table" and Statement.AstType or "<no AST>"))
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
WriteStatlist = function(Statlist, DontIndent)
	local out = {}
	for _,Statement in ipairs(Statlist.Body) do
		local StatementText = StringSplit(WriteStatement(Statement) .. ConsiderSemicolon(), "\n")
		if #out > 0 and StatementText[1]:sub(1, 1) == "(" and out[#out]:sub(-1, -1) ~= ";" then
			out[#out] = out[#out] .. ";"
		end
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
	return CompileWithFormattingData(WriteStatlist(AST, true))
end
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

local function JoinSegments(Lhs, Rhs) -- Used for parts of statements
	if Lhs:sub(-1, -1):match("[%w_]") and Rhs:sub(1, 1):match("[%w_]") then
		return Lhs .. " " .. Rhs
	end
	return Lhs .. Rhs
end

local function JoinSegmentList(Segments)
	if #Segments == 0 then
		return ""
	end
	local s = Segments[1]
	for i = 2, #Segments do
		s = JoinSegments(s, Segments[i])
	end
	return s
end

local function JoinWithOperator(Lhs, Op, Rhs) -- For handling special cases with concatenation
	local Part1
	if Op == ".." and Lhs:sub(-1, -1):match("%d") and Lhs:match("%d+$") == Lhs:match("[%w_]+$") then
		Part1 = Lhs .. " " .. Op
	else
		Part1 = JoinSegments(Lhs, Op)
	end
	if Op == ".." and Rhs:sub(1, 3) == "..." then
		return Part1 .. " " .. Rhs
	else
		return JoinSegments(Part1, Rhs)
	end
end

local WriteStatlist
local function WriteExpression(Expression, IsExpandable)
	-- Do not write extra brackets for trimming values when they would get trimmed in the given context anyways
	local ParenCount = IsExpandable and Expression.ParenCount or 0
	if Expression.AstType == "Function" then
		-- TO BE OPTIMISED OR AT LEAST MERGED MAN IDK
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = Expression.Arguments[i].Name
		end
		if Expression.VarArg then
			NewArguments[#NewArguments+1] = "..."
		end
		return JoinSegmentList({
			"function(" .. table.concat(NewArguments, ",") .. ")",
				WriteStatlist(Expression.Body),
			"end"
		})

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
		if #Expression.Arguments == 1 then
			local Arg1 = Expression.Arguments[1]
			if Arg1.AstType == "StringExpr" or Arg1.AstType == "ConstructorExpr" then
				return Base .. WriteExpression(Arg1)
			end
		end
		local NewArguments = {}
		for i,Argument in ipairs(Expression.Arguments) do
			NewArguments[i] = WriteExpression(Argument, i==#Expression.Arguments)
		end
		if ParenCount > 0 then
			return "(" .. Base .. "(" .. table.concat(NewArguments, ",") .. "))" --subtle truncation
		else
			return Base .. "(" .. table.concat(NewArguments, ",") .. ")"
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
		if ParenCount > 0 then
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
		if ParenCount > 0 then
			return "(...)" --subtle truncation
		else
			return "..." --so complex!
		end

	elseif Expression.AstType == "ConstructorExpr" then
		local Parts = {}
		for i,Entry in ipairs(Expression.EntryList) do
			if Entry.Type == "Value" then
				Parts[#Parts+1] = WriteExpression(Entry.Value, i==#Expression.EntryList)
			elseif Entry.Type == "Key" then
				Parts[#Parts+1] = "[" .. WriteExpression(Entry.Key) .. "]" .. "=" .. WriteExpression(Entry.Value)
			else--if Entry.Type == "Keystring" then
				Parts[#Parts+1] = Entry.Key .. "=" .. WriteExpression(Entry.Value)
			end
		end
		return "{" .. table.concat(Parts, ",") .. "}"

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
		if Expression.Op == "not" and RhsOut:sub(1, 1):match("[%w_]") then
			return Expression.Op .. " " .. RhsOut
		end
		return Expression.Op .. RhsOut

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
		return JoinWithOperator(LhsOut, Expression.Op, RhsOut)

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
		return JoinSegmentList({
			start .. "(" .. table.concat(NewArguments, ",") .. ")",
				WriteStatlist(Statement.Body),
			"end"
		})

	elseif Statement.AstType == "IfStatement" then
		local Lines = {}
		for i,Clause in ipairs(Statement.Clauses) do
			if i == 1 then
				Lines[#Lines+1] = "if"
				Lines[#Lines+1] = WriteExpression(Clause.Condition)
				Lines[#Lines+1] = "then"
			elseif Clause.Condition then
				Lines[#Lines+1] = "elseif"
				Lines[#Lines+1] = WriteExpression(Clause.Condition)
				Lines[#Lines+1] = "then"
			else
				Lines[#Lines+1] = "else"
			end
			Lines[#Lines+1] = WriteStatlist(Clause.Body)
		end
		Lines[#Lines+1] = "end"
		return JoinSegmentList(Lines)

	elseif Statement.AstType == "WhileStatement" then
		return JoinSegmentList({
			"while", WriteExpression(Statement.Condition), "do",
				WriteStatlist(Statement.Body),
			"end"
		})

	elseif Statement.AstType == "DoStatement" then
		return JoinSegmentList({
			"do",
				WriteStatlist(Statement.Body),
			"end"
		})

	elseif Statement.AstType == "NumericForStatement" then
		local Variable = Statement.Variable.Name
		local Start = WriteExpression(Statement.Start)
		local End = WriteExpression(Statement.End)
		local Elements = {Start, End, Statement.Step and WriteExpression(Statement.Step)}
		return JoinSegmentList({
			"for", Variable, "=", table.concat(Elements, ","), "do",
				WriteStatlist(Statement.Body),
			"end"
		})

	elseif Statement.AstType == "GenericForStatement" then
		local NewVariables = {}
		for i,Variable in ipairs(Statement.VariableList) do
			NewVariables[i] = Variable.Name
		end
		local NewGenerators = {}
		for i,Generator in ipairs(Statement.Generators) do
			NewGenerators[i] = WriteExpression(Generator, i==#Statement.Generators)
		end
		return JoinSegmentList({
			"for", table.concat(NewVariables, ","), "in", table.concat(NewGenerators, ","), "do",
				WriteStatlist(Statement.Body),
			"end"
		})

	elseif Statement.AstType == "RepeatStatement" then
		return JoinSegmentList({
			"repeat",
				WriteStatlist(Statement.Body),
			"until", WriteExpression(Statement.Condition)
		})

	elseif Statement.AstType == "LocalStatement" then
		local NewValues = {}
		for i,Value in ipairs(Statement.InitList) do
			NewValues[i] = WriteExpression(Value, i==#Statement.InitList)
		end
		local NewLocals = {}
		for i,Local in ipairs(Statement.LocalList) do
			NewLocals[i] = Local.Name
		end
		if #NewValues > 0 then
			return "local " .. table.concat(NewLocals, ",") .. "=" .. table.concat(NewValues, ",")
		else
			return "local " .. table.concat(NewLocals, ",")
		end

	elseif Statement.AstType == "ReturnStatement" then
		if #Statement.Arguments == 0 then
			return "return"
		end
		local NewArguments = {}
		for i,Argument in ipairs(Statement.Arguments) do
			NewArguments[i] = WriteExpression(Argument, i==#Statement.Arguments)
		end
		return JoinSegmentList({"return", table.concat(NewArguments, ",")})

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
			NewRhs[i] = WriteExpression(Value, i==#Statement.Rhs)
		end
		return table.concat(NewLhs, ",") .. "=" .. table.concat(NewRhs, ",")

	elseif Statement.AstType == "CallStatement" then
		return WriteExpression(Statement.Expression)

	end
	error("We didn't return on a statement!? " .. tostring(Statement) .. " " .. tostring(type(Statement) == "table" and Statement.AstType or "<no AST>"))
end

local function JoinStatements(LeftText, LeftStatement, RightText, RightStatement)
	local LeftType, RightType = LeftStatement.AstType, RightStatement.AstType
	-- Note that a statement can only start with `a-zA-Z_(`, so RightText matches will do %a instead of %w
	if RightText:sub(1, 1) == "(" then -- Right side is a CallStatement that requires being bracket-wrapped
		-- If the left side could be misinterpreted as a prefixexp, force a semicolon
		-- (Holy this case has so many special rules I love optimising :D)
		if LeftText:sub(-1, -1) == ")" then
			-- Not a single case where this won't be seen as different without the ;
			return LeftText .. ";" .. RightText
		end
		if
			LeftType == "Function" or
			LeftType == "IfStatement" or
			LeftType == "WhileStatement" or
			LeftType == "DoStatement" or
			LeftType == "NumericForStatement" or
			LeftType == "GenericForStatement" or
			LeftType == "BreakStatement" or
			LeftType == "ContinueStatement"
		then
			-- Statement always ends with a keyword, meaning no ; is required
			return LeftText .. RightText
		end
		if LeftType == "CallStatement" then
			-- Statement always ends as a valid prefixexp
			return LeftText .. ";" .. RightText
		end
		-- At this point we know we end with a non-bracket wrapped expression
		-- Now we just need to figure out if it meets the conditions of a `var` or `functioncall`
		local LastExpression
		if LeftType == "RepeatStatement" then
			LastExpression = LeftStatement.Condition
		elseif LeftType == "LocalStatement" then
			LastExpression = LeftStatement.InitList[#LeftStatement.InitList]
		elseif LeftType == "ReturnStatement" then
			LastExpression = LeftStatement.Arguments[#LeftStatement.Arguments]
		elseif LeftType == "AssignmentStatement" then
			LastExpression = LeftStatement.Rhs[#LeftStatement.Rhs]
		end
		while LastExpression and (LastExpression.AstType == "BinopExpr" or LastExpression.AstType == "UnopExpr") do
			LastExpression = LastExpression.Rhs
		end
		if not LastExpression then
			print("me is special case", LeftType, LeftText)
			-- Special case of a `local a,b,c` with no `=` part or an empty `return`
			return LeftText .. RightText
		elseif
			LastExpression.AstType == "CallExpr" or
			LastExpression.AstType == "StringCallExpr" or
			LastExpression.AstType == "TableCallExpr" or
			LastExpression.AstType == "VarExpr" or
			LastExpression.AstType == "IndexExpr" or
			LastExpression.AstType == "MemberExpr"
		then
			-- Can be seen as a prefixexp, split it up
			return LeftText .. ";" .. RightText
		else
			-- Isn't a valid prefixexp so no ; is required
			return LeftText .. RightText
		end
	end
	if LeftText:sub(-1, -1):match("[%a_]") and RightText:sub(1, 1):match("[%a_]") then
		-- Both sides meet at a word of some sorts, gotta seperate
		return LeftText .. ";" .. RightText
	end
	if LeftText:sub(-1, -1):match("%d") and RightText:sub(1, 1):match("[%a_]") then
		-- Potential for a malformed number, gotta seperate
		return LeftText .. ";" .. RightText
	end
	return LeftText .. RightText
end

WriteStatlist = function(Statlist)
	local Statements = Statlist.Body
	if #Statements == 0 then
		return ""
	end
	local StatementTexts = {}
	for _,Statement in ipairs(Statements) do
		StatementTexts[#StatementTexts+1] = WriteStatement(Statement)
	end
	local out = StatementTexts[1]
	for i = 2,#StatementTexts do
		out = JoinStatements(
			out              , Statements[i-1],
			StatementTexts[i], Statements[i]
		)
	end
	return out
end

return function(AST)
	return WriteStatlist(AST)
end
local ParseLua = require("LuaParser")

--== BEGIN REWRITER ==--

local _seed = os.time()
math.randomseed(_seed)
print("Using seed", _seed)

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

	--== MinifyVariableNames ==--
	-- Replaces local variable names with minified characters. The variable must be localised. Takes priority over ObscureVariableNames
	MinifyVariableNames = true,

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

Consider making a default "+1" operation at the end of the loop instead of each statement pointing to its target?

Note that defining a local twice (local x; local x) can cause some unexpected behaviour and may produce wrong code
I expect the second local will be treated like it didn't exist and turned into an assignment, causing the code to continue using the old var
This could be wrong in extremely specific cases

This entire process of "control flattening" is very difficult to do in lua, considering scopes are very closed
Most languages bring in new scopes per function, but we get new scopes per any sort of Body entry (Like an If statement)
This forces us to do some quite ugly and unreliable self-management
--]]
local function FlattenControlFlow(ast)
	local function PerformFlattening(Body, FunctionVariables, FunctionDepth)
		local BodyScope = Body.Scope
		Body = Body.Body
		FunctionDepth = FunctionDepth or 0
		local NewAST = {}
		--Step 1: Variable collection and scope calculations
		local Variables = {}
		local InstructionPointer = "__ins" .. math.random(1e4,1e5-1) .. "__"
		local InstructionExpression = {AstType="VarExpr", Name=InstructionPointer, Local={CanRename=true, Scope=BodyScope, Name=InstructionPointer}}
		Variables[1] = InstructionPointer
		local ScopeChain = {BodyScope}
		local function GetNewVariable(VarName)
			return VarName .. "__ver_" .. FunctionDepth .. "_" .. #ScopeChain
		end
		local function DeepScan(t, IsPreDefined, blacklist)
			blacklist = blacklist or {}
			blacklist[t] = true
			for k,v in next,t do
				if type(v) == "table" and k ~= "Scope" and not blacklist[v] then
					if v.AstType == "Function" then
						if v.IsLocal then
							--This is just to fix local functions that get defined but never used
							--What if you just didn't do that in the first place? :)
							local Name = v.Name
							if Name and not Name.TrueName then
								local NewName = GetNewVariable(Name.Name)
								Name.TrueName = Name.Name
								Name.Name = NewName
								Variables[#Variables+1] = NewName
							end
						elseif v.Name then --Potentially a member expression or something, give it a check
							DeepScan(v.Name, IsPreDefined, blacklist)
						end
						v.Body.Body = PerformFlattening(v.Body, v.Arguments, FunctionDepth+1)
					elseif v.AstType == "VarExpr" then
						if v.Local and not v.TrueName then
							if not v.Local.TrueName then
								local NewName = GetNewVariable(v.Local.Name)
								v.Local.TrueName = v.Local.Name
								v.Local.Name = NewName
								v.Name = NewName --not required since rewriter only uses local name but its good practice
								Variables[#Variables+1] = NewName
							end
							v.TrueName = v.Local.TrueName
							v.Name = v.Local.Name --Don't declare new variable scope if its just another occurance, that gets indicated by new lcoal
						end --no need to scan a VarExpr
					elseif v.AstType == "LocalStatement" then
						for _,Local in next,v.LocalList do
							if not Local.TrueName then
								local NewName = GetNewVariable(Local.Name)
								Local.TrueName = Local.Name
								Local.Name = NewName
								Variables[#Variables+1] = NewName
							end
						end
						DeepScan(v.InitList, IsPreDefined, blacklist)
					elseif v.AstType == "IfStatement" then
						--for clause in statement
						for _,Clause in next,v.Clauses do
							ScopeChain[#ScopeChain+1] = Clause.Body.Scope
							DeepScan(Clause, IsPreDefined, blacklist)
							ScopeChain[#ScopeChain] = nil
						end
						DeepScan(v, IsPreDefined, blacklist)
					elseif v.AstType == "WhileStatement"
						or v.AstType == "DoStatement"
						or v.AstType == "RepeatStatement"
						or v.AstType == "NumericForStatement"
						or v.AstType == "GenericForStatement" then
						ScopeChain[#ScopeChain+1] = v.Body.Scope
						if v.AstType == "NumericForStatement" then
							local StartName = "_start__dedicated"
							local FinishName = "_finish__dedicated"
							local StepName = "_step__dedicated"
							v.DedicatedVariable = {AstType="VarExpr", Name=v.Variable.Name.."__dedicated", Local={CanRename=true, Scope=v.Body.Scope, Name=v.Variable.Name .. "__dedicated"}}
							v.DedicatedStart = {AstType="VarExpr", Name=StartName, Local={CanRename=true, Scope=v.Body.Scope, Name=StartName}}
							v.DedicatedFinish = {AstType="VarExpr", Name=FinishName, Local={CanRename=true, Scope=v.Body.Scope, Name=FinishName}}
							v.DedicatedStep = {AstType="VarExpr", Name=StepName, Local={CanRename=true, Scope=v.Body.Scope, Name=StepName}}
						elseif v.AstType == "GenericForStatement" then
							local KeyName = "_key__dedicated"--v.VariableList[1].Name.."__dedicated"
							local GenName = "_gen__dedicated"--__"..math.random(1e4,1e5-1)
							local ObjName = "_obj__dedicated"--__"..math.random(1e4,1e5-1)
							v.DedicatedKey = {AstType="VarExpr", Name=KeyName, Local={CanRename=true, Scope=v.Body.Scope, Name=KeyName}}
							v.DedicatedGenerator = {AstType="VarExpr", Name=GenName, Local={CanRename=true, Scope=v.Body.Scope, Name=GenName}}
							v.DedicatedObject = {AstType="VarExpr", Name=ObjName, Local={CanRename=true, Scope=v.Body.Scope, Name=ObjName}}
						end
						DeepScan(v, IsPreDefined, blacklist)
						ScopeChain[#ScopeChain] = nil
					elseif v.CanRename then --The raw form of a Local which we've somehow come across (probably a function arg)
						if not v.TrueName then
							local NewName = GetNewVariable(v.Name)
							v.TrueName = v.Name
							v.Name = NewName
							if not IsPreDefined then
								Variables[#Variables+1] = NewName
							end
						end
					else
						DeepScan(v, IsPreDefined, blacklist)
					end
				end
			end
		end
		if FunctionVariables then
			DeepScan(FunctionVariables, true)
		end
		DeepScan(Body)
		local SeenVariables = {}
		local VariableObjects = {}
		for i,Variable in next,Variables do
			if not SeenVariables[Variable] then
				VariableObjects[#VariableObjects+1] = {CanRename=true, Scope=BodyScope, Name=Variable}
				SeenVariables[Variable] = true
			else
				print("WARNING: Double-definition of local " .. Variable .. " found during flattening - no guarantee of expected behaviour")
			end
		end
		NewAST[1] = {
			AstType = "LocalStatement",
			LocalList = VariableObjects,
			InitList = {{AstType="NumberExpr", Value={Data="1"}}},
		}

		--Step 2: Compile the statements
		local function CreateInstructionPointer(nextIndex)
			return {
				AstType="AssignmentStatement",
				Lhs={InstructionExpression},
				Rhs={{AstType="NumberExpr", Value={Data=tostring(nextIndex)}}}
			}
		end
		local function StandardProcedure(Statement, index, forceNext)
			return {
				Body = {AstType="Statlist", Scope=BodyScope, Body={
					Statement,
					CreateInstructionPointer(forceNext or index+1),
				}},
				Condition = {
					AstType="BinopExpr", Op="==",
					Rhs={AstType="NumberExpr", Value={Data=tostring(index)}},
					Lhs=InstructionExpression,
				}
			}
		end
		local function OffsetInstructions(t, offset, minimum, blacklist) --This is my punishment for not being modular - round 2!
			minimum = minimum or -9e9
			blacklist = blacklist or {}
			blacklist[t] = true
			for k,v in next,t do
				if type(v) == "table" and k ~= "Scope" and not blacklist[v] then
					if v.AstType == "AssignmentStatement" and v.Lhs[1] == InstructionExpression then
						local cur = tonumber(v.Rhs[1].Value.Data)
						if cur and cur > minimum then
							v.Rhs[1].Value.Data = tostring(cur+offset)
						end
					elseif v.AstType == "BinopExpr" and v.Lhs == InstructionExpression and tonumber(v.Rhs.Value.Data) then
						local cur = tonumber(v.Rhs.Value.Data)
						if cur and cur > minimum then
							v.Rhs.Value.Data = tostring(cur+offset)
						end
					else
						OffsetInstructions(v, offset, minimum, blacklist)
					end
				end
			end
		end
		local function ExtendInstructions(BaseInstructions, NewInstructions)
			local offset = #BaseInstructions
			for i = 1,#NewInstructions do
				BaseInstructions[i+offset] = NewInstructions[i]
			end
			OffsetInstructions(NewInstructions, offset)
		end
		local function ForceGoToInstruction(t, old, new, blacklist) --DeepScan3 :):):):)
			blacklist = blacklist or {}
			blacklist[t] = true
			for k,v in next,t do
				if type(v) == "table" and k ~= "Scope" and not blacklist[v] then
					if v.AstType == "AssignmentStatement" and v.Lhs[1] == InstructionExpression then
						local Value = v.Rhs[1].Value
						if not old or Value.Data == tostring(old) then
							Value.Data = tostring(new)
						end
					else
						ForceGoToInstruction(v, old, new, blacklist)
					end
				end
			end
		end
		local function CollectInstructionsFromBody(Body)
			local CollectedInstructions = {}
			for _,Statement in ipairs(Body) do
				local index = #CollectedInstructions+1

				--Special ones
				if Statement.AstType == "LocalStatement" then
					Statement.AstType = "AssignmentStatement"
					Statement.Lhs = Statement.LocalList
					for i,Lhs in ipairs(Statement.Lhs) do
						Statement.Lhs[i] = {AstType="VarExpr", Name=Lhs.Name, Local={CanRename=true, Scope=BodyScope, Name=Lhs.Name}}
					end
					Statement.Rhs = Statement.InitList
					if #Statement.Rhs == 0 then
						Statement.Rhs = {{AstType="NilExpr"}}
					end
					Statement.InitList = nil
					Statement.LocalList = nil
					CollectedInstructions[index] = StandardProcedure(Statement, index)

				elseif Statement.AstType == "Function" and Statement.IsLocal then
					Statement.IsLocal = false
					local FuncName = Statement.Name.Name
					CollectedInstructions[index] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {{AstType="VarExpr", Name=FuncName, Local={CanRename=true, Scope=BodyScope, name=FuncName}}},
						Rhs = {Statement},
					}, index)

				elseif Statement.AstType == "ReturnStatement" then
					local out = StandardProcedure(Statement, index)
					out.Body.Body[2] = nil
					CollectedInstructions[index] = out

				elseif Statement.AstType == "BreakStatement"
					or Statement.AstType == "ContinueStatement" then
					--The loop that contains these statements will point them to the correct index itself
					local out = StandardProcedure(Statement, index, Statement.AstType)
					local outBody = out.Body.Body
					outBody[1] = outBody[2]
					outBody[2] = nil
					CollectedInstructions[index] = out

				elseif Statement.AstType == "IfStatement" then
					--CONSIDER: Full flattening (splitting each elseif into a new if else instruction) - this'll be very complex but very flat
					--CONSIDER: Improving the fake-else statement to skip over all instructions by directly pointing to the final instruction instead of allocating a dummy slot
					--(This is done in post-processing but doing it here is more efficient)
					local Clauses = Statement.Clauses
					CollectedInstructions[index] = {} --Reserve a slot
					local AllInstructions = {}
					local HasACatchAll = not Statement.Clauses[#Statement.Clauses].Condition
					if not HasACatchAll then
						Statement.Clauses[#Statement.Clauses+1] = {Body={AstType="Stalist", Scope=BodyScope, Body={}}} --this just works with no adjustment, /shrug
					end

					for i,Clause in next,Statement.Clauses do
						local SubInstructions = CollectInstructionsFromBody(Clause.Body.Body)
						Clause.Body.Body = {CreateInstructionPointer(#CollectedInstructions+1)}

						if #SubInstructions == 0 then
							local FakeInstruction = StandardProcedure({}, 1)
							FakeInstruction.Body.Body[1] = FakeInstruction.Body.Body[2]
							FakeInstruction.Body.Body[2] = nil
							SubInstructions[1] = FakeInstruction
						end

						for _,Instruction in next,SubInstructions do
							AllInstructions[#AllInstructions+1] = {Instruction, #SubInstructions+#CollectedInstructions+1}
						end

						ExtendInstructions(CollectedInstructions, SubInstructions)
					end
					local ExitInstruction = #CollectedInstructions+1
					for i = 1, #AllInstructions do --Ensure all final instructions from each subset exit at the end of the main if instruction
						ForceGoToInstruction(AllInstructions[i][1], AllInstructions[i][2], ExitInstruction)
					end

					local out = StandardProcedure(Statement, index)
					out.Body.Body[2] = nil
					CollectedInstructions[index] = out

				elseif Statement.AstType == "WhileStatement" then
					CollectedInstructions[index] = {} --Reserve a slot
					local SubInstructions = CollectInstructionsFromBody(Statement.Body.Body)
					ForceGoToInstruction(SubInstructions, #SubInstructions+1, 0) --Point the end towards where the starting if check will be (sorted by OffsetInstructions)
					ExtendInstructions(CollectedInstructions, SubInstructions)
					ForceGoToInstruction(SubInstructions, "BreakStatement", #CollectedInstructions+1) --Point break statements to beyond the loop
					ForceGoToInstruction(SubInstructions, "ContinueStatement", index) --Point continue statements to the loop's start

					local NewStatement = {
						AstType = "IfStatement",
						Clauses = {
							{Condition = Statement.Condition, Body = {AstType="Statlist", Body={CreateInstructionPointer(index+1)}}}, --Point to while body
							{Body = {AstType="Statlist", Body={CreateInstructionPointer(#CollectedInstructions+1)}}}, --Else, get out
						},
					}
					local out = StandardProcedure(NewStatement, index)
					out.Body.Body[2] = nil
					CollectedInstructions[index] = out

				elseif Statement.AstType == "RepeatStatement" then
					local SubInstructions = CollectInstructionsFromBody(Statement.Body.Body)
					--We don't point the instructions pointing to the end anywhere else since we are about to add a statement on the end to handle the loop end
					ExtendInstructions(CollectedInstructions, SubInstructions)
					local LoopEndHandler = #CollectedInstructions+1
					local ExitInstruction = #CollectedInstructions+2
					ForceGoToInstruction(SubInstructions, "BreakStatement", ExitInstruction) --Point break statements to beyond the loop
					ForceGoToInstruction(SubInstructions, "ContinueStatement", LoopEndHandler) --Point continue statements to the loop's end handler

					local NewStatement = {
						AstType = "IfStatement",
						Clauses = {
							{Condition = Statement.Condition, Body = {AstType="Statlist", Body={CreateInstructionPointer(ExitInstruction)}}}, --Point to exit
							{Body = {AstType="Statlist", Body={CreateInstructionPointer(index)}}}, --Else, keep going
						},
					}
					local out = StandardProcedure(NewStatement, LoopEndHandler)
					out.Body.Body[2] = nil
					CollectedInstructions[LoopEndHandler] = out

				elseif Statement.AstType == "DoStatement" then
					ExtendInstructions(CollectedInstructions, CollectInstructionsFromBody(Statement.Body.Body)) --cool one-liner (all variable stuff was handled step 1)

				elseif Statement.AstType == "NumericForStatement" then
					CollectedInstructions[index] = {} --Reserve 4 slots
					CollectedInstructions[index+1] = {}
					CollectedInstructions[index+2] = {}
					CollectedInstructions[index+3] = {}
					local SubInstructions = CollectInstructionsFromBody(Statement.Body.Body)
					--We don't point the instructions pointing to the end anywhere else since we are about to add a statement on the end to handle the loop end
					ExtendInstructions(CollectedInstructions, SubInstructions)
					local LoopEndHandler = #CollectedInstructions+1
					local ExitInstruction = #CollectedInstructions+2
					ForceGoToInstruction(SubInstructions, "BreakStatement", ExitInstruction) --Point break statements to beyond the loop
					ForceGoToInstruction(SubInstructions, "ContinueStatement", LoopEndHandler) --Point continue statements to the loop's end handler

					Statement.Variable = {AstType="VarExpr", Name=Statement.Variable.Name, Local=Statement.Variable}
					CollectedInstructions[index] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.DedicatedStart, Statement.DedicatedFinish, Statement.DedicatedStep},
						Rhs = {Statement.Start, Statement.End, Statement.Step or {AstType="NumberExpr", Value={Data="1"}}}
					}, index)
					CollectedInstructions[index+1] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.DedicatedVariable},
						Rhs = {Statement.DedicatedStart}
					}, index+1)
					CollectedInstructions[index+2] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.Variable},
						Rhs = {Statement.DedicatedVariable}
					}, index+2)
					local IfCheck = StandardProcedure({
						AstType = "IfStatement",
						Clauses = {
							{
								Condition = {AstType="BinopExpr", Op="or", Lhs={
									AstType="BinopExpr", Op="and", Lhs={
										AstType="BinopExpr", Op=">=", Lhs=Statement.DedicatedStep, Rhs={AstType="NumberExpr", Value={Data="0"}}
									}, Rhs = {
										AstType="BinopExpr", Op="<=", Lhs=Statement.Variable, Rhs=Statement.DedicatedFinish
									}
								}, Rhs={
									AstType="BinopExpr", Op="and", Lhs={
										AstType="BinopExpr", Op="<=", Lhs=Statement.DedicatedStep, Rhs={AstType="NumberExpr", Value={Data="0"}}
									}, Rhs = {
										AstType="BinopExpr", Op=">=", Lhs=Statement.Variable, Rhs=Statement.DedicatedFinish
									}
								}},
								Body = {AstType="Statlist", Body={CreateInstructionPointer(index+4)}} --Point to loop body
							},
							{Body = {AstType="Statlist", Body={CreateInstructionPointer(ExitInstruction)}}}, --Else, get out
						},
					}, index+3) --Step 3: check where we should go
					IfCheck.Body.Body[2] = nil
					CollectedInstructions[index+3] = IfCheck
					local LoopEnd = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.DedicatedVariable},
						Rhs = {{AstType="BinopExpr", Op="+", Lhs=Statement.DedicatedVariable, Rhs=Statement.DedicatedStep or {AstType="NumberExpr", Value={Data="1"}}}}
					}, LoopEndHandler) --Step Loop+1: Increment and go back to the check
					LoopEnd.Body.Body[2].Rhs[1].Value.Data = tostring(index+2)
					CollectedInstructions[LoopEndHandler] = LoopEnd

				elseif Statement.AstType == "GenericForStatement" then
					CollectedInstructions[index] = {} --Reserve 4 slots (yes, 4)
					CollectedInstructions[index+1] = {}
					CollectedInstructions[index+2] = {}
					CollectedInstructions[index+3] = {}
					local SubInstructions = CollectInstructionsFromBody(Statement.Body.Body)
					if #SubInstructions == 0 then
						local FakeInstruction = StandardProcedure({}, 1)
						FakeInstruction.Body.Body[1] = FakeInstruction.Body.Body[2]
						FakeInstruction.Body.Body[2] = nil
						SubInstructions[1] = FakeInstruction
					end
					ForceGoToInstruction(SubInstructions, #SubInstructions+1, -2) --Point the end towards where the 2nd reserved slot will be
					ExtendInstructions(CollectedInstructions, SubInstructions)
					local ExitInstruction = #CollectedInstructions+1
					ForceGoToInstruction(SubInstructions, "BreakStatement", ExitInstruction) --Point break statements to beyond the loop
					ForceGoToInstruction(SubInstructions, "ContinueStatement", index+1) --Point continue statements to the loop's end handler

					for i,Variable in next,Statement.VariableList do
						Statement.VariableList[i] = {AstType="VarExpr", Name=Variable.Name, Local=Variable}
					end
					CollectedInstructions[index] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.DedicatedGenerator, Statement.DedicatedObject, Statement.DedicatedKey},
						Rhs = Statement.Generators
					}, index)
					CollectedInstructions[index+1] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = Statement.VariableList,
						Rhs = {{AstType="CallExpr", Base=Statement.DedicatedGenerator, Arguments={Statement.DedicatedObject, Statement.DedicatedKey}}}
					}, index+1) --Step 2: set the standard variable to the dedicated variable
					local IfCheck = StandardProcedure({
						AstType = "IfStatement",
						Clauses = {
							{
								Condition = {AstType="BinopExpr", Op="~=", Lhs=Statement.VariableList[1], Rhs={AstType="NilExpr"}},
								Body = {AstType="Statlist", Body={CreateInstructionPointer(index+3)}} --Point to loop body
							},
							{Body = {AstType="Statlist", Body={CreateInstructionPointer(ExitInstruction)}}}, --Else, get out
						},
					}, index+2) --Step 3: check where we should go
					IfCheck.Body.Body[2] = nil
					CollectedInstructions[index+2] = IfCheck
					CollectedInstructions[index+3] = StandardProcedure({
						AstType = "AssignmentStatement",
						Lhs = {Statement.DedicatedKey},
						Rhs = {Statement.VariableList[1]}
					}, index+3)

				--Else, normal stuff
				else
					CollectedInstructions[index] = StandardProcedure(Statement, index)
				end
			end
			return CollectedInstructions
		end
		CollectedInstructions = CollectInstructionsFromBody(Body)

		--Step 3: Do some post-processing on the instructions
		--Clear dummy statements
		local DummiesCleared = 0
		for i = #CollectedInstructions, 1, -1 do
			local Instruction = CollectedInstructions[i]
			local InstructionBody = Instruction.Body.Body
			if #InstructionBody == 1 and InstructionBody[1].AstType == "AssignmentStatement" and InstructionBody[1].Lhs[1] == InstructionExpression then
				local DummyTarget = InstructionBody[1].Rhs[1].Value.Data
				table.remove(CollectedInstructions, i)
				ForceGoToInstruction(CollectedInstructions, i, DummyTarget) --Point anything that pointed to the dummy to the dummy's target instead
				OffsetInstructions(CollectedInstructions, -1, i) --Knock down by an index anything that was at this instruction or higher
				DummiesCleared = DummiesCleared + 1
			end
		end
		if DummiesCleared > 0 then
			print("[Flattener] Removed " .. DummiesCleared .. " dummy/proxy statements")
		end
		--Shuffle it
		--TODO: Actually change instruction numbers too for extra obscurity
		local ToShuffle = {}
		for a,b in next,CollectedInstructions do
			ToShuffle[a] = b
		end
		CollectedInstructions={}
		while #ToShuffle>0 do
			CollectedInstructions[#CollectedInstructions+1] = table.remove(ToShuffle, math.random(1, #ToShuffle))
		end

		--Wrap it up
		NewAST[2] = {
			AstType = "WhileStatement",
			Condition = {
				AstType = "BinopExpr",
				Op = "<=",
				Rhs = {AstType="NumberExpr", Value={Data=tostring(#CollectedInstructions)}},
				Lhs = InstructionExpression,
			},
			Body = {
				AstType = "Statlist",
				Scope = BodyScope,
				Body = {{
					AstType = "IfStatement",
					Clauses = CollectedInstructions
				}}
			}
		}

		--Done!
		return NewAST
	end
	ast.Body = PerformFlattening(ast)
end

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
local JunkStatements = {
	function()
		local var = GenerateRandomString()
		return "if " .. var .. " then " .. var .. "() end"
	end,
	function()
		return "local " .. GenerateRandomString()
	end,
	function()
		local arg = GenerateRandomString()
		return "local function " .. GenerateRandomString() .. "("..arg..",...) return "..arg.."(...) end"
	end,
	function()
		local arg = GenerateRandomString()
		local f1 = GenerateRandomString()
		local f2 = GenerateRandomString()
		return "local function "..GenerateRandomString().."("..arg..") local "..arg.."="..arg.." return "..f1.."("..arg..") or "..f2.."("..arg..") end"
	end,
	function()
		return "local " .. GenerateRandomString() .. " = " .. math.random(-10,10)
	end,
	function()
		return "local " .. GenerateRandomString() .. " = \"\""
	end,
	function()
		return "local " .. GenerateRandomString() .. " = {}"
	end,
	function()
		local var = GenerateRandomString()
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
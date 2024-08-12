local RewriterOptions

--[[ Apply Binary Search
Optimises the flattener's if structure
Instead of a huge chain of "==1 ==2 ==3 ==4", we use dividing <= checks
This reduces the total amount of checks performed for each clause from N to log2(MaxN), which is great for us
We already expect our list to be sorted on arrival
--]]
local function ApplyBinarySearch(BodyScope, ClauseList, Start, End)
	-- We expect a range size of 1, 2, or 3+
	local Range = End-Start + 1
	local InstructionExpression = ClauseList[1].Condition.Lhs
	if Range < 1 or Range%1 ~= 0 then
		error("Invalid input into ApplyBinarySearch")
	elseif Range == 1 then
		-- return just the body, no if statement
		return ClauseList[Start].Body.Body

	elseif Range == 2 then
		-- return a 2-piece if statement
		return {{
			AstType = "IfStatement",
			Clauses = {
				{
					Condition = ClauseList[Start].Condition,
					Body = ClauseList[Start].Body
				}, {
					Body = ClauseList[End].Body
				}
			}
		}}

	else
		-- get ApplyBinarySearch of top half and bottom half
		-- put into basic table
		-- Managing these scopes is gonna be, uh, nightmarish if we want aggressive renaming to work
		local MidPoint = math.floor((Start+End)/2)
		return {{
			AstType = "IfStatement",
			Clauses = {
				{
					Condition = {
						AstType="BinopExpr", Op="<=",
						Rhs={AstType="NumberExpr", Value={Data=tostring(MidPoint)}},
						Lhs=InstructionExpression,
					},
					-- we don't listify the return for the AST, we expect the return to do that for us (including us)
					Body = {AstType="Statlist", Scope=BodyScope, Body=ApplyBinarySearch(BodyScope, ClauseList, Start, MidPoint)}
				}, {
					Body = {AstType="Statlist", Scope=BodyScope, Body=ApplyBinarySearch(BodyScope, ClauseList, MidPoint+1, End)}
				}
			}
		}}
	end
end

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
This forces us to do some self-management
--]]
local function PerformFlattening(Body, FunctionVariables, FunctionDepth)
	local BodyScope = Body.Scope
	Body = Body.Body
	FunctionDepth = FunctionDepth or 0
	local NewAST = {}
	--Step 1: Variable collection and scope calculations
	local Variables = {}
	local InstructionPointer = "__ins" .. math.random(1e4,1e5-1) .. "__"
	local InstructionExpression = {AstType="VarExpr", Name=InstructionPointer, Local={CanRename=true, Scope=BodyScope, Name=InstructionPointer}}
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
							Variables[#Variables+1] = Name
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
							Variables[#Variables+1] = v.Local
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
							Variables[#Variables+1] = Local
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
						local KeyName = "_key__dedicated"
						local GenName = "_gen__dedicated"
						local ObjName = "_obj__dedicated"
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
							Variables[#Variables+1] = v
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
	local VariableObjects = {InstructionExpression.Local}
	for i,Variable in next,Variables do
		if not SeenVariables[Variable] then
			VariableObjects[#VariableObjects+1] = Variable
			SeenVariables[Variable] = true
		end
	end
	BodyScope.LocalsInOrder = VariableObjects
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
					if Value.Data == tostring(old) then
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
					Statement.Lhs[i] = {AstType="VarExpr", Name=Lhs.Name, Local=Lhs}
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
				local FuncName = Statement.Name
				CollectedInstructions[index] = StandardProcedure({
					AstType = "AssignmentStatement",
					Lhs = {{AstType="VarExpr", Name=FuncName.Name, Local=FuncName}},
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

	local MaxInstructionN = #CollectedInstructions

	if RewriterOptions.ApplyBinarySearch and #CollectedInstructions > 3 then
		table.sort(CollectedInstructions, function(a, b)
			a = tonumber(a.Condition.Rhs.Value.Data)
			b = tonumber(b.Condition.Rhs.Value.Data)
			return a < b --wild
		end)
		CollectedInstructions = ApplyBinarySearch(BodyScope, CollectedInstructions, 1, MaxInstructionN)[1].Clauses
	end

	--Wrap it up
	NewAST[2] = {
		AstType = "WhileStatement",
		Condition = {
			AstType = "BinopExpr",
			Op = "<=",
			Rhs = {AstType="NumberExpr", Value={Data=tostring(MaxInstructionN)}},
			Lhs = InstructionExpression,
		},
		Body = {
			AstType = "Statlist",
			Scope = BodyScope,
			Body = {MaxInstructionN > 0 and {
				AstType = "IfStatement",
				Clauses = CollectedInstructions
			} or nil}
		}
	}

	--Done!
	return NewAST
end

return function(TableObj, Options)
	RewriterOptions = Options
	if RewriterOptions.PerformCodeFlattening then
		return PerformFlattening(TableObj)
	else
		return TableObj.Body
	end
end
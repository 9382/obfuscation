local RewriterOptions

--== Standard helper functions ==--
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
local ProtectedNames = {["if"]=true, ["do"]=true, ["in"]=true, ["as"]=true, _G=true, ["or"]=true, os=true, io=true}
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
			if ProtectedNames[out] then -- safety
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

local function GenerateFirstAvailableName(ExistingNames)
	LastVar = {} --funny magic change
	local VarName = GenerateVariableName()
	while ExistingNames[VarName] do
		VarName = GenerateVariableName()
	end
	return VarName
end

--== Regular non-aggressive renaming ==--
local function RenameScopeVariables(Scope)
	for i,Local in next,Scope.LocalsInOrder do
		if Local.CanRename then
			Local.Name = GenerateVariableName(Local.Name)
			Local.CanRename = false --shouldn't have to do this but flattener is trying to fight me
			--theres a potential that LocalStatement/Function special handling is double-defining locals
		end
	end
	for _,child in next,Scope.Children do
		RenameScopeVariables(child)
	end
end

--== The aggressive renaming ==--
-- Part 1: Figuring out when locals get used/unused
local function GatherLocalsAndBodies(TableObj, LocalStorage, BodyStorage, blacklist)
	blacklist = blacklist or {}
	blacklist[TableObj] = true
	for a,b in next,TableObj do
		if type(b) == "table" then
			if b.CanRename ~= nil and a ~= "Variable" then -- Thank you, NumericForStatement!
				LocalStorage[#LocalStorage+1] = b
			elseif b.AstType == "Statlist" then
				BodyStorage[#BodyStorage+1] = b
			elseif
				not blacklist[b]
				and a ~= "Scope"
				-- Special cases with an intermediary scope, because life wasn't hard enough
				and not (TableObj.AstType == "Function" and a == "Arguments")
				and not (TableObj.AstType == "GenericForStatement" and a == "VariableList")
				and not (TableObj.AstType == "NumericForStatement" and a == "Variable") -- Not technically required
			then
				GatherLocalsAndBodies(b, LocalStorage, BodyStorage, blacklist)
			end
		end
	end
end

local function CompileLocalData(Statlist, AssociatedScope)
	local Scope = Statlist.Scope
	Scope.RenamingChildren = {}
	local LocalsToRename = {}
	if AssociatedScope then
		for _, Local in next, AssociatedScope.LocalsInOrder do
			if not Local.UseByScope then
				Local.UseByScope = {}
			end
			Local.UseByScope[Scope] = {First=1, Last=2}
			LocalsToRename[Local] = true
			-- This is technically somewhat risky code since this causes a disconnect between its real scope
			-- But since these are very specific situations it's not actually that concerning
			-- And it also technically won't invalidate most operations
			Local.Scope = Scope
		end
	end
	for i, Statement in ipairs(Statlist.Body) do
		if Statement.AstType == "RepeatStatement" then -- hacky fix for the "until" part of a repeat statement
			local CombinedBody = {}
			for _, SubStatement in next,Statement.Body.Body do
				CombinedBody[#CombinedBody+1] = SubStatement
			end
			CombinedBody[#CombinedBody+1] = Statement.Condition
			Statement = {AstType="RepeatStatement", Body={Body=CombinedBody, AstType="Statlist", Scope=Statement.Body.Scope}}
			-- The actual format of it doesn't have to be correct (mostly), it just needs to have the relevant data
		end
		Scope.CurrentStatementN = i
		local Locals, Bodies = {}, {}
		GatherLocalsAndBodies(Statement, Locals, Bodies)
		for j, Local in ipairs(Locals) do
			-- print("Local", i, j, Local.Name)

			if not Local.UseByScope then
				Local.UseByScope = {}
			end
			if not Local.UseByScope[Scope] then
				Local.UseByScope[Scope] = {First=i, Last=i+1} --never finish where we start, could end poorly
			elseif i > Local.UseByScope[Scope].Last then
				Local.UseByScope[Scope].Last = i
			end

			if Local.Scope == Scope then -- our property, we must rename it at the end
				LocalsToRename[Local] = true
			elseif Scope.Upvalues[Local] then -- property of something above us, update last used for all above
				local ParentToAdjust = Scope
				repeat
					ParentToAdjust = ParentToAdjust.Parent
					local ParentN = ParentToAdjust.CurrentStatementN
					if ParentN then
						if Local.UseByScope[ParentToAdjust] then
							if ParentN > Local.UseByScope[ParentToAdjust].Last then
								Local.UseByScope[ParentToAdjust].Last = ParentN
							end
						else
							Local.UseByScope[ParentToAdjust] = {First=1, Last=math.max(ParentN, 2)}
						end
					end
				until ParentToAdjust == Local.Scope
			else
				error("Impossible local (mistreatment of associated scopes)")
			end
		end
		for _, Body in ipairs(Bodies) do
			-- If the scope's parent isn't us, then we have an intermediary scope
			-- This appears in the case of a Function's args and the variables for (Numeric/Generic)ForLoop
			-- We assign the responsibility of these intermediary locals to the sub-body
			CompileLocalData(Body, Body.Scope.Parent ~= Scope and Body.Scope.Parent)
			Scope.RenamingChildren[#Scope.RenamingChildren+1] = Body
		end
	end
	Scope.LocalsToRename = LocalsToRename
end

-- Part 2: Renaming based on the collected data
local function AggressivelyRenameLocals(Statlist)
	local Scope = Statlist.Scope
	local LocalsToRename = Scope.LocalsToRename
	local UsedNames = {}
	-- get the relevant data for our specific scope
	local ExpectedUpvalues = {}
	for Local, _ in next, Scope.Upvalues do
		if Local.UseByScope[Scope] and not LocalsToRename[Local] then
			ExpectedUpvalues[#ExpectedUpvalues+1] = Local
			UsedNames[Local.Name] = true
		end
	end
	-- magic begins here
	local InUseNames = {}
	for i, Statement in ipairs(Statlist.Body) do
		-- Technically a local Function is actually 2 statements condensed (local x; x = function() end)
		-- If we change too early, we create invalidating code
		-- For this reason, we delay until after renaming every time we see such a case
		local ToName = {}
		local ToRemove = {}
		for _, Local in next, ExpectedUpvalues do
			if Local.UseByScope[Scope].Last == i then
				if Statement.AstType == "Function" then
					ToRemove[#ToRemove+1] = Local
				else
					UsedNames[Local.Name] = false
				end
			end
		end
		for Local, _ in next, Scope.LocalsToRename do
			if Local.UseByScope[Scope].First == Local.UseByScope[Scope].Last then
				error("Bad variable range overwrite")
			end
			if Local.UseByScope[Scope].First == i then
				if Local.CanRename then
					ToName[#ToName+1] = Local
				else
					UsedNames[Local.Name] = true
				end
			elseif Local.UseByScope[Scope].Last == i and not Local.CanRename then
				if Statement.AstType == "Function" then
					ToRemove[#ToRemove+1] = Local
				else
					UsedNames[Local.Name] = false
				end
			end
		end
		for _, Local in next, ToName do -- in 2 parts since we want to remove all now-unused names first
			local NewName = GenerateFirstAvailableName(UsedNames)
			UsedNames[NewName] = true
			Local.Name = NewName
			Local.CanRename = false
		end
		for _, Local in next, ToRemove do
			UsedNames[Local.Name] = false
		end
	end
	-- and now the subscopes
	for _,Child in next,Scope.RenamingChildren do
		AggressivelyRenameLocals(Child)
	end
end

--== Entry point ==--
return function(AST, Options)
	RewriterOptions = Options
	if RewriterOptions.MinifyVariableNames and RewriterOptions.AggressivelyMinifyVariables then
		CompileLocalData(AST)
		AggressivelyRenameLocals(AST)
	elseif RewriterOptions.ObscureVariableNames or RewriterOptions.MinifyVariableNames then
		RenameScopeVariables(AST.Scope)
	end
end
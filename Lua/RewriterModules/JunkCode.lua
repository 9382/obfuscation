local ParseLua = require("../LuaParser")
local RewriterOptions

local _ValidCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
local function GenerateRandomString(P1, P2)
	if P2 then --Min, Max
		P1 = P2 - math.random(0, P2-P1)
	elseif P1 then --Exact
		-- Do nothing
	else --No input, use default
		P1 = math.random(5, 6)
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

--[[
We can write junk statements in any formatting style we want
since we use ParseLua to make it into reliable AST data
--]]
local JunkStatements = {
	-- function()
	-- 	local var = GenerateRandomString()
	-- 	return "local " .. var .. "; if " .. var .. " then " .. var .. "() end"
	-- end, -- doesnt work; 1 statement limit
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
		return "local function "..GenerateRandomString().."("..arg..", "..f1..", "..f2..") local "..arg.."="..arg.." return "..f1.."("..arg..") or "..f2.."("..arg..") end"
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
	-- function()
	-- 	local var = GenerateRandomString()
	-- 	return "local " .. var .. " = function() end; while " .. var .. " do " .. var .. " = " .. var .. "() end"
	-- end, -- doesnt work; 1 statement limit
}
local function GenerateJunkCode(Scope)
	local s,r = ParseLua(JunkStatements[math.random(1,#JunkStatements)]())
	for _, Local in next, r.Scope.LocalsInOrder do
		Local.Scope = Scope
		Scope.LocalsInOrder[#Scope.LocalsInOrder+1] = Local
	end
	for _, Child in next, r.Scope.Children do
		Child.Parent = Scope
		Scope.Children[#Scope.Children+1] = Child
	end
	r.Scope = Scope
	r.Body[1].IsJunk = true
	return r.Body[1]
end

local function InsertJunkCode(TableObj, blacklist)
	blacklist = blacklist or {}
	if blacklist[TableObj] then
		return
	end
	blacklist[TableObj] = true
	if TableObj.AstType == "Statlist" then
		local Scope = TableObj.Scope
		TableObj = TableObj.Body
		local LastStatement = #TableObj > 0 and TableObj[#TableObj].AstType
		local IterationStart = #TableObj+1
		if LastStatement == "ReturnStatement" or LastStatement == "BreakStatement" or LastStatement == "ContinueStatement" then
			IterationStart = #TableObj --else we may produce non-compiling code
		end
		for i = IterationStart, 1, -1 do
			if math.random() < RewriterOptions.JunkCodeChance then
				table.insert(TableObj, i, GenerateJunkCode(Scope))
			end
		end
	end
	for a,b in next,TableObj do
		if type(b) == "table" and a ~= "Scope" and (RewriterOptions.AllowNestedJunkCode or not b.IsJunk) then
			InsertJunkCode(b, blacklist)
		end
	end
end

return function(TableObj, Options)
	RewriterOptions = Options
	if Options.AddJunkCode then
		InsertJunkCode(TableObj)
	end
end
local ParseLua = require("./LuaParser")

-- This script is the serializer used to turn lua code into a string that can be decoded by Executor.lua
-- The code to be serialized goes at the very bottom

local _seed = os.time()
math.randomseed(_seed)
print("Using seed", _seed)

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
	if t.Parsed ~= nil then --Override string forms with their parsed version
		AssignKey(t,"Parsed",0)
		t.Constant = nil
		t.Data = nil
	elseif t.Constant ~= nil then --...or the version without quotes in case of long strings
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
				deepModify(b.Body)
				t[a] = b.Body
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

	deepModify(p.Body, true)
	local output = serializer(p.Body)

	io.open("Serializer output.lua", "w+"):write(output)
	return true, output
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
	return true, serializer(p.Body)
end
local ParseLua = require("./LuaParser")
local LuaWriter = require("./RewriterModules/LuaWriter")
local PerformFlattening = require("./RewriterModules/FlattenAST")
local InsertJunkCode = require("./RewriterModules/JunkCode")
local RenameVariables = require("./RewriterModules/VariableRenamer")

-- A lua code rewriter. The lua code to be rewritten goes at the very bottom

local _seed = os.time()
math.randomseed(_seed)
print("Using seed", _seed)

local RewriterOptions = {
	--== IndentCharacter ==--
	-- The character used for indenting
	-- Ignored if code is one-lined
	IndentCharacter = "\t",

	--== UseNewlines ==--
	-- Whether or not the output should be neatly formatted with newlines
	UseNewlines = true,

	--== UseSemicolons ==--
	-- Whether or not to use a semicolon at the end of each expression
	-- Could help fix "ambiguous syntax" issues
	-- Recommended to use if UseNewlines is off to avoid potential problems
	UseSemicolons = false,

	--== AddExtraSpacing ==--
	-- Adds extra spacing in commas and expressions (E.g. ", " vs ",")
	-- Warning: Turning this option off often produces failing code :/
	AddExtraSpacing = true,

	--== ObscureVariableNames ==--
	-- Replaces local variable names with complete garbage. The variable must be localised
	ObscureVariableNames = false,

	--== MinifyVariableNames ==--
	-- Replaces local variable names with minified strings. The variable must be localised. Takes priority over ObscureVariableNames
	MinifyVariableNames = true,

	--== AggressivelyMinifyVariableNames ==--
	-- Allows the minifier to re-use variable names the moment the variable is gauranteed to be unused
	-- Except to see frequent repetition of variable names, including in the same scope
	AggressivelyMinifyVariables = false,

	--== ObscureNumbers / ObscureStrings / ObscureGlobals ==--
	-- Turns normal constants into a complex form
	-- ObscureGlobals will only obscure globals normally found in _G (E.g. print or table)
	-- It will not effect script-made globals for the sake of getfenv()
	ObscureNumbers = false,
	ObscureStrings = false, --NOT YET IMPLEMENTED
	ObscureGlobals = false, --NOT YET IMPLEMENTED

	--== UseShortCallExprs ==--
	-- This turns statements like print("Test") into print"Test"
	UseShortCallExprs = false,

	--== AddJunkCode ==--
	-- Adds code that serves no purpose functionally and is just there to obscure the script visually
	AddJunkCode = false,

	--== JunkCodeChance ==--
	-- The chance at each statement that junk code is added if enabled
	JunkCodeChance = 1/4,

	--== AllowNestedJunkCode ==--
	-- Whether or not junk code should be generated inside junk code
	-- Warning: Junk code may become too much if this is enabled and JunkCodeChance is too high
	AllowNestedJunkCode = true,

	--== PerformCodeFlattening ==--
	-- Performs code flattening to help obscure the normal flow of the function
	PerformCodeFlattening = false,
}

local function Main(C)
	local s,p = ParseLua(C)
	if not s then
		print("Failed to parse the lua - "..p)
		return false,p
	end

	if not RewriterOptions.UseNewlines and not RewriterOptions.UseSemicolons then
		print("WARNING: Semicolons should really be used when no newlines are present")
	end

	if RewriterOptions.MinifyVariableNames and RewriterOptions.AggressivelyMinifyVariables and RewriterOptions.PerformCodeFlattening then
		print("WARNING: Aggressive variable minification has little effect when flattening code")
	end

	if (RewriterOptions.MinifyVariableNames and RewriterOptions.AggressivelyMinifyVariables) and RewriterOptions.PerformCodeFlattening then
		error("ERROR: AggressivelyMinifyVariables is currently incompatible with AddJunkCode/PerformCodeFlattening")
	end

	if RewriterOptions.AddJunkCode then
		InsertJunkCode(p, RewriterOptions)
	end

	if RewriterOptions.PerformCodeFlattening then
		p.Body = PerformFlattening(p, RewriterOptions)
	end

	if RewriterOptions.ObscureVariableNames or RewriterOptions.MinifyVariableNames then
		RenameVariables(p, RewriterOptions)
	end

	local result = LuaWriter(p, RewriterOptions)
	return true, table.concat(result,"\n")
end

print(Main([==[
print("Test")
]==]))

return Main
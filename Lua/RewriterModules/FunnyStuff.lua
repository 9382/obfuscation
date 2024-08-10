local RewriterOptions

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

local function CenterAlignCode(str)
	local lines = StringSplit(str, "\n")
	local longestLine = 0
	for i = 1, #lines do
		lines[i] = lines[i]:gsub("^%s*(.-)%s*$", "%1")
		longestLine = math.max(longestLine, #lines[i])
	end
	longestLine = math.min(longestLine, 100)
	for i = 1, #lines do
		if lines[i] ~= "" then
			lines[i] = string.rep(" ", math.floor((longestLine-#lines[i])/2)) .. lines[i]
		end
	end
	return table.concat(lines, "\n")
end

return function(Output, Options)
	RewriterOptions = Options
	if RewriterOptions.CenterAlignedCode then
		Output = CenterAlignCode(Output)
	end
	return Output
end
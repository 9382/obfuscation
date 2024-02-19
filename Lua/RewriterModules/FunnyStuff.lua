local RewriterOptions

local function CenterAlignCode(lines)
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
end

return function(CodeLines, Options)
	RewriterOptions = Options
	if RewriterOptions.CenterAlignedCode then
		CenterAlignCode(CodeLines)
	end
end
# LuaObfuscator

These scripts allow any lua script to be parsed and turned into a string that can be executed in Lua. Main intention of the script is to function as an obfuscator. Note that there are *no* fancy tricks applied here, and it isn't incredibly difficult to reverse engineer a script obfuscated using this back into its original state. If it gets deobfuscated, your only solution is to just deal with it. Also, it is *really* slow, so that sucks too,

IMPORTANT: These scripts are designed for use in roblox / roblox studio, which uses luau. For this reason, the parser is not entirely faithful to Lua 5.1. Important (and only) differences to note are the inclusion of the `continue` keyword and the removal of labels (`::label::`, `goto label`). Syntax like `+=` from luau is currently *not* implemented (because I couldn't easily hack the parser to do so), and attempting to parse a script using it will lead to an error.

## Info

Some more info on each of the files

### Parser.lua

This script is, as hinted from the top comments, actually a fork of [stravant's LuaMinify](https://github.com/stravant/LuaMinify/tree/master/RobloxPlugin), with minor changes to accomodate for some differences in Luau. My main modifications to the script here is the bit at the bottom. The serializer and bitwriter are just unmodified cut-outs from a previous project, shoddily chucked in for simplicity. I'll probably simplify it down like I did in the Executor at some point, but for now, nope.

Important to note in this is the simplifying of the table into purely numerical (or boolean) format, minimising the use of strings. Yes, this means the executor has very obscure references everywhere. It was originally developed using proper string references, but that version fell behind after long enough, so now there only exists the numerical version. I *may* try revive/recreate the stringed version for the sake of readability, but I wouldn't count on it.

Since the script was designed for roblox studio, it just outputs the result by printing it. Lovely, I know.

### Executor.lua

This script is purely made by me, and parses the output given by Parser.lua. It attempts to follow as much behaviour as close as possible to lua implementation (even including the stupid [truncation logic](https://www.lua.org/manual/5.1/manual.html#2.5)). Note that, while I don't *believe* there are any flaws in the executor, I can't give any guarantee, and there is a (hopefully very small) chance any script will error. The string parser is simply, once again, a stripped down version of a serializer and bitwriter from an old project, but optimised specifically for this to get over some complications with execution speed and string size limits. Lot of important comments in here, so if you want to understand the flow, do give this a proper read.

#### ExecutorMinified.lua

A minified version of the executor, specifically used for the idea of obfuscation purposes. Pop the execution string in the `[=[]=]`. Same as the Executor, so nothing to really say here.

### Executor return theory.txt

Not important to the code at all, but it is a nice insight into my through process about how to handle the Return statement through all the conditions there are. Created because so many just-code-and-hope attempts to make the system failed and I decided I needed to actually sit down and plan ahead or it wasn't getting made (yeah, thats right, nearly none of this was planned. God knows how it works).

### Rewriter.lua

This script does what the name implies and rewrites the given lua code using certain settings. As far as I'm aware it should rewrite everything perfectly( enough) assuming the settings arent garbage, and so should be safe to use.
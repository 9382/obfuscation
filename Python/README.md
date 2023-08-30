# PythonObfuscator (Unfinished README)

This follows the same idea as LuaObfuscator, which is to be able to parse and execute a string as python code *without* using exec. Note that this module is in progress and is not in a finished state by any standard, and versions can expected to be broken or unfinished

The code for this is designed to work for python 3.8. Future and past versions will likely continue to work, though certain features may be broken or unavailable

## Info

Some more info on each of the files

### ASTExecutor.py

This is the base how-to-run-it file, designed to parse something ran through python's `ast.parse`. There is no element of obfuscation here, and this is purely to get a start footing before attempting anything. The progress on elements being implemented are available in the script, which lists the implementation status of different expressions and statuses.
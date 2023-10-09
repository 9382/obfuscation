import random
import sys
import ast

_DEBUG = False
def debugprint(*args, **kwargs):
	if _DEBUG:
		print("[Debug]", *args, **kwargs)
if not _DEBUG:
	print("[Debug] debugprint is disabled")

# The indentation character used. Generally either a tab or spaces will be the input
OPTION_indent_char = "\t"

# Attempts to ""obfuscate"" numbers by abstracting them. Slightly messy looking
OPTION_obscure_numbers = False

# Attempts to re-name variables with random text. Tries to avoid changing anything that could be a concern, but it isn't perfect. Also INCREDIBLY messy
# Failures can be expected when using nonlocals/globals inside classes or using nonlocals weirdly
# Does not currently respect __all__ exports, and will rename them
OPTION_obscure_variables = True

# Same as obscure_variables, but instead uses as few characters as it can. Same limitations/issues apply (UNIMPLEMENTED)
OPTION_minimise_variables = False

# Occassionally inserts a bit of garbage (code that does, quite literally, nothing)
OPTION_insert_junk = True

# Attempts to figure out when if elif statements are used instead of exponentially indenting else statements
OPTION_use_elif = False

# Uses brackets in places where they are probably excessive, but could be worth having to be on the safe side
OPTION_extra_brackets = False

# Attempts to determine when a doc string is present and excludes it from the output
OPTION_ignore_docstrings = True

""" Progress Report

== Statements ==
Name				Status				Extra notes

FunctionDef			Implemented
AsyncFunctionDef	Implemented
ClassDef			Implemented
Return				Implemented

Delete				Implemented
Assign				Implemented
AugAssign			Implemented
AnnAssign			Implemented

For					Implemented
AsyncFor			Implemented
While				Implemented
If					Implemented
With				Implemented
AsyncWith			Implemented

Match				Not implemented		Not something I have significant personal experience with, so going to hold off for now

Raise				Implemented
Try					Implemented
Assert				Implemented

Import				Implemented
ImportFrom			Implemented

Global				Implemented
Nonlocal			Implemented
Expr				Implemented			I guess it's technically not implemented until all expressions are too, but uh, oh well
Pass				Implemented
Break				Implemented
Continue			Implemented


== Expressions ==
Name				Status				Extra notes

BoolOp				Implemented
NamedExpr			Implemented
BinOp				Implemented
UnaryOp				Implemented
Lambda				Implemented
IfExp				Implemented			Untested on tricky scenarios
Dict				Implemented
Set					Implemented
ListComp			Implemented
SetComp				Implemented
DictComp			Implemented
GeneratorExp		Implemented

Await				Implemented
Yield				Implemented
YieldFrom			Implemented

Compare				Implemented
Call				Implemented
FormattedValue		Implemented
JoinedStr			Implemented			Not entirely confident this is implemented well, but it works on simpler cases at least
Constant			Implemented

Attribute			Implemented
Subscript			Implemented
Starred				Implemented
Name				Implemented
List				Implemented
Tuple				Implemented

Slice				Implemented
Index				Implemented
ExtSlice			Not implemented		Removed after py3.8 (or at least changed). No idea what it actually is cause no damn example is given

== Notes ==
Quite a few areas like Call or Attribute need to be careful on when deciding to place () around their reference, since sometimes not doing so can be a terrible move (E.g. lambdas)

The script can't handle class-level nonlocals well, as it attempts to keep the name while also having no correct reference. I am considering this a neccessary evil.
The alternative would be to evaluate the entire statement list and then go back at the end, and the system as-is can NOT handle that.
"""

def CreateExecutionLoop(code):
	import builtins
	class VariableScope:
		def __init__(self, Parent, scopeType):
			self.Parent = Parent
			self.scopeType = scopeType
			self.Globals = set()
			self.NonLocals = set()
			self.VarMapping = {}
		def getVar(self, var, *, fromChild=False):
			out = self._getVar(var, fromChild=fromChild)
			if not fromChild and type(out) == tuple:
				return out[0]
			else:
				return out
		def _getVar(self, var, *, fromChild=False):
			debugprint("Asked to retrieve variable",var)
			# The order of this is very messy, changes a lot, and is mostly guess work
			# But, uh, this makes sense, right?
			if self.scopeType == "class" and not fromChild:
				return var, True
			if var in self.VarMapping:
				return self.VarMapping[var], True
			if self.Parent:
				#damn import *
				newVar, exists = self.Parent.getVar(var, fromChild=True)
				if exists:
					return newVar, True
			if hasattr(builtins, var):
				return var, True
			else:
				return var, False
		def createVar(self, var):
			if not OPTION_obscure_variables:
				return str(var)
			else:
				if var in self.VarMapping:
					return self.VarMapping[var]
				elif hasattr(builtins, var):
					self.VarMapping[var] = var
					return var
				else:
					if self.scopeType == "class":
						newName = var
					else:
						newName = GenerateRandomStr()
					self.VarMapping[var] = newName
					return newName
		def deleteVar(self, var):
			debugprint("Asked to delete variable",var)
			if self.scopeType == "asclause":
				self.Parent.deleteVar(var)
			if var in self.VarMapping:
				self.VarMapping.pop(var)
			else:
				if var in self.NonLocals or var in self.Globals:
					#The nonlocal and global state will persist beyond deletion, so DONT clear those
					self.Parent.deleteVar(var)
		def triggerGlobal(self, var):
			if self.scopeType != "core":
				self.VarMapping[var] = self.Parent.getVar(var)
				self.Globals.add(var)
				self.Parent.triggerGlobal(var)
		def triggerNonlocal(self, var):
			if self.scopeType == "core":
				raise SyntaxError("nonlocal declaration not allowed at module level")
			self.VarMapping[var] = self.Parent.getVar(var)
			self.NonLocals.add(var)

	class ReturnStatement:
		def __init__(self, Type, Data=None):
			self.Type = Type
			self.Data = Data

	class ExecutorException(Exception):
		pass

	_RandomCharacters = ["_"]
	for i in range(65, 91):
		_RandomCharacters.append(chr(i))
	for i in range(97, 123):
		_RandomCharacters.append(chr(i))
	def GenerateRandomStr(length=None):
		if not length:
			length = random.randint(20,40)
		randomStr = ""
		for i in range(length):
			randomStr = randomStr + random.choice(_RandomCharacters)
		return randomStr

	def WrapInQuotes(strobject):
		# TODO: Not convinced we are treating \ correctly
		# Need to figure out how the input is treated and manage it accordingly
		newString = ""
		canDoDouble = True
		ignoreNext = False
		if '"' in strobject:
			canDoDouble = False
		for char in strobject:
			if ignoreNext:
				newString = newString + char
				ignoreNext = False
				continue
			if (canDoDouble and char == '"') or (not canDoDouble and char == "'"):
				newString = newString + '\\' + char
			else:
				newString = newString + char
				if char == "\\":
					ignoreNext = True
					newString = newString + char
		if canDoDouble:
			return f'"{newString}"'
		else:
			return f"'{newString}'"

	def MangleNumber(number):
		if not OPTION_obscure_numbers:
			return str(number)
		method = random.randint(0, 3)
		if method == 0:
			offset = random.randint(-100, 100)
			return f"({number-offset}+{offset})"
		elif method == 1:
			offset = random.randint(1, 20)
			return f"({number-offset}+len(('{GenerateRandomStr(offset)}')))"
		elif method == 2:
			offset = random.randint(1, 20)
			return f"({number+offset}+-len(('{GenerateRandomStr(offset)}')))"
		elif method == 3: #Method 1/2 but abuses tuples
			return f"({number-1}+len(('{GenerateRandomStr()}',)))"

	def ParseOperator(op):
		op = type(op)
		#Boolean operations (and/or) are not supported and are handled just in the BoolOp expr
		#UnaryOp
		if op == ast.Invert:
			return "~"
		elif op == ast.Not:
			return "not "
		elif op == ast.UAdd:
			return "+"
		elif op == ast.USub:
			return "-"
		#BinaryOp
		elif op == ast.Add:
			return "+"
		elif op == ast.Sub:
			return "-"
		elif op == ast.Mult:
			return "*"
		elif op == ast.MatMult:
			return "@"
		elif op == ast.Div:
			return "/"
		elif op == ast.Mod:
			return "%"
		elif op == ast.Pow:
			return "**"
		elif op == ast.LShift:
			return "<<"
		elif op == ast.RShift:
			return ">>"
		elif op == ast.BitOr:
			return "|"
		elif op == ast.BitXor:
			return "^"
		elif op == ast.BitAnd:
			return "&"
		elif op == ast.FloorDiv:
			return "//"
		#Compare
		elif op == ast.Eq:
			return "=="
		elif op == ast.NotEq:
			return "!="
		elif op == ast.Lt:
			return "<"
		elif op == ast.LtE:
			return "<="
		elif op == ast.Gt:
			return ">"
		elif op == ast.GtE:
			return ">="
		elif op == ast.Is:
			return "is"
		elif op == ast.IsNot:
			return "is not"
		elif op == ast.In:
			return "in"
		elif op == ast.NotIn:
			return "not in"
		#None of the above
		else:
			raise ExecutorException(f"Unrecognised operator type '{op}'")

	_DEBUG_LastExpr = None
	_DEBUG_LastStatement = None

	def ExecuteExpression(expr, scope, *, ShouldWrap=True, ShouldObscureKeyword=True, GiveDetailedInfo=False):
		nonlocal _DEBUG_LastExpr
		_DEBUG_LastExpr = expr
		exprType = type(expr)
		debugprint("Executing expression...",exprType)

		if exprType == ast.Constant:
			if type(expr.value) == str:
				if ShouldWrap:
					out = WrapInQuotes(expr.value).replace("\n","\\n")
					if OPTION_insert_junk:
						return out + "[::]"
					else:
						return out
				else:
					expr.value.replace("\n","\\n")
			elif type(expr.value) == int or type(expr.value) == float:
				return MangleNumber(expr.value)
			return str(expr.value)

		elif exprType == ast.Name:
			scopemethod = (type(expr.ctx) == ast.Store) and scope.createVar or scope.getVar
			if GiveDetailedInfo:
				out = scopemethod(expr.id)
				return out, out != expr.id
			else:
				return scopemethod(expr.id)

		elif exprType == ast.NamedExpr:
			target, value = ExecuteExpression(expr.target, scope), ExecuteExpression(expr.value, scope)
			return f"({target} := {value})"

		elif exprType == ast.Starred:
			return f"*{ExecuteExpression(expr.value, scope)}"

		elif exprType == ast.Attribute:
			if type(expr.value) in [ast.Name, ast.Attribute]:
				return f"{ExecuteExpression(expr.value, scope)}.{expr.attr}"
			else:
				return f"({ExecuteExpression(expr.value, scope)}).{expr.attr}"

		elif exprType == ast.JoinedStr:
			out = "".join(ExecuteExpression(value, scope, ShouldWrap=False) for value in expr.values)
			if ShouldWrap:
				return f"f{WrapInQuotes(out)}"
			else:
				return out

		elif exprType == ast.FormattedValue:
			value = ExecuteExpression(expr.value, scope)
			conversion = ""
			if expr.conversion == 115:
				conversion = "!s"
			elif expr.conversion == 114:
				conversion = "!r"
			elif expr.conversion == 97:
				conversion = "!a"
			if expr.format_spec:
				spec = f":{ExecuteExpression(expr.format_spec, scope, ShouldWrap=False)}"
			else:
				spec = ""
			return f"{{{value}{conversion}{spec}}}"

		elif exprType == ast.keyword:
			value = ExecuteExpression(expr.value, scope)
			if ShouldObscureKeyword:
				if expr.arg:
					return f"{scope.getVar(expr.arg)}={value}"
				else:
					return f"**{scope.getVar(value)}"
			else:
				if expr.arg:
					return f"{expr.arg}={value}"
				else:
					return f"**{value}"

		elif exprType == ast.alias:
			if expr.asname:
				return f"{expr.name} as {scope.createVar(expr.asname)}"
			else:
				if OPTION_obscure_variables and expr.name != "*":
					return f"{expr.name} as {scope.createVar(expr.name)}"
				else:
					return f"{expr.name}"

		elif exprType == ast.withitem:
			return f"{ExecuteExpression(expr.context_expr, scope)} as {ExecuteExpression(expr.optional_vars, scope)}"

		elif exprType in [ast.Tuple, ast.List, ast.Set]:
			out = []
			for entry in expr.elts:
				out.append(ExecuteExpression(entry, scope))
			out = ", ".join(out)
			if exprType == ast.Tuple:
				return f"({out})"
			elif exprType == ast.List:
				return f"[{out}]"
			elif exprType == ast.Set:
				return f"{{{out}}}"

		elif exprType == ast.Dict:
			pairs = []
			for i in range(len(expr.keys)):
				key, value = expr.keys[i], expr.values[i]
				if key == None: #value is a dict that needs unpacking
					pairs.append(f"**{ExecuteExpression(value, scope)}")
				else:
					pairs.append(f"{ExecuteExpression(key, scope)}: {ExecuteExpression(value, scope)}")
			return f"{{{', '.join(pairs)}}}"

		elif exprType in [ast.ListComp, ast.SetComp, ast.GeneratorExp]:
			subScope = VariableScope(scope, "generator")
			generators = ParseGenerators(expr.generators, subScope)
			term = ExecuteExpression(expr.elt, subScope)
			if exprType == ast.ListComp:
				return f"[{term} {generators}]"
			elif exprType == ast.SetComp:
				return f"{{{term} {generators}}}"
			elif exprType == ast.GeneratorExp:
				return f"({term} {generators})"

		elif exprType == ast.DictComp:
			subScope = VariableScope(scope, "generator")
			generators = ParseGenerators(expr.generators, subScope)
			key = ExecuteExpression(expr.key, subScope)
			value = ExecuteExpression(expr.value, subScope)
			return f"{{{key}: {value} {generators}}}"

		elif exprType == ast.Index:
			return ExecuteExpression(expr.value, scope)

		elif exprType == ast.Slice:
			lower = expr.lower and ExecuteExpression(expr.lower, scope) or ""
			upper = expr.upper and ExecuteExpression(expr.upper, scope) or ""
			step = expr.step and ExecuteExpression(expr.step, scope) or ""
			return f"{lower}:{upper}:{step}"

		elif exprType == ast.Subscript:
			value = ExecuteExpression(expr.value, scope)
			Slice = ExecuteExpression(expr.slice, scope)
			return f"{value}[{Slice}]"

		elif exprType == ast.BoolOp:
			joiner = (type(expr.op) == ast.And) and " and " or " or " #and and or or
			if OPTION_extra_brackets:
				return joiner.join(f"({ExecuteExpression(subExpr, scope)})" for subExpr in expr.values)
			else:
				return joiner.join(f"{ExecuteExpression(subExpr, scope)}" for subExpr in expr.values)

		elif exprType == ast.UnaryOp:
			op = ParseOperator(expr.op)
			operand = ExecuteExpression(expr.operand, scope)
			return f"({op}{operand})"

		elif exprType == ast.BinOp:
			Lhs = ExecuteExpression(expr.left, scope)
			op = ParseOperator(expr.op)
			Rhs = ExecuteExpression(expr.right, scope)
			return f"({Lhs} {op} {Rhs})"

		elif exprType == ast.Compare:
			comparison = ExecuteExpression(expr.left, scope)
			for i in range(len(expr.ops)):
				op, nextValue = ParseOperator(expr.ops[i]), ExecuteExpression(expr.comparators[i], scope)
				comparison += f" {op} {nextValue}"
			return f"({comparison})"

		elif exprType == ast.IfExp:
			return f"({ExecuteExpression(expr.body, scope)}) if ({ExecuteExpression(expr.test, scope)}) else ({ExecuteExpression(expr.orelse, scope)})"

		elif exprType == ast.Call:
			if type(expr.func) in [ast.Name, ast.Attribute]:
				info = ExecuteExpression(expr.func, scope, GiveDetailedInfo=True)
				if type(info) == tuple:
					func, hadChange = info
				else:
					func = info
					hadChange = False
			else:
				func = f"({ExecuteExpression(expr.func, scope)})"
			arguments = []
			for arg in expr.args:
				arguments.append(ExecuteExpression(arg, scope))
			for kwarg in expr.keywords:
				arguments.append(ExecuteExpression(kwarg, scope, ShouldObscureKeyword=hadChange))
			arguments = ", ".join(arguments)
			return f"{func}({arguments})"

		elif exprType == ast.Await:
			return f"await {ExecuteExpression(expr.value, scope)}"

		elif exprType == ast.Lambda:
			subScope = VariableScope(scope, "lambda")
			args = HandleArgs(subScope, expr.args)
			body = ExecuteExpression(expr.body, subScope)
			return f"lambda {args}: {body}"

		elif exprType == ast.Yield:
			if expr.value:
				return f"yield {ExecuteExpression(expr.value, scope)}"
			else:
				return "yield"

		elif exprType == ast.YieldFrom:
			return f"yield from {ExecuteExpression(expr.value, scope)}"

		else:
			raise ExecutorException(f"[!] Unimplemented expression type {exprType}")

	def ExecuteStatement(statement, scope):
		nonlocal _DEBUG_LastStatement
		_DEBUG_LastStatement = statement
		stType = type(statement)
		debugprint("Executing statement...",stType)

		if stType == ast.Expr:
			if OPTION_ignore_docstrings and type(statement.value) == ast.Constant:
				return []
			return ExecuteExpression(statement.value, scope)

		elif stType == ast.Assign:
			target = ", ".join([ExecuteExpression(t, scope) for t in statement.targets])
			value = ExecuteExpression(statement.value, scope)
			return f"{target} = {value}"

		elif stType == ast.AnnAssign:
			target = ExecuteExpression(statement.target, scope)
			if statement.value:
				value = ExecuteExpression(statement.value, scope)
				return f"{target} = {value}"
			else:
				return #Who cares?

		elif stType == ast.AugAssign:
			value = ExecuteExpression(statement.value, scope)
			target = ExecuteExpression(statement.target, scope)
			return f"{target} {ParseOperator(statement.op)}= {value}"

		elif stType == ast.Assert:
			test = ExecuteExpression(statement.test, scope)
			msg = statement.msg and ExecuteExpression(statement.msg, scope)
			if OPTION_extra_brackets:
				if msg:
					return f"assert ({test}), ({msg})"
				else:
					return f"assert ({test})" #There's like, no reasonable way the brackets are needed at this point, but you asked for them!
			else:
				if msg:
					return f"assert {test}, {msg}"
				else:
					return f"assert {test}"

		elif stType == ast.Raise:
			if statement.exc:
				if statement.cause:
					return f"raise {ExecuteExpression(statement.exc, scope)} from {ExecuteExpression(statement.cause, scope)}"
				return f"raise {ExecuteExpression(statement.exc, scope)}"
			return "raise"

		elif stType == ast.Global:
			for name in statement.names:
				scope.triggerGlobal(name)
			return f"global {', '.join(scope.getVar(name) for name in statement.names)}"

		elif stType == ast.Nonlocal:
			for name in statement.names:
				scope.triggerNonlocal(name)
			return f"nonlocal {', '.join(scope.getVar(name) for name in statement.names)}"

		elif stType == ast.Delete:
			for name in statement.targets:
				scope.deleteVar(name)
			return f"del {', '.join(ExecuteExpression(name, scope) for name in statement.targets)}"

		elif stType == ast.Return:
			if statement.value:
				return f"return {ExecuteExpression(statement.value, scope)}"
			else:
				return "return"

		elif stType == ast.Pass:
			return "pass"

		elif stType == ast.Break:
			return "break"

		elif stType == ast.Continue:
			return "continue"

		elif stType == ast.If:
			out = []
			out.append(f"if {ExecuteExpression(statement.test, scope)}:")
			out.extend(ExecuteStatList(statement.body, scope))
			if len(statement.orelse) > 0:
				if OPTION_use_elif and len(statement.orelse) == 1 and type((alternate := statement.orelse[0])) == ast.If:
					out.append(f"elif {ExecuteExpression(alternate.test, scope)}:")
					out.extend(ExecuteStatement(alternate, scope)[1:])
				else:
					out.append("else:")
					out.extend(ExecuteStatList(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType == ast.While:
			out = []
			out.append(f"while {ExecuteExpression(statement.test, scope)}:")
			out.extend(ExecuteStatList(statement.body, scope))
			if len(statement.orelse) > 0:
				out.append("else:")
				out.extend(ExecuteStatList(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType in [ast.For, ast.AsyncFor]:
			iterRange = ExecuteExpression(statement.iter, scope)
			target = ExecuteExpression(statement.target, scope)
			body = ExecuteStatList(statement.body, scope)
			orelse = ExecuteStatList(statement.orelse, scope)
			out = []
			out.append(f"{stType == ast.AsyncFor and 'async ' or ''}for {target} in {iterRange}:")
			out.extend(body)
			if orelse:
				out.append("else:")
				out.extend(orelse)
			elif OPTION_insert_junk:
				out.extend(["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType in [ast.With, ast.AsyncWith]:
			out = []
			out.append(f"{stType == ast.AsyncWith and 'async ' or ''}with {', '.join(ExecuteExpression(item, scope) for item in statement.items)}:")
			out.extend(ExecuteStatList(statement.body, scope))
			return out

		elif stType == ast.Try:
			out = []
			out.append("try:")
			out.extend(ExecuteStatList(statement.body, scope))
			for handler in statement.handlers:
				out.extend(ExecuteStatement(handler, scope))
			if len(statement.orelse) > 0:
				out.append("else:")
				out.extend(ExecuteStatList(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else:",f"{OPTION_indent_char}pass"])
			if len(statement.finalbody) > 0:
				out.append("finally:")
				out.extend(ExecuteStatList(statement.finalbody, scope))
			elif OPTION_insert_junk:
				out.extend(["finally:",f"{OPTION_indent_char}pass"])
			return out

		elif stType == ast.ExceptHandler:
			out = []
			if statement.type:
				typeText = ExecuteExpression(statement.type, scope)
				if statement.name:
					out.append(f"except {typeText} as {scope.createVar(statement.name)}:")
				else:
					out.append(f"except {typeText}:")
			else:
				out.append("except:")
			out.extend(ExecuteStatList(statement.body, scope))
			return out

		elif stType == ast.Import:
			return f"import {', '.join(ExecuteExpression(entry, scope) for entry in statement.names)}"

		elif stType == ast.ImportFrom:
			return f"from {'.'*statement.level}{statement.module} import {', '.join(ExecuteExpression(entry, scope) for entry in statement.names)}"

		elif stType == ast.FunctionDef or stType == ast.AsyncFunctionDef:
			subScope = VariableScope(scope, "function")
			out = []
			decorators = ImplementObjectDecorators(statement.decorator_list, scope)
			if scope.scopeType == "class":
				name = statement.name
			else:
				name = scope.createVar(statement.name)
			args = HandleArgs(subScope, statement.args)
			body = ExecuteStatList(statement.body, subScope)
			out.extend(decorators)
			if stType == ast.AsyncFunctionDef:
				out.append(f"async def {name}({args}):")
			else:
				out.append(f"def {name}({args}):")
			out.extend(body)
			return out

		elif stType == ast.ClassDef:
			subScope = VariableScope(scope, "class")
			out = []
			decorators = ImplementObjectDecorators(statement.decorator_list, scope)
			name = scope.createVar(statement.name)
			args = []
			for base in statement.bases:
				args.append(ExecuteExpression(base, scope))
			for keyword in statement.keywords:
				args.append(ExecuteExpression(keyword, scope))
			args = ", ".join(args)
			body = ExecuteStatList(statement.body, subScope)
			out.extend(decorators)
			out.append(f"class {name}({args}):")
			out.extend(body)
			return out

		else:
			raise ExecutorException(f"[!] Unimplemented statement type {stType}")
		raise ExecutorException(f"[!] Statement of type {stType} never returned")

	def ExecuteStatList(statList, scope, Indent=True):
		debugprint("Executing statement list...")
		compiledText = []
		for statement in statList:
			if type(statement) in [ast.FunctionDef, ast.AsyncFunctionDef]:
				scope.createVar(statement.name)
		for statement in statList:
			if OPTION_insert_junk and random.randint(1,4) == 1:
				out = ExecuteStatement(GenerateRandomJunk(), scope)
				if type(out) == list:
					compiledText.extend(out)
				elif type(out) == str:
					compiledText.append(out)
			out = ExecuteStatement(statement, scope)
			if type(out) == list:
				compiledText.extend(out)
			elif type(out) == str:
				compiledText.append(out)
			elif out == None:
				pass
			else:
				debugprint(":( poor type return to statlist",type(out))
				compiledText.append(str(out))
		if Indent:
			for i in range(len(compiledText)):
				compiledText[i] = f"{OPTION_indent_char}{compiledText[i]}"
		return compiledText

	#def f2(x, y, z=None, *, a, b, c=None, **k):
	#	print('Cool')
	def HandleArgs(scope, arguments):
		#Setup
		argString = []

		#Positionals
		debugprint("Positionals",arguments.args,arguments.defaults)
		defaultOffset = len(arguments.args)-len(arguments.defaults)
		for i in range(len(arguments.args)):
			arg = arguments.args[i]
			if i >= defaultOffset:
				default = ExecuteExpression(arguments.defaults[i-defaultOffset], scope)
				argString.append(f"{scope.getVar(arg.arg)}={default}")
			else:
				argString.append(f"{scope.getVar(arg.arg)}")

		if arguments.vararg:
			argString.append(f"*{scope.getVar(arguments.vararg.arg)}")
		elif len(arguments.kwonlyargs) > 0:
			argString.append("*")

		#Keyword args
		for i in range(len(arguments.kwonlyargs)):
			kwarg, default = arguments.kwonlyargs[i].arg, arguments.kw_defaults[i]
			if default:
				default = ExecuteExpression(default, scope)
				argString.append(f"{scope.getVar(kwarg)}={default}")
			else:
				argString.append(f"{scope.getVar(kwarg)}")

		if arguments.kwarg:
			argString.append(f"**{scope.getVar(arguments.kwarg.arg)}")

		return ", ".join(argString)

	def ImplementObjectDecorators(decorators, scope):
		out = []
		for decorator in decorators:
			out.append(f"@{ExecuteExpression(decorator, scope)}")
		return out

	#x = ["A", "DD", "B", "CCBC"]
	#print([S+str(ord(C)) for S in x if S != "A" for C in S if C != "B"])
	def ParseGenerators(generators, scope):
		terms = []
		for generator in generators:
			out = ""
			if generator.is_async:
				out += f"async "
			out += f"for {ExecuteExpression(generator.target, scope)} in {ExecuteExpression(generator.iter, scope)}"
			for conditional in generator.ifs:
				out += f" if {ExecuteExpression(conditional, scope)}"
			terms.append(out)
		return " ".join(terms)

	JunkLines = [
		lambda: ast.While(test=ast.Constant(value=GenerateRandomStr()),body=[ast.Break()],orelse=[]),
		lambda: ast.Pass(),
		lambda: ast.If(test=ast.Constant(value=GenerateRandomStr()),body=[ast.Pass()],orelse=[]),
		lambda: ast.If(
			test=ast.NamedExpr(target=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),value=ast.Constant(value=0)),
			body=[ast.Expr(value=ast.Call(func=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),args=[],keywords=[]))],orelse=[]
		)
	]
	def GenerateRandomJunk():
		return random.choice(JunkLines)()

	def __main__():
		scope = VariableScope(None, "core")
		debugprint("Input code:",code)
		if _DEBUG:
			beforeRun = ast.dump(code)
		try:
			out = ExecuteStatList(code.body, scope, Indent=False)
		except BaseException as exc:
			if _DEBUG:
				afterRun = ast.dump(code)
				if beforeRun != afterRun:
					debugprint("[!] The AST has been modified during execution. New AST:",afterRun)
				debugprint("[!] We ran into a critical error")
				if _DEBUG_LastExpr:
					debugprint("Last expression:",ast.dump(_DEBUG_LastExpr))
				else:
					debugprint("Last expression: None")
				if _DEBUG_LastStatement:
					debugprint("Last statement:",ast.dump(_DEBUG_LastStatement))
				else:
					debugprint("Last statement: None")
			raise exc
		else:
			if _DEBUG:
				afterRun = ast.dump(code)
				if beforeRun != afterRun:
					debugprint("[!] The AST has been modified during execution. New AST:",afterRun)
			out = "\n".join(out)
			debugprint(out)
			return out

	return __main__


testing = ast.parse(r"""
## Testing HandleArgAssignment (the call arg handler)
print("Hey!")
print(False)
print("What?", end="ASD\n")
print((lambda x,y : y/x)(5,6))
x,y,z = 5,6,7
print(x**2)
def f(arg, arg2):
	print(arg2, arg)
	return arg2*arg, arg/arg2
def f2(x, y, z=None, *, a, b, c=None, **k):
	print(x,y,z,"split",a,b,c,"split",k)
f2(1, 2, z=True, a=5, b=6, p=8, c=7)
print("out=", f(2, 3))

## Testing string management
s1 = '''ASSSDSD
sad
	b
b
bsoda'""''"''\\""'''

## Testing class objects
class Test:
	def __init__(self, v):
		self.y = v
	def gety(self):
		return self.y
print("Test=",Test)
TestObj = Test(8)
print("TestObj=",TestObj)
print("TO.gety()=",TestObj.gety())
TestObj.y += 15
print("TO.gety()=",TestObj.gety())

## Testing unpacking into dictionaries
x = {"A":5, 6:True}
y = {**x, 8:True}
z = {**y, **x, "A":1}
print(x,y,z)

## Testing complex generators
x = ["A", "DD", "B", "CCBC"]

print("ListComp")
obj = [S+str(ord(C)) for S in x if S != "A" for C in S if C != "B"]
print(type(obj), obj)

print("SetComp")
obj = {S+str(ord(C)) for S in x if S != "A" for C in S if C != "B"}
print(type(obj), obj)

print("DictComp")
obj = {S+str(ord(C)): S for S in x if S != "A" for C in S if C != "B"}
print(type(obj), obj)

print("Generator")
gen = (S+str(ord(C)) for S in x if S != "A" for C in S if C != "B")
print(type(gen), gen)
for v in gen:
	print("Gen object entry",v)

## Testing decorators
def d1(obj):
	print("Hooking obj in D1...")
	def ret():
		print("D1 hook on",obj)
		return obj()
	return ret
def d2(obj):
	print("Hooking obj in D2...")
	def ret():
		print("D2 hook on",obj)
		return obj()
	return ret

print("Decorators test 1")
@d1
@d2
def test():
	print("This is test")
	return True
print("out=",test())
print("Test part 2")
@d1
@d2
class test2:
	print("Executing body of test")
print("Running test")
print("out=",test2())
print("Decorators test done")

## Testing IfExpressions
print("IfExp1", 1 if True else 2 if True else 3 if True else 4)
print("IfExp2", 1 if True else 2 if False else 3 if True else 4)
print("IfExp3", (1 if True else 2) if False else (3 if True else 4))

## Testing a with clause (makes file so leaving commented)
# try:
# 	with open("with.txt","w") as f:
# 		print("Closed?",f.closed)
# 		print("file",f)
# 		f.write("Test text")
# 		f.dfsajasfjh()
# except:
# 	print("Ignoring intentional fail")
# print("Closed?",f.closed)

## Testing starred expressions in function calls
x = [2,3,4]
y = {2:3, 4:5}
y2 = {"end":"A\\n"}
print(1,[x],5)
print(1,*[x],5)
print(1, y, 6)
print(1, *y, 6)
print(1, 2, y2)
print(1, 2, *y2)
print(1, 2, **y2)

## Testing starred expressions in assignments
(n,*y,n) = 1,2,3
print(n,y)
a,*y,b,c = 1,2,3,4,5
print(a,y,b,c)
a,b,*y,c = 1,2,3,4,5
print(a,b,y,c)

## Testing JoinedStr and FormattedValue
x = [5, True]
y = [*x, 8]
z = [*y, *x, "A", 1]
print("x", x, "y", y, "z", z)
print(f"A{x*3}B{y}C{z*2}")
b = 5.4321
print(f"{b:2.3}")

## Testing global and nonlocal
## Note that the nonlocal x here breaks this system
x = 1
def y():
	global u
	x = 2
	class u:
		global w
		def w(self):
			print("w",self)
			x(2)
			u.o(3)
		#nonlocal x
		def x(self):
			print("x",self)
		def o(self):
			print("o",self)

y()
print("u=",u)
print("x=",x)
print("w=",w)
print("u.w exists?",hasattr(u,"w"))
print("u.x exists?",hasattr(u,"x"))
print("u.o exists?",hasattr(u,"o"))
w(1)

## Testing AnnAssign
a = 1
b: int = "2"
print(a,type(a))
print(b,type(b))
c: int #Does literally nothing but is valid syntax :/

## Testing imports
import imptest as xy
print(xy)
print(xy.__dict__)

# import imptest.imptest_file as tt
from imptest.imptest_file2 import *
print(xx)

from imptest import imptest_file2 as b2, imptest2 as mod
print("imptest_file2=",b2,"imptest2=",mod)
from imptest.imptest2 import imptest_subfile
print("imptest_subfile=",imptest_subfile)

## Testing for statements, just cause
x = {1:2, 3:4}
for y in x:
	print("fy",y)
for y,*z in x.items():
	print("fyz",y,z)
""")

# testing = ast.parse("""
# import asyncio

# async def test2(a):
# 	print("T2",a)
# 	await asyncio.sleep(1)
# 	print("Done!")
# 	return a

# async def test(a):
# 	print("Async run",a)
# 	return await test2(a)

# final = asyncio.run(test(5))
# print("Final=",final)
# """)

testing = ast.parse("""
def ParseOperator(op):
	op = type(op)
	#Boolean operations (and/or) are not supported and are handled just in the BoolOp expr
	#UnaryOp
	if op == ast.Invert:
		return lambda x: ~x
	elif op == ast.Not:
		return lambda x: not x
	elif op == ast.UAdd:
		return lambda x: +x
	elif op == ast.USub:
		return lambda x: -x
	#BinaryOp
	elif op == ast.Add:
		return lambda x,y: x + y
	elif op == ast.Sub:
		return lambda x,y: x - y
	elif op == ast.Mult:
		return lambda x,y: x * y
	elif op == ast.MatMult:
		return lambda x,y: x @ y
	elif op == ast.Div:
		return lambda x,y: x / y
	elif op == ast.Mod:
		return lambda x,y: x % y
	elif op == ast.Pow:
		return lambda x,y: x ** y
	elif op == ast.LShift:
		return lambda x,y: x << y
	elif op == ast.RShift:
		return lambda x,y: x >> y
	elif op == ast.BitOr:
		return lambda x,y: x | y
	elif op == ast.BitXor:
		return lambda x,y: x ^ y
	elif op == ast.BitAnd:
		return lambda x,y: x & y
	elif op == ast.FloorDiv:
		return lambda x,y: x // y
	#Compare
	elif op == ast.Eq:
		return lambda x,y: x == y
	elif op == ast.NotEq:
		return lambda x,y: x != y
	elif op == ast.Lt:
		return lambda x,y: x < y
	elif op == ast.LtE:
		return lambda x,y: x <= y
	elif op == ast.Gt:
		return lambda x,y: x > y
	elif op == ast.GtE:
		return lambda x,y: x >= y
	elif op == ast.Is:
		return lambda x,y: x is y
	elif op == ast.IsNot:
		return lambda x,y: x is not y
	elif op == ast.In:
		return lambda x,y: x in y
	elif op == ast.NotIn:
		return lambda x,y: x not in y
	#None of the above
	else:
		raise ExecutorException(f"Unrecognised operator type '{op}'")
""")

if len(sys.argv) > 1:
	try:
		content = ast.parse(open(sys.argv[1],"r",encoding="utf-8").read())
		debugprint("AST Dump:",ast.dump(content))
		open("_Rewriter_output.py","w",encoding="utf-8").write(CreateExecutionLoop(content)())
	except Exception as exc:
		import traceback
		print(f"[!] Encountered an error while processing {sys.argv} - {exc}")
		traceback.print_exc()
	input("Process complete...")
else:
	debugprint("AST Dump:",ast.dump(testing))
	debugprint("Generating execution loop")
	out = CreateExecutionLoop(testing)
	debugprint("Executing execution loop")
	finalText = out()
	debugprint("Finished execution loop")


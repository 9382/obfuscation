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

# Make the indentation inconsistent and random but still legal. Ignores the given indent input
OPTION_variable_indentation = False

# Attempts to ""obfuscate"" numbers by abstracting them. Slightly messy looking
OPTION_obscure_numbers = False

# Replaces strings with chr() compilation sequences
# Warning: Behaves horribly with f"strings"
OPTION_obscure_strings = False

# Attempts to re-name variables with random text. Tries to avoid changing anything that could be a concern, but it isn't perfect. Also INCREDIBLY messy
# Failures can be expected when using nonlocals/globals inside classes or using nonlocals weirdly
# Does not currently respect __all__ exports, and will rename them
OPTION_obscure_variables = True

# Same as obscure_variables, but instead uses as few characters as it can. Same limitations/issues apply
OPTION_minimise_variables = True

# Avoids renaming functions or classes so as to keep the eventual __qualname__ intact
OPTION_respect_qualname = True

# A funny bug encountered during the coding of minimise variables due to a missing return statement, too funny not to keep in. Only applies if minimise variables is enabled
OPTION_land_of_the_underscores = False

# Uses semicolons for sets of simple expressions to reduce line count
OPTION_use_semicolons = True

# Enables obscuring of positional arguments. Don't use if positional arguments are sometimes directly referenced, as this may cause errors
OPTION_obscure_posargs = True

# Occassionally inserts a bit of garbage (code that does, quite literally, nothing)
# Don't use a probability less than around 1/2.1 or there will be infinite recursion errors
OPTION_insert_junk = False
OPTION_junk_code_chance = 1/4
# Alternate version of junk code creation that can generate multiple junk in a row
# Don't use a probability less than around 1/3.1 or there will be infinite recursion errors
OPTION_use_while_for_junk = True

# Adds useless annotation markers, E.g. x: int or def y() -> int:
OPTION_add_useless_annotations = True

# Attempts to figure out when if elif statements are used instead of exponentially indenting else statements
OPTION_use_elif = True

# Uses brackets in places where they are probably excessive, but could be worth having to be on the safe side (this is barely coded :/)
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

SimpleStatements = [ # https://docs.python.org/3.8/reference/simple_stmts.html
	ast.Expr,
	ast.Assert,
	ast.Assign,
	ast.AugAssign,
	ast.AnnAssign,
	ast.Pass,
	ast.Delete,
	ast.Return,
	# ast.Yield, # Handled by ast.Expr
	ast.Raise,
	ast.Break,
	ast.Continue,
	ast.Import, ast.ImportFrom,
	# ast.Future, # Not a concept we have, basically just an import
	ast.Global,
	ast.Nonlocal,
]

class ExecutorException(Exception):
	pass

def PerformASTManipulation(AST):
	# DO NOT DO VARIABLE NAMES HERE. Thats for the writer to handle. We want to avoid any scope related logic here
	_DEBUG_LastExpr = None
	_DEBUG_LastStatement = None

	def ConvertInto(Object, NewObject):
		# A cursed method to replace an object with another by modifying the object itself instead of the object that references it
		assert isinstance(Object, ast.AST) and isinstance(NewObject, ast.AST)
		Object.__class__ = NewObject.__class__
		setattr(Object, "_fields", NewObject._fields)
		for field in Object._fields:
			if hasattr(Object, field):
				delattr(Object, field)
		for field in NewObject._fields:
			if hasattr(NewObject, field):
				setattr(Object, field, getattr(NewObject, field))

	_RandomCharacters = ["_"]
	if not (OPTION_minimise_variables and OPTION_land_of_the_underscores):
		for i in range(65, 91):
			_RandomCharacters.append(chr(i))
		for i in range(97, 123):
			_RandomCharacters.append(chr(i))
	def GenerateRandomStr(length=None):
		if length == None:
			length = random.randint(10,30)
		randomStr = ""
		for i in range(length):
			randomStr += random.choice(_RandomCharacters)
		return randomStr

	def MangleStringConstant(expr):
		value = expr.value
		offset = random.randint(-10,10)
		genVar = GenerateRandomStr(6)
		newExpr = ast.Call(
			func=ast.Attribute(value=ast.Call(func=ast.Name(id="str", ctx=ast.Load()), args=[], keywords=[]), attr="join", ctx=ast.Load()),
			args=[ast.ListComp(
				elt=ast.Call(func=ast.Name(id="chr", ctx=ast.Load()), args=[ast.BinOp(left=ast.Name(id=genVar, ctx=ast.Load()), op=ast.Add(), right=ast.Constant(value=offset))], keywords=[]),
				generators=[ast.comprehension(target=ast.Name(id=genVar, ctx=ast.Store()), iter=ast.List(elts=[ast.Constant(value=ord(x)-offset) for x in value], ctx=ast.Load()), ifs=[], is_async=0)]
			)],
			keywords=[]
		)
		ModifyExpression(newExpr) # Cause mangled numbers in our mangled string constants sound fun
		ConvertInto(expr, newExpr)

	def MangleNumberConstant(expr):
		number = expr.value
		method = random.randint(0, 2)
		if method == 0:
			offset = random.randint(-100, 100)
			ConvertInto(expr, ast.BinOp(
				left=ast.Constant(value=number-offset),
				op=ast.Add(),
				right=ast.Constant(value=offset)
			))
		elif method == 1:
			offset = random.randint(0, 20)
			ConvertInto(expr, ast.BinOp(
				left=ast.Constant(value=number-offset),
				op=ast.Add(),
				right=ast.Call(func=ast.Name(id="len", ctx=ast.Load()), args=[ast.Constant(value=GenerateRandomStr(offset))], keywords=[])
			))
		elif method == 2: #Method 1 but abuses tuples
			ConvertInto(expr, ast.BinOp(
				left=ast.Constant(value=number-1),
				op=ast.Add(),
				right=ast.Call(func=ast.Name(id="len", ctx=ast.Load()), args=[ast.Tuple(elts=[ast.Constant(value=GenerateRandomStr(random.randint(0, 20)))])], keywords=[])
			))

	def ModifyExpression(expr, *, PreventFormat=False):
		nonlocal _DEBUG_LastExpr
		_DEBUG_LastExpr = expr
		exprType = type(expr)
		debugprint("Modifying expression...",exprType)

		if exprType == ast.Constant:
			# No sub-expressions here
			if type(expr.value) == str:
				if OPTION_obscure_strings and not PreventFormat:
					MangleStringConstant(expr)
			elif type(expr.value) == int or type(expr.value) == float:
				if OPTION_obscure_numbers:
					MangleNumberConstant(expr)

		elif exprType == ast.Name:
			pass # No sub-expressions here

		elif exprType == ast.NamedExpr:
			ModifyExpression(expr.target)
			ModifyExpression(expr.value)

		elif exprType == ast.Starred:
			ModifyExpression(expr.value)

		elif exprType == ast.Attribute:
			ModifyExpression(expr.value)

		elif exprType == ast.JoinedStr:
			for value in expr.values:
				ModifyExpression(value, PreventFormat=True)

		elif exprType == ast.FormattedValue:
			ModifyExpression(expr.value)
			if expr.format_spec:
				ModifyExpression(expr.format_spec, PreventFormat=True)

		elif exprType == ast.keyword:
			ModifyExpression(expr.value)

		elif exprType == ast.alias:
			pass # No sub-expressions here

		elif exprType == ast.withitem:
			ModifyExpression(expr.context_expr)
			ModifyExpression(expr.optional_vars)

		elif exprType in [ast.Tuple, ast.List, ast.Set]:
			for entry in expr.elts:
				ModifyExpression(entry)

		elif exprType == ast.Dict:
			for i in range(len(expr.keys)):
				key, value = expr.keys[i], expr.values[i]
				ModifyExpression(value)
				if key != None:
					ModifyExpression(key)

		elif exprType in [ast.ListComp, ast.SetComp, ast.GeneratorExp]:
			ModifyGenerators(expr.generators)
			ModifyExpression(expr.elt)

		elif exprType == ast.DictComp:
			ModifyGenerators(expr.generators)
			ModifyExpression(expr.key)
			ModifyExpression(expr.value)

		elif exprType == ast.Index:
			ModifyExpression(expr.value)

		elif exprType == ast.Slice:
			if expr.lower:
				ModifyExpression(expr.lower)
			if expr.upper:
				ModifyExpression(expr.upper)
			if expr.step:
				ModifyExpression(expr.step)

		elif exprType == ast.Subscript:
			ModifyExpression(expr.value)
			ModifyExpression(expr.slice)

		elif exprType == ast.BoolOp:
			for subExpr in expr.values:
				ModifyExpression(subExpr)

		elif exprType == ast.UnaryOp:
			ModifyExpression(expr.operand)

		elif exprType == ast.BinOp:
			ModifyExpression(expr.left)
			ModifyExpression(expr.right)

		elif exprType == ast.Compare:
			ModifyExpression(expr.left)
			for comparator in expr.comparators:
				ModifyExpression(comparator)

		elif exprType == ast.IfExp:
			ModifyExpression(expr.body)
			ModifyExpression(expr.test)
			ModifyExpression(expr.orelse)

		elif exprType == ast.Call:
			ModifyExpression(expr.func)
			for arg in expr.args:
				ModifyExpression(arg)
			for kwarg in expr.keywords:
				ModifyExpression(kwarg)

		elif exprType == ast.Await:
			ModifyExpression(expr.value)

		elif exprType == ast.Lambda:
			ModifyArgs(expr.args)
			ModifyExpression(expr.body)

		elif exprType == ast.Yield:
			ModifyExpression(expr.value)

		elif exprType == ast.YieldFrom:
			ModifyExpression(expr.value)

		else:
			raise ExecutorException(f"[!] Unimplemented expression type {exprType}")

	#Fun fact: `obj: obj = "somevalue"` is valid even though obj was previously undefined, because annotation magic
	AnnotationTypes = ["int", "str", "float", "list", "dict", "None"]
	def ModifyStatement(statement):
		nonlocal _DEBUG_LastStatement
		_DEBUG_LastStatement = statement
		stType = type(statement)
		debugprint("Modifying statement...",stType)

		if stType == ast.Expr:
			if OPTION_ignore_docstrings and type(statement.value) == ast.Constant:
				return
			ModifyExpression(statement.value)

		elif stType == ast.Assign:
			ModifyExpression(statement.value)
			for target in statement.targets:
				ModifyExpression(target)

		elif stType == ast.AnnAssign:
			ModifyExpression(statement.target)
			ModifyExpression(statement.annotation)
			if statement.value:
				ModifyExpression(statement.value)

		elif stType == ast.AugAssign:
			ModifyExpression(statement.value)
			ModifyExpression(statement.target)

		elif stType == ast.Assert:
			ModifyExpression(statement.test)
			if statement.msg:
				ModifyExpression(statement.msg)

		elif stType == ast.Raise:
			if statement.exc:
				ModifyExpression(statement.exc)
				if statement.cause:
					ModifyExpression(statement.cause)

		elif stType == ast.Global:
			pass # No sub-expressions here

		elif stType == ast.Nonlocal:
			pass # No sub-expressions here

		elif stType == ast.Delete:
			pass # No sub-expressions here

		elif stType == ast.Return:
			if statement.value:
				ModifyExpression(statement.value)

		elif stType == ast.Pass:
			pass # No sub-expressions here

		elif stType == ast.Break:
			pass # No sub-expressions here

		elif stType == ast.Continue:
			pass # No sub-expressions here

		elif stType == ast.If:
			ModifyExpression(statement.test)
			ModifyStatlist(statement.body)
			ModifyStatlist(statement.orelse)

		elif stType == ast.While:
			ModifyExpression(statement.test)
			ModifyStatlist(statement.body)
			ModifyStatlist(statement.orelse)

		elif stType in [ast.For, ast.AsyncFor]:
			ModifyExpression(statement.iter)
			ModifyExpression(statement.target)
			ModifyStatlist(statement.body)
			ModifyStatlist(statement.orelse)

		elif stType in [ast.With, ast.AsyncWith]:
			for item in statement.items:
				ModifyExpression(item)
			ModifyStatlist(statement.body)

		elif stType == ast.Try:
			ModifyStatlist(statement.body)
			for handler in statement.handlers:
				ModifyStatement(handler)
			ModifyStatlist(statement.orelse)
			ModifyStatlist(statement.finalbody)

		elif stType == ast.ExceptHandler:
			if statement.type:
				ModifyExpression(statement.type)
			ModifyStatlist(statement.body)

		elif stType == ast.Import:
			for entry in statement.names:
				ModifyExpression(entry)

		elif stType == ast.ImportFrom:
			for entry in statement.names:
				ModifyExpression(entry)

		elif stType == ast.FunctionDef or stType == ast.AsyncFunctionDef:
			ModifyObjectDecorators(statement.decorator_list)
			ModifyArgs(statement.args)
			ModifyStatlist(statement.body)

		elif stType == ast.ClassDef:
			ModifyObjectDecorators(statement.decorator_list)
			for base in statement.bases:
				ModifyExpression(base)
			for keyword in statement.keywords:
				ModifyExpression(keyword)
			ModifyStatlist(statement.body)

		else:
			raise ExecutorException(f"[!] Unimplemented statement type {stType}")

	def ModifyStatlist(statList):
		debugprint("Modifying statement list...")
		if OPTION_insert_junk:
			# Junk insert round 1 - insert useless code
			for i in range(len(statList), -1, -1):
				# Fun fact: You are allowed to put stuff after a break/continue/return for some reason and its not a syntax error
				# So we don't need to adjust the range used
				while random.random() <= OPTION_junk_code_chance:
					statList.insert(i, random.choice(JunkLines)())
					if not OPTION_use_while_for_junk:
						break
			# Junk insert round 2 - wrap random (probably valid) code in ifs or elses
			if len(statList) > 1:
				startPoint = random.randint(0, len(statList)-1)
				endPoint = random.randint(startPoint, len(statList)-1) + 1
				while len(statList) > 1 and endPoint != len(statList):
					temporaryCopy = list(statList)
					statList.clear()
					statList.extend(temporaryCopy[:startPoint])
					realBody = temporaryCopy[startPoint:endPoint]
					fakeBody = [random.choice(JunkLines)()]
					condition, isTrue = GetWrapperTest()
					statList.append(ast.If(
						test=condition,
						body=isTrue and realBody or fakeBody,
						orelse=isTrue and fakeBody or realBody,
					))
					statList.extend(temporaryCopy[endPoint:])
					startPoint = random.randint(startPoint+1, len(statList)-1)
					endPoint = random.randint(startPoint, len(statList)-1) + 1
					if not OPTION_use_while_for_junk:
						break
		if len(statList) == 0 and OPTION_insert_junk:
			statList.append(ast.Pass())
		for statement in statList:
			ModifyStatement(statement)

	def ModifyArgs(arguments):
		#Positionals
		for default in arguments.defaults:
			ModifyExpression(default)
		#Keyword args
		for default in arguments.kw_defaults:
			if default != None:
				ModifyExpression(default)

	def ModifyObjectDecorators(decorators):
		for decorator in decorators:
			ModifyExpression(decorator)

	def ModifyGenerators(generators):
		for generator in generators:
			ModifyExpression(generator.iter)
			ModifyExpression(generator.target)
			for conditional in generator.ifs:
				ModifyExpression(conditional)

	WrapperTrueConditionLines = [
		lambda: ast.NamedExpr(target=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),value=ast.Constant(value=random.randint(1,10))),
		lambda: ast.Constant(value=GenerateRandomStr()),
	]
	WrapperFalseConditionLines = [
		lambda: ast.NamedExpr(target=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),value=ast.Constant(value=0)),
		lambda: ast.Constant(value=""),
		lambda: ast.Constant(value=None),
	]
	def GetWrapperTest():
		return (random.choice(WrapperTrueConditionLines)(), True) if random.randint(1, 2) == 1 else (random.choice(WrapperFalseConditionLines)(), False)

	JunkLines = [
		lambda: ast.While(test=ast.Constant(value=GenerateRandomStr()),body=[ast.Break()],orelse=[]),
		lambda: ast.Pass(),
		lambda: ast.If(test=ast.Constant(value=GenerateRandomStr()),body=[ast.Pass()],orelse=[]),
		lambda: ast.If(
			test=ast.NamedExpr(target=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),value=ast.Constant(value=0)),
			body=[ast.Expr(value=ast.Call(func=ast.Name(id=GenerateRandomStr(),ctx=ast.Store()),args=[],keywords=[]))],
			orelse=[]
		),
		lambda: ast.Assign(targets=[ast.Name(id=GenerateRandomStr(),ctx=ast.Store())],value=ast.Constant(value=random.random())),
		lambda: ast.Assign(targets=[ast.Name(id=GenerateRandomStr(),ctx=ast.Store())],value=ast.Constant(value=random.randint(-10,10))),
		lambda: ast.Assign(targets=[ast.Name(id=GenerateRandomStr(),ctx=ast.Store())],value=ast.Constant(value="")),
		lambda: ast.Assign(targets=[ast.Name(id=GenerateRandomStr(),ctx=ast.Store())],value=ast.List(elts=[],ctx=ast.Load())),
		lambda: ast.Assign(targets=[ast.Name(id=GenerateRandomStr(),ctx=ast.Store())],value=ast.Dict(keys=[],values=[])),
	]

	try:
		ModifyStatlist(AST.body)
	except BaseException as exc:
		if _DEBUG:
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


def CreateExecutionLoop(code):
	import builtins
	class VariableScope:
		def __init__(self, Parent, scopeType):
			self.Parent = Parent
			self.scopeType = scopeType
			self.Globals = set()
			self.NonLocals = set()
			self.VarMapping = {}
		def getVar(self, var, *, fromChild=False, getFromCore=False):
			out = self._getVar(var, fromChild=fromChild, getFromCore=getFromCore)
			if not fromChild and type(out) == tuple:
				return out[0]
			else:
				return out
		def _getVar(self, var, *, fromChild, getFromCore):
			debugprint("Asked to retrieve variable",var,self.VarMapping)
			# The order of this is very messy, changes a lot, and is mostly guess work
			# But, uh, this makes sense, right?
			if getFromCore:
				if self.scopeType != "core":
					return self.Parent.getVar(var, getFromCore=True)
				else:
					return self.createVar(var)
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
		def createVar(self, var, forceNormal=False):
			if not (OPTION_obscure_variables or OPTION_minimise_variables):
				return str(var)
			else:
				if var in self.VarMapping:
					return self.VarMapping[var]
				elif hasattr(builtins, var) or forceNormal:
					self.VarMapping[var] = var
					return var
				else:
					if self.scopeType == "class":
						newName = var
					else:
						newName = GenerateRandomStr(ForVariable=True)
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
				if var not in self.Globals:
					self.VarMapping[var] = self.Parent.getVar(var, getFromCore=True)
					self.Globals.add(var)
				self.Parent.triggerGlobal(var)
		def triggerNonlocal(self, var):
			if self.scopeType == "core":
				raise SyntaxError("nonlocal declaration not allowed at module level")
			if var not in self.NonLocals:
				self.VarMapping[var] = self.Parent.getVar(var)
				self.NonLocals.add(var)

	_RandomCharacters = ["_"]
	if not (OPTION_minimise_variables and OPTION_land_of_the_underscores):
		for i in range(65, 91):
			_RandomCharacters.append(chr(i))
		for i in range(97, 123):
			_RandomCharacters.append(chr(i))
	lastVar = []
	def GenerateSmallestStr():
		nonlocal lastVar
		for i in range(len(lastVar)-1, -1, -1):
			if lastVar[i] != len(_RandomCharacters)-1:
				lastVar[i] = lastVar[i] + 1
				out = str().join(_RandomCharacters[c] for c in lastVar)
				if out in ["if", "do", "in", "as", "is", "or"]:
					return GenerateSmallestStr() #just be a bit careful
				return out #(This missing line is the origin of land of the underscores)
			else:
				lastVar[i] = 0
		lastVar = [0] * (len(lastVar)+1)
		return _RandomCharacters[0] * len(lastVar)
	def GenerateRandomStr(length=None, ForVariable=False):
		if OPTION_minimise_variables and ForVariable:
			return GenerateSmallestStr()
		if length == None:
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
					out = WrapInQuotes(expr.value).replace("\n","\\n").replace("\0", "\\0")
					if OPTION_insert_junk:
						return out + "[::]"
					else:
						return out
				return expr.value
			else:
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
				out = f"f{WrapInQuotes(out)}"
				if OPTION_insert_junk:
					out = out + "[::]"
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
				if (OPTION_obscure_variables or OPTION_minimise_variables) and expr.name != "*":
					return f"{expr.name} as {scope.createVar(expr.name)}"
				else:
					return f"{expr.name}"

		elif exprType == ast.withitem:
			return f"{ExecuteExpression(expr.context_expr, scope)} as {ExecuteExpression(expr.optional_vars, scope)}"

		elif exprType in [ast.Tuple, ast.List, ast.Set]:
			items = []
			for entry in expr.elts:
				items.append(ExecuteExpression(entry, scope))
			out = ", ".join(items)
			if exprType == ast.Tuple:
				if len(items) == 1:
					return f"({out},)"
				else:
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

	#Fun fact: `obj: obj = "somevalue"` is valid even though obj was previously undefined, because annotation magic
	AnnotationTypes = ["int", "str", "float", "list", "dict", "None"]
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
			value = ExecuteExpression(statement.value, scope)
			if OPTION_add_useless_annotations:
				if len(statement.targets) == 1 and type(t := statement.targets[0]) in [ast.Name, ast.Attribute, ast.Subscript]:
					if type(t) == ast.Name and (t.id in scope.NonLocals or t.id in scope.Globals): # Can't annotate a nonlocal/global because idk
						target = ExecuteExpression(t, scope)
					else:
						target = ExecuteExpression(t, scope) + ": " + random.choice(AnnotationTypes)
					return f"{target} = {value}"
			target = " = ".join([ExecuteExpression(t, scope) for t in statement.targets])
			return f"{target} = {value}"

		elif stType == ast.AnnAssign:
			target = ExecuteExpression(statement.target, scope)
			if OPTION_add_useless_annotations:
				target = target + ": " + ExecuteExpression(statement.annotation, scope)
			if statement.value:
				value = ExecuteExpression(statement.value, scope)
				return f"{target} = {value}"
			elif OPTION_add_useless_annotations:
				return target
			else:
				return #Who cares? literally the equivilant of a pass statement

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
			ExtendWithBody(out, ExecuteStatlist(statement.body, scope))
			if len(statement.orelse) > 0:
				if OPTION_use_elif and len(statement.orelse) == 1 and type((alternate := statement.orelse[0])) == ast.If:
					out.append(f"elif {ExecuteExpression(alternate.test, scope)}:")
					alternateBody = ExecuteStatement(alternate, scope)
					alternateBody[0] = alternateBody[0].replace(f"if {ExecuteExpression(alternate.test, scope)}:", "")
					if alternateBody[0] != "": # the alternate.body is fully inlined by semicolons
						out[len(out)-1] += alternateBody[0] # Already has a starting space
					out.extend(alternateBody[1:])
				else:
					out.append("else:")
					ExtendWithBody(out, ExecuteStatlist(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else: pass"] if OPTION_use_semicolons else ["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType == ast.While:
			out = []
			out.append(f"while {ExecuteExpression(statement.test, scope)}:")
			ExtendWithBody(out, ExecuteStatlist(statement.body, scope))
			if len(statement.orelse) > 0:
				out.append("else:")
				ExtendWithBody(out, ExecuteStatlist(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else: pass"] if OPTION_use_semicolons else ["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType in [ast.For, ast.AsyncFor]:
			iterRange = ExecuteExpression(statement.iter, scope)
			target = ExecuteExpression(statement.target, scope)
			body = ExecuteStatlist(statement.body, scope)
			orelse = ExecuteStatlist(statement.orelse, scope)
			out = []
			out.append(f"{stType == ast.AsyncFor and 'async ' or ''}for {target} in {iterRange}:")
			ExtendWithBody(out, body)
			if orelse:
				out.append("else:")
				ExtendWithBody(out, orelse)
			elif OPTION_insert_junk:
				out.extend(["else: pass"] if OPTION_use_semicolons else ["else:",f"{OPTION_indent_char}pass"])
			return out

		elif stType in [ast.With, ast.AsyncWith]:
			out = []
			out.append(f"{stType == ast.AsyncWith and 'async ' or ''}with {', '.join(ExecuteExpression(item, scope) for item in statement.items)}:")
			ExtendWithBody(out, ExecuteStatlist(statement.body, scope))
			return out

		elif stType == ast.Try:
			out = []
			out.append("try:")
			ExtendWithBody(out, ExecuteStatlist(statement.body, scope))
			for handler in statement.handlers:
				out.extend(ExecuteStatement(handler, scope))
			if len(statement.orelse) > 0:
				out.append("else:")
				ExtendWithBody(out, ExecuteStatlist(statement.orelse, scope))
			elif OPTION_insert_junk:
				out.extend(["else: pass"] if OPTION_use_semicolons else ["else:",f"{OPTION_indent_char}pass"])
			if len(statement.finalbody) > 0:
				out.append("finally:")
				ExtendWithBody(out, ExecuteStatlist(statement.finalbody, scope))
			elif OPTION_insert_junk:
				out.extend(["finally: pass"] if OPTION_use_semicolons else ["finally:",f"{OPTION_indent_char}pass"])
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
			ExtendWithBody(out, ExecuteStatlist(statement.body, scope))
			return out

		elif stType == ast.Import:
			return f"import {', '.join(ExecuteExpression(entry, scope) for entry in statement.names)}"

		elif stType == ast.ImportFrom:
			return f"from {'.'*statement.level}{statement.module} import {', '.join(ExecuteExpression(entry, scope) for entry in statement.names)}"

		elif stType == ast.FunctionDef or stType == ast.AsyncFunctionDef:
			subScope = VariableScope(scope, "function")
			out = []
			decorators = ImplementObjectDecorators(statement.decorator_list, scope)
			name = scope.createVar(statement.name)
			args = HandleArgs(subScope, statement.args)
			body = ExecuteStatlist(statement.body, subScope)
			out.extend(decorators)
			returntype = (" -> "+random.choice(AnnotationTypes) if OPTION_add_useless_annotations else "")
			if stType == ast.AsyncFunctionDef:
				out.append(f"async def {name}({args}){returntype}:")
			else:
				out.append(f"def {name}({args}){returntype}:")
			ExtendWithBody(out, body)
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
			body = ExecuteStatlist(statement.body, subScope)
			out.extend(decorators)
			out.append(f"class {name}({args}):")
			ExtendWithBody(out, body)
			return out

		else:
			raise ExecutorException(f"[!] Unimplemented statement type {stType}")
		raise ExecutorException(f"[!] Statement of type {stType} never returned")

	def ExtendWithBody(out, body):
		if len(body) == 1 and body[0][0] not in " \t": # Entire thing was presumably simple and inlined by semicolons
			out[len(out)-1] += " " + body[0]
		else:
			out.extend(body)

	def ExecuteStatlist(statList, scope, Indent=True):
		debugprint("Executing statement list...")
		compiledText = []
		for statement in statList:
			# Pre-calculate variable names to avoid post-scope weirdness
			# This does not do any sort of deep traversal, only statements immediately in this statlist are considered
			# This helps make code like `def x(): return y \n y = 5` rewrite correctly when replacing variables (though just dont write like this in the first place please)
			if type(statement) in [ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef]:
				scope.createVar(statement.name, forceNormal=OPTION_respect_qualname)
			elif type(statement) == ast.Assign:
				for target in statement.targets:
					ExecuteExpression(target, scope)
			elif type(statement) in [ast.AugAssign, ast.AnnAssign]:
				ExecuteExpression(statement.target, scope)
			elif type(statement) in [ast.Import, ast.ImportFrom]:
				for name in statement.names:
					ExecuteExpression(name, scope)
			elif type(statement) in [ast.Global, ast.Nonlocal]:
				ExecuteStatement(statement, scope)
		previousWasSimple = False
		entireBodyWasSimple = True
		for statement in statList:
			isSimple = OPTION_use_semicolons and type(statement) in SimpleStatements
			out = ExecuteStatement(statement, scope)
			if type(out) == list:
				compiledText.extend(out)
			elif type(out) == str:
				if isSimple and previousWasSimple and len(compiledText) > 0:
					compiledText[len(compiledText)-1] += "; " + out
				else:
					compiledText.append(out)
			elif out == None:
				pass
			else:
				debugprint(":( poor type return to statlist",type(out))
				compiledText.append(str(out))
			previousWasSimple = isSimple
			entireBodyWasSimple = entireBodyWasSimple and isSimple
		if Indent and not entireBodyWasSimple:
			IndentChar = OPTION_variable_indentation and (
				random.randint(1, 4)==1 and "\t"*random.randint(1, 2) or " "*random.randint(1, 6) # Yes, this is legal indentation logic
			) or OPTION_indent_char
			for i in range(len(compiledText)):
				compiledText[i] = f"{IndentChar}{compiledText[i]}"
		return compiledText

	#def f2(x, y, z=None, *, a, b, c=None, **k):
	#	print('Cool')
	def HandleArgs(scope, arguments):
		#Setup
		argString = []
		def createVar(name, forceNormal):
			text = scope.createVar(name, forceNormal=forceNormal)
			return text + ": " + random.choice(AnnotationTypes) if (OPTION_add_useless_annotations and scope.scopeType != "lambda") else text

		#Positionals
		debugprint("Positionals",arguments.args,arguments.defaults)
		defaultOffset = len(arguments.args)-len(arguments.defaults)
		for i in range(len(arguments.args)):
			arg = arguments.args[i]
			if i >= defaultOffset:
				default = ExecuteExpression(arguments.defaults[i-defaultOffset], scope)
				argString.append(f"{createVar(arg.arg, not OPTION_obscure_posargs)}={default}")
			else:
				argString.append(f"{createVar(arg.arg, not OPTION_obscure_posargs)}")

		if arguments.vararg:
			argString.append(f"*{createVar(arguments.vararg.arg, not OPTION_obscure_posargs)}")
		elif len(arguments.kwonlyargs) > 0:
			argString.append("*")

		#Keyword args
		for i in range(len(arguments.kwonlyargs)):
			kwarg, default = arguments.kwonlyargs[i].arg, arguments.kw_defaults[i]
			if default:
				default = ExecuteExpression(default, scope)
				argString.append(f"{createVar(kwarg, True)}={default}")
			else:
				argString.append(f"{createVar(kwarg, True)}")

		if arguments.kwarg:
			argString.append(f"**{createVar(arguments.kwarg.arg, True)}")

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
			generationTarget = ExecuteExpression(generator.iter, scope)
			out += f"for {ExecuteExpression(generator.target, scope)} in {generationTarget}"
			for conditional in generator.ifs:
				out += f" if {ExecuteExpression(conditional, scope)}"
			terms.append(out)
		return " ".join(terms)

	def __main__():
		scope = VariableScope(None, "core")
		debugprint("Input code:",code)
		debugprint("Performing AST Manipulation...")
		PerformASTManipulation(code)
		debugprint("Writing AST...")
		if _DEBUG:
			beforeRun = ast.dump(code)
		try:
			out = ExecuteStatlist(code.body, scope, Indent=False)
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
					debugprint("[!] The AST has been modified during execution. New AST:", afterRun)
					debugprint("[!} Old AST:", beforeRun)
			out = "\n".join(out)
			return out

	return __main__


testing = ast.parse(open("TestCode.py", "r", encoding="utf-8").read())

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
	open("_Rewriter_output.py","w",encoding="utf-8").write(finalText)
	debugprint("Finished execution loop")

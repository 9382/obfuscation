# THIS VERSION USES SERIALIZED INPUT AND REFERENCES, AND THEREFORE MAY BE LESS UNDERSTANDABLE
import math
import ast

_DEBUG = True
def debugprint(*args, **kwargs):
	if _DEBUG:
		print("[Debug]", *args, **kwargs)
if not _DEBUG:
	print("[Debug] debugprint is disabled")

""" Progress Report

== Statements ==
Name				Status				Extra notes

FunctionDef			Implemented
AsyncFunctionDef	Implemented			Useless until an implementation for await is figured out, but might as well have it
ClassDef			Implemented
Return				Implemented

Delete				Implemented
Assign				Implemented
AugAssign			Implemented			Support for an __iXYZ__ attr check? possibly OTT
AnnAssign			Implemented			Literally just a glorified Assign

For					Implemented			Mostly untested
AsyncFor			Not implemented
While				Implemented
If					Implemented			Partially tested
With				Implemented			Basic implementation works but unsure if there's a more complicated scenario possible
AsyncWith			Not implemented

Match				Not implemented		Switch statement (but called `case`)

Raise				Implemented
Try					Implemented			Mostly untested
Assert				Implemented

Import				Implemented
ImportFrom			Implemented

Global				Implemented			Tested but still not confident it's perfect
Nonlocal			Implemented			ditto
Expr				Implemented
Pass				Implemented
Break				Implemented			Mostly untested
Continue			Implemented			Mostly untested


== Expressions ==
Name				Status				Extra notes

BoolOp				Implemented
NamedExpr			Implemented
BinOp				Implemented
UnaryOp				Implemented
Lambda				Implemented
IfExp				Implemented
Dict				Implemented
Set					Implemented
ListComp			Implemented			Does not support an async for call
SetComp				Implemented			^
DictComp			Implemented			^
GeneratorExp		Implemented			^

Await				Not implemented		I despise this expression, and for good reason too
Yield				Not implemented
YieldFrom			Not implemented

Compare				Implemented
Call				Implemented
FormattedValue		Implemented
JoinedStr			Implemented
Constant			Implemented 		`kind` is ignored since idk what the point is

Attribute			Implemented			Some statements manually escape evaluating this (Assign/Delete). That's probably fine though
Subscript			Implemented			ditto
Starred				Implemented			Currently no punishment for >1 Starred expr in Assign. Implemented manually into multiple methods since there's no simple way to provide an unpacking :/
Name				Implemented
List				Implemented
Tuple				Implemented			Leaches off list generator

Slice				Implemented			Part of Subscript. Doesn't support constants (are those a py3.8 thing even?)
Index				Implemented			Undocumented (removed after py3.8?). Part of Subscript.
ExtSlice			Not implemented		Removed after py3.8 (or at least changed). No idea what it actually is cause no damn example is given

== Notes ==
Currently, the main thing actually missing is async / yield support, which has absolutely no implementation right now. That'll likely need a *lot* of looking into it, and that isn't gonna be fun

Await is going to be incredibly complex to implement - we don't want everything here to be async too (hell, we can't do that thanks to FunctionDef), but overrunning asyncio.run is apparently illegal, which is unhelpful
It's possible to run some async functions, E.g. using .send(None), but this won't respect blocking functions like asyncio.sleep, which is a big problem

The issue with yield: Yield has to be able to basically "kill" a function upon hitting a yield *expression*, but then be able to resume from such a state once called again.
The key thing to note here is that yield is an expression. This makes a statement like "a = yield 5" valid, and that's a complete nightmare for us.
"""

def CreateExecutionLoop(code):
	import builtins
	class VariableScope:
		def __init__(self, Parent, scopeType):
			self.Parent = Parent
			if scopeType == "core":
				self.Variables = globals()
			else:
				self.Variables = {}
			self.scopeType = scopeType
			self.References = set()
			self.Assignments = set()
			self.Globals = set()
			self.NonLocals = set()
		def getVar(self, var):
			debugprint("Asked to retrieve variable",var)
			# debugprint(self,self.Variables)
			self.References.add(var)
			if var in self.Variables:
				return self.Variables[var]
			else:
				if self.Parent:
					return self.Parent.getVar(var)
				elif hasattr(builtins,var):
					return getattr(builtins,var)
				else:
					raise NameError(f"name '{var}' is not defined")
		def setVarRaw(self, var, value): #Bypass scope-based checks
			debugprint("Asked to raw set variable",var)
			if type(var) != str:
				raise ExecutorException(f"Attempted to set a variable of type {type(var)}")
			if var in self.Globals or var in self.NonLocals:
				self.Parent.setVarRaw(var, value)
				return
			if var in self.References and var not in self.Assignments:
				raise UnboundLocalError(f"local variable '{var}' referenced before assignment")
			self.Variables[var] = value
			self.Assignments.add(var)
		def setVar(self, var, value):
			debugprint("Asked to set variable",var)
			if self.scopeType == "asclause":
				return self.Parent.setVar(var, value)
			self.setVarRaw(var, value)
		def deleteVar(self, var):
			debugprint("Asked to delete variable",var)
			if self.scopeType == "asclause":
				self.Parent.deleteVar(var)
			if var in self.Variables:
				self.Variables.pop(var)
				self.Assignments.discard(var)
				self.References.discard(var)
			else:
				if var in self.NonLocals or var in self.Globals:
					#The nonlocal and global state will persist beyond deletion, so DONT clear those
					self.Parent.deleteVar(var)
		def triggerGlobal(self, var):
			if self.scopeType != "core":
				if var in self.References:
					raise SyntaxError(f"name '{var}' is used prior to global declaration")
				elif var in self.Assignments:
					raise SyntaxError(f"name '{var}' is assigned to before global declaration")
				self.Globals.add(var)
				self.Parent.triggerGlobal(var)
		def triggerNonlocal(self, var):
			if self.scopeType == "core":
				raise SyntaxError("nonlocal declaration not allowed at module level")
			if self.Parent.scopeType == "core" or var not in self.Parent.Variables:
				raise SyntaxError(f"no binding for nonlocal '{var}' found")
			self.NonLocals.add(var)

	class ClassScope(VariableScope):
		def __init__(self, Parent, Class):
			super().__init__(Parent, "class")
			self.Class = Class
		def getVar(self, var):
			return self.Parent.getVar(var) #We don't offer variables, since we dont store them like that
		def setVar(self, var, value):
			if var in self.Globals or var in self.NonLocals:
				self.Parent.setVar(var, value) #It doesn't become a member of the class if a nonlocal/global call is given
			else:
				self.Variables[var] = value
				setattr(self.Class, var, value)
		def deleteVar(self, var):
			self.Variables.pop(var)
			delattr(self.Class, var)

	class ReturnStatement:
		def __init__(self, Type, Data=None):
			self.Type = Type
			self.Data = Data

	class ExecutorException(Exception):
		pass

	def GoodGrammar(arg, tSingular, tPlural):
		if len(arg) == 1:
			return f"{tSingular} '{arg[0]}'"
		elif len(arg) == 2:
			return f"{tPlural} '{arg[0]}' and '{arg[1]}'"
		else:
			s = f"{tSingular} "
			for i in range(len(arg)-2):
				s = s + f"'{arg[i]}', "
			return f"{s}'{arg[len(arg)-2]}' and '{arg[len(arg)-1]}'"

	def ParseOperator(op):
		op = op[0]
		#Boolean operations (and/or) are not supported and are handled just in the BoolOp expr
		#UnaryOp
		if op == 0:
			return lambda x: ~x
		elif op == 1:
			return lambda x: not x
		elif op == 2:
			return lambda x: +x
		elif op == 3:
			return lambda x: -x
		#BinaryOp
		elif op == 4:
			return lambda x,y: x + y
		elif op == 5:
			return lambda x,y: x - y
		elif op == 6:
			return lambda x,y: x * y
		elif op == 7:
			return lambda x,y: x @ y
		elif op == 8:
			return lambda x,y: x / y
		elif op == 9:
			return lambda x,y: x % y
		elif op == 10:
			return lambda x,y: x ** y
		elif op == 11:
			return lambda x,y: x << y
		elif op == 12:
			return lambda x,y: x >> y
		elif op == 13:
			return lambda x,y: x | y
		elif op == 14:
			return lambda x,y: x ^ y
		elif op == 15:
			return lambda x,y: x & y
		elif op == 16:
			return lambda x,y: x // y
		#Compare
		elif op == 17:
			return lambda x,y: x == y
		elif op == 18:
			return lambda x,y: x != y
		elif op == 19:
			return lambda x,y: x < y
		elif op == 20:
			return lambda x,y: x <= y
		elif op == 21:
			return lambda x,y: x > y
		elif op == 22:
			return lambda x,y: x >= y
		elif op == 23:
			return lambda x,y: x is y
		elif op == 24:
			return lambda x,y: x is not y
		elif op == 25:
			return lambda x,y: x in y
		elif op == 26:
			return lambda x,y: x not in y
		#None of the above
		else:
			raise ExecutorException(f"Unrecognised operator type '{op}'")

	_DEBUG_LastExpr = None
	_DEBUG_LastStatement = None

	def ExecuteExpression(expr, scope, *, ForcedContext=None):
		nonlocal _DEBUG_LastExpr
		_DEBUG_LastExpr = expr
		exprType = expr[0]
		debugprint("Executing expression...",exprType)

		if exprType == 16:
			return expr[1]

		elif exprType == 20:
			ctx = ForcedContext if ForcedContext != None else expr[2][0]
			if ctx == 0:
				return scope.getVar(expr[1])
			elif ctx == 1:
				return expr[1]
			elif ctx == 2:
				return expr[1]

		elif exprType == 1:
			target, value = ExecuteExpression(expr[1], scope), ExecuteExpression(expr[2], scope)
			if type(target) == tuple or type(target) == list:
				raise SyntaxError(f"cannot use assignment expressions with {type(target)}")
			scope.setVar(target, value)
			return value

		elif exprType == 19:
			ctx = ForcedContext if ForcedContext != None else expr[2][0]
			if ctx == 0:
				return ExecuteExpression(expr[1], scope)
			elif ctx == 1:
				return ExecuteExpression(expr[1], scope)
			elif ctx == 2:
				raise ExecutorException("Direct call to evaluate a Starred del expression")

		elif exprType == 17:
			ctx = ForcedContext if ForcedContext != None else expr[3][0]
			if ctx == 0:
				return getattr(ExecuteExpression(expr[1], scope), expr[2])
			elif ctx == 1:
				raise ExecutorException("Direct call to evaluate an Attribute store expression") #stmt.Assign shouldn't let a Store call creep into here. If it does, panic
			elif ctx == 2:
				raise ExecutorException("Direct call to evaluate an Attribute del expression") #ditto for stmt.Delete

		elif exprType == 15:
			return str().join(ExecuteExpression(value, scope) for value in expr[1])

		elif exprType == 14:
			value = ExecuteExpression(expr[1], scope)
			if expr[2] == 115:
				value = str(value)
			elif expr[2] == 114:
				value = repr(value)
			elif expr[2] == 97:
				value = ascii(value)
			if expr[3]:
				value = format(value, ExecuteExpression(expr[3], scope))
			else:
				value = format(value)
			return value

		elif exprType == 25:
			return expr[1], ExecuteExpression(expr[2], scope)

		elif exprType in [22, 21, 7]:
			out = []
			for entry in expr[1]:
				if entry[0] == 19:
					out.extend(ExecuteExpression(entry, scope))
				else:
					out.append(ExecuteExpression(entry, scope))
			if exprType == 22:
				return tuple(out)
			elif exprType == 21:
				return out
			elif exprType == 7:
				return set(out)

		elif exprType == 6:
			out = {}
			for i in range(len(expr[1])):
				key, value = expr[1][i], expr[2][i]
				if key == None: #value is a dict that needs unpacking
					for k,v in ExecuteExpression(value, scope).items():
						out[k] = v
				else:
					out[ExecuteExpression(key, scope)] = ExecuteExpression(value, scope)
			return out

		elif exprType in [8, 9, 11]:
			subScope = VariableScope(scope, "generator")
			out = ParseGenerators(expr[2], [expr[1]], subScope)
			if exprType == 8:
				return [x[0] for x in out]
			elif exprType == 9:
				return {x[0] for x in out}
			elif exprType == 11:
				return (x[0] for x in out)

		elif exprType == 10:
			subScope = VariableScope(scope, "generator")
			out = ParseGenerators(expr[3], [expr[1], expr[2]], subScope)
			return {x[0]: x[1] for x in out}

		elif exprType == 24:
			return ExecuteExpression(expr[1], scope)

		elif exprType == 23:
			lower = expr[1] and ExecuteExpression(expr[1], scope)
			upper = expr[2] and ExecuteExpression(expr[2], scope)
			step = expr[3] and ExecuteExpression(expr[3], scope)
			return slice(lower, upper, step)

		elif exprType == 18:
			ctx = ForcedContext if ForcedContext != None else expr[3][0]
			if ctx == 0:
				value = ExecuteExpression(expr[1], scope)
				Slice = ExecuteExpression(expr[2], scope)
				return value[Slice]
			elif ctx == 1:
				raise ExecutorException("This shouldn't get called")
			elif ctx == 2:
				raise ExecutorException("This shouldn't get called")

		elif exprType == 0:
			op = expr[1][0]
			if op == 27:
				for subExpr in expr[2]:
					value = ExecuteExpression(subExpr, scope)
					if not value:
						return value
				return value
			elif op == 28:
				for subExpr in expr[2]:
					value = ExecuteExpression(subExpr, scope)
					if value:
						return value
				return value

		elif exprType == 3:
			op = ParseOperator(expr[1])
			operand = ExecuteExpression(expr[2], scope)
			return op(operand)

		elif exprType == 2:
			Lhs = ExecuteExpression(expr[1], scope)
			op = ParseOperator(expr[2])
			Rhs = ExecuteExpression(expr[3], scope)
			return op(Lhs, Rhs)

		elif exprType == 12:
			subject = ExecuteExpression(expr[1], scope)
			for i in range(len(expr[2])):
				op, comparison = ParseOperator(expr[2][i]), ExecuteExpression(expr[3][i], scope)
				successState = op(subject, comparison)
				if successState == True:
					if i == len(expr[2])-1:
						return True
					else:
						subject = comparison
				else:
					return False

		elif exprType == 5: #This is in and of itself an IfExp
			return ExecuteExpression(expr[2], scope) if ExecuteExpression(expr[1], scope) else ExecuteExpression(expr[3], scope)

		elif exprType == 13:
			func = ExecuteExpression(expr[1], scope)
			args = []
			for entry in expr[2]:
				if entry[0] == 19:
					args.extend(ExecuteExpression(entry, scope))
				else:
					args.append(ExecuteExpression(entry, scope))
			kwargs = {}
			for entry in expr[3]:
				name, value = ExecuteExpression(entry, scope)
				if name:
					kwargs[name] = value
				else:
					kwargs.update(value)
			return func(*args, **kwargs)

		elif exprType == 4:
			def LambdaHandler(args, kwargs):
				subScope = VariableScope(scope, "lambda")
				HandleArgAssignment(subScope, expr[1], "<lambda>", args, kwargs)
				return ExecuteExpression(expr[2], subScope)
			return lambda *args, **kwargs : LambdaHandler(args, kwargs)

		else:
			raise ExecutorException(f"[!] Unimplemented expression type {exprType}")

	def ExecuteStatement(statement, scope):
		nonlocal _DEBUG_LastStatement
		_DEBUG_LastStatement = statement
		stType = statement[0]
		debugprint("Executing statement...",stType)

		if stType == 19:
			ExecuteExpression(statement[1], scope)

		elif stType == 4:
			for target in statement[1]:
				if target[0] == 17:
					delattr(ExecuteExpression(target[1], scope), target[2])
				elif target[0] == 18:
					del ExecuteExpression(target[1], scope)[ExecuteExpression(target[2], scope)]
				else:
					scope.deleteVar(ExecuteExpression(target, scope))

		elif stType == 5 or stType == 7:
			if stType == 5:
				value = ExecuteExpression(statement[2], scope)
				for target in statement[1]:
					Assign(target, value, scope)
			elif stType == 7:
				if statement[3]:
					Assign(statement[1], ExecuteExpression(statement[3], scope), scope)
				#else: Literally just decorative, don't care, don't process it
		elif stType == 6:
			value = ExecuteExpression(statement[3], scope)
			target = statement[1]
			op = ParseOperator(statement[2])
			if target[0] == 20:
				scope.setVar(ExecuteExpression(target, scope), op(ExecuteExpression(target, scope, ForcedContext=0), value))
			elif target[0] == 17:
				setattr(ExecuteExpression(target[1], scope), target[2], op(ExecuteExpression(target, scope, ForcedContext=0), value))
			elif target[0] == 18:
				targetValue = ExecuteExpression(target[1], scope)
				targetSlice = ExecuteExpression(target[2], scope)
				targetValue[targetSlice] = op(targetValue[targetSlice], value)
			else:
				raise ExecutorException(f"Unable to assign to unrecognised type '{target[0]}'")

		elif stType == 14:
			if not ExecuteExpression(statement[1], scope):
				raise AssertionError(ExecuteExpression(statement[2], scope))

		elif stType == 12:
			if statement[1]:
				if statement[2]:
					raise ExecuteExpression(statement[1], scope) from ExecuteExpression(statement[2], scope)
				raise ExecuteExpression(statement[1], scope)
			raise

		elif stType == 17:
			for entry in statement[1]:
				scope.triggerGlobal(entry)

		elif stType == 18:
			for entry in statement[1]:
				scope.triggerNonlocal(entry)

		elif stType == 3:
			if statement[1]:
				return ReturnStatement("Return", ExecuteExpression(statement[1], scope))
			else:
				return ReturnStatement("Return")

		elif stType == 20:
			pass #Do literally nothing

		elif stType == 21:
			return ReturnStatement("Break")

		elif stType == 22:
			return ReturnStatement("Continue")

		elif stType == 10:
			if ExecuteExpression(statement[1], scope):
				return ExecuteStatList(statement[2], scope)
			else:
				return ExecuteStatList(statement[3], scope)

		elif stType == 9:
			while ExecuteExpression(statement[1], scope):
				out = ExecuteStatList(statement[2], scope)
				if out != None:
					if out.Type == "Break":
						break
					elif out.Type == "Continue":
						continue
					else:
						return out
			else:
				return ExecuteStatList(statement[3], scope)

		elif stType == 8:
			iterRange = ExecuteExpression(statement[2], scope)
			for value in iterRange:
				Assign(statement[1], value, scope)
				out = ExecuteStatList(statement[3], scope)
				if out != None:
					if out.Type == "Break":
						break
					elif out.Type == "Continue":
						continue
					else:
						return out
			else:
				return ExecuteStatList(statement[4], scope)

		elif stType == 11:
			toExit = []
			for item in statement[1]:
				out = ExecuteExpression(item[0], scope)
				out.__enter__()
				toExit.append(out)
				if item.optional_vars:
					storeAs = ExecuteExpression(item[1], scope)
					#If this isnt a name expr, then uh, good luck!
					scope.setVar(storeAs, out)
			try:
				out = ExecuteStatList(statement[2], scope)
			except BaseException as exc:
				for item in toExit:
					item.__exit__()
				raise exc
			else:
				for item in toExit:
					item.__exit__()
				return out

		elif stType == 13:
			try:
				out = ExecuteStatList(statement[1], scope)
				if out != None:
					return out
			except ExecutorException as exc: #Executor errors are not to reach the source code ever
				raise exc
			except BaseException as exc:
				for handler in statement[2]:
					if handler[1] == None or isinstance(exc, ExecuteExpression(handler[1], scope)):
						subScope = VariableScope(scope, "asclause")
						if handler[2]:
							subScope.setVarRaw(handler[2], exc)
						out = ExecuteStatList(handler[3], subScope)
						if out != None:
							return out
						break
			else:
				out = ExecuteStatList(statement[2], scope)
				if out != None:
					return out
			finally:
				return ExecuteStatList(statement[4], scope)

		elif stType == 15:
			for name in statement[1]:
				target, storedName = name[1], name[2]
				out = __import__(target, globals(), locals(), [], 0)
				if storedName:
					for term in target.split(".")[1:]: #This looks scary but I think it's valid
						out = out.__dict__[term]
					scope.setVar(storedName, out)
				else:
					scope.setVar(target, out)
				#The above code is incredibly confusing, but feels accurate.
				#E.g. import urllib.parse imports urllib, while import urllib.parse as y imports just urllib.parse (as y)

		elif stType == 16:
			module = statement[1]
			for name in statement[2]:
				target, storedName = name[1], name[2]
				if target == "*":
					out = __import__(module, globals(), locals(), [], statement[3])
					for term in module.split(".")[1:]:
						out = out.__dict__[term]
					for term in dir(out):
						scope.setVar(term, out.__dict__[term])
				else:
					out = __import__(module, globals(), locals(), [target], statement[3])
					out = out.__dict__[target]
					scope.setVar(storedName or target, out)

		elif stType == 0:
			def FunctionHandler(*args, **kwargs):
				subScope = VariableScope(scope, "function")
				HandleArgAssignment(subScope, statement[2], statement[1], args, kwargs)
				out = ExecuteStatList(statement[3], subScope)
				if out != None:
					if out.Type == "Break" or out.Type == "Continue":
						raise SyntaxError(f"'{out.Type}' outside loop")
					else:
						return out.Data
			FunctionHandler.__name__ = statement[1]
			FunctionHandler.__qualname__ = statement[1] #Technically a bit wrong but eh
			FunctionHandler = ImplementObjectDecorators(FunctionHandler, statement[4], scope)
			scope.setVar(statement[1], FunctionHandler)

		elif stType == 1:
			async def FunctionHandler(*args, **kwargs):
				subScope = VariableScope(scope, "function")
				HandleArgAssignment(subScope, statement[2], statement[1], args, kwargs)
				out = ExecuteStatList(statement[3], subScope)
				if out != None:
					if out.Type == "Break" or out.Type == "Continue":
						raise SyntaxError(f"'{out.Type}' outside loop")
					else:
						return out.Data
			FunctionHandler.__name__ = statement[1]
			FunctionHandler.__qualname__ = statement[1] #Technically a bit wrong but eh
			FunctionHandler = ImplementObjectDecorators(FunctionHandler, statement[4], scope)
			scope.setVar(statement[1], FunctionHandler)

		elif stType == 2:
			bases = tuple([ExecuteExpression(entry, scope) for entry in statement[2]])
			keywords = {}
			for entry in statement[3]:
				keywords[entry[1]] = ExecuteExpression(entry[2])
			class DummyClass(*bases, **keywords): #This is legal, wow. Thanks python!
				pass
			DummyClass.__name__ = statement[1]
			DummyClass.__qualname__ = statement[1]
			subScope = ClassScope(scope, DummyClass) #Custom class subscope
			out = ExecuteStatList(statement[4], subScope) #We shouldn't end early, period
			if out != None:
				raise SyntaxError(f"Now that is just illegal class logic, I don't even know what to say anymore")
			DummyClass = ImplementObjectDecorators(DummyClass, statement[5], scope)
			scope.setVar(statement[1], DummyClass)

		else:
			raise ExecutorException(f"[!] Unimplemented statement type {stType}")

	def ExecuteStatList(statList, scope):
		for statement in statList:
			out = ExecuteStatement(statement, scope)
			if out != None: #Send off our return/break/continue statement
				return out

	#Who doesn't love "for x,*y in z:" being a valid statement that you have to accomodate for!
	def Assign(target, value, scope):
		targetType = target[0]
		if targetType == 20:
			scope.setVar(ExecuteExpression(target, scope), value)
		elif targetType == 17:
			setattr(ExecuteExpression(target[1], scope), target[2], value)
		elif targetType == 18:
			ExecuteExpression(target[1], scope)[ExecuteExpression(target[2], scope)] = value
		elif targetType == 22 or targetType == 21:
			if not hasattr(value, "__iter__"):
				raise TypeError(f"cannot unpack non-iterable {type(value)} object")
			iterator = value.__iter__()
			for i in range(len(target[1])):
				item = target[1][i]
				if item[0] == 19:
					offset = len(value)-len(target[1])
					for lower in range(i):
						Assign(target[1][lower], iterator.__next__(), scope)
					scope.setVar(ExecuteExpression(item, scope), [iterator.__next__() for i in range(i, offset+i+1)])
					for upper in range(i+1,len(target[1])):
						Assign(target[1][upper], iterator.__next__(), scope)
					return
			# No starred expression, do normal stuff
			if len(target[1]) < len(value):
				raise ValueError(f"not enough values to unpack (expected {len(target[1])}, got {len(value)})")
			elif len(target[1]) > len(value):
				raise ValueError(f"too many values to unpack (expected {len(target[1])})")
			else:
				for i in range(len(target[1])):
					Assign(target[1][i], iterator.__next__(), scope)
		elif targetType == 19:
			raise SyntaxError("starred assignment target must be in a list or tuple")
		else:
			raise ExecutorException(f"Unable to assign to unrecognised type '{targetType}'")

	#def f2(x, y, z=None, *, a, b, c=None, **k):
	#	print('Cool')
	def HandleArgAssignment(scope, astArgs, representation, args, kwargs):
		"""
		General handler for assigning arguments into an executable body
		This gets messy incredibly fast
		Note: When handling kwarg defaults, its given as a list like [None, None, Constant()],
		but for the posargs, its just a list with no 'None's. Turns out that once a single positional arg is optional,
		all the posargs after that have to be optional too, which explains the weird behaviour.
		"""

		#Setup
		assignedNames = {}
		wantedPositionals = {}
		for pa in astArgs[1]:
			wantedPositionals[pa[0]] = True
		wantedKeywords = {}
		for kwa in astArgs[3]:
			wantedKeywords[kwa[0]] = True

		posargCollector = []
		kwargCollector = {}

		#Positional defaults
		defaultOffset = len(astArgs[1])-len(astArgs[6])
		for i in range(len(astArgs[6])):
			scope.setVar(astArgs[1][i+defaultOffset][0], ExecuteExpression(astArgs[6][i], scope.Parent))
			wantedPositionals[astArgs[1][i+defaultOffset][0]] = False

		#Input positionals
		for i in range(len(args)):
			if i < len(astArgs[1]):
				scope.setVar(astArgs[1][i][0], args[i])
				assignedNames[astArgs[1][i][0]] = True
				wantedPositionals[astArgs[1][i][0]] = False
			else:
				posargCollector.append(args[i])

		#kwarg defaults
		for i in range(len(astArgs[4])):
			default = astArgs[4][i]
			if default != None:
				kw = astArgs[3][i]
				scope.setVar(kw[0], ExecuteExpression(default, scope.Parent))
				wantedKeywords[kw[0]] = False

		#Input kwargs
		for key, value in kwargs.items():
			if key in assignedNames:
				raise TypeError(f"{representation}() got multiple values for argument '{key}'")
			if key in wantedPositionals:
				scope.setVar(key, value)
				assignedNames[key] = True
				wantedPositionals[key] = False
			elif key in wantedKeywords:
				scope.setVar(key, value)
				assignedNames[key] = True
				wantedKeywords[key] = False
			else:
				kwargCollector[key] = value

		#Final processing and error check
		missing = []
		for key, wanted in wantedPositionals.items():
			if wanted == True:
				missing.append(key)
		if len(missing) > 0:
			raise TypeError(f"{representation}() missing {len(missing)} required positional {GoodGrammar(missing, 'argument:', 'arguments:')}")
		missing = []
		for key, wanted in wantedKeywords.items():
			if wanted == True:
				missing.append(key)
		if len(missing) > 0:
			raise TypeError(f"{representation}() missing {len(missing)} required keyword-only {GoodGrammar(missing, 'argument:', 'arguments:')}")
		if astArgs[2]:
			scope.setVar(astArgs[2][0], posargCollector)
		elif len(posargCollector) > 0:
			raise TypeError(f"{representation}() received too many positional arguments")
		if astArgs[5]:
			scope.setVar(astArgs[5][0], kwargCollector)
		elif len(kwargCollector) > 0:
			raise TypeError(f"{representation}() received too many keyword arguments")

	def ImplementObjectDecorators(obj, decorators, scope):
		for i in range(len(decorators)-1, -1, -1): #Traverse in reverse order
			decorator = ExecuteExpression(decorators[i], scope)
			obj = decorator(obj)
		return obj

	#x = ["A", "DD", "B", "CCBC"]
	#print([S+str(ord(C)) for S in x if S != "A" for C in S if C != "B"])
	def ParseGenerators(generators, toEvaluate, scope):
		"""
		General generator handler, handling nested for and if statements
		This should always be passed a subscope to avoid complications
		"""
		def RecursiveHandle(generators, i):
			gen = generators[i]
			iterator = ExecuteExpression(gen[1], scope)
			storage = ExecuteExpression(gen[0], scope)
			combinations = []
			for term in iterator:
				scope.setVar(storage, term)
				ShouldEvaluate = True
				for condition in gen[2]:
					if not ExecuteExpression(condition, scope):
						ShouldEvaluate = False
						break
				if ShouldEvaluate:
					if i == len(generators)-1: #Last generator
						for term in toEvaluate:
							combinations.append([ExecuteExpression(out, scope) for out in toEvaluate])
					else:
						combinations.extend(RecursiveHandle(generators, i+1))
			return combinations
		return RecursiveHandle(generators, 0)

	# At this point we parse the obfuscated AST
	def DeserializeAST():
		def padleft(s,n,p):
			return p*(n-len(s))+s
		def ToBit(n, pad=1):
			if n == 0:
				return "0"*pad
			power = math.floor(math.log(n,2))
			final = ""
			while True:
				p2 = 2**power
				if n >= p2:
					n = n - p2
					final = final + "1"
				else:
					final = final + "0"
				power = power - 1
				if power < 0:
					return padleft(final,pad,"0")
		def DBitToNum(dbits):
			power = 0
			result = 0
			for bit in dbits:
				if bit == "1":
					result = result + 2**power
				power = power - 1
			return result
		# We use a simplified, unclassed, and minimised version of our bitreader
		Buffer = ""
		BufferPosition = 0
		def EnsureBufferHasData(bits):
			nonlocal BufferPosition
			nonlocal Buffer
			for i in range(math.floor((bits-len(Buffer)-1)/6+1)):
				Buffer = Buffer + ToBit(ord(code[BufferPosition])%64,6)
				BufferPosition = BufferPosition + 1
		def ReadRaw(bits):
			EnsureBufferHasData(bits)
			nonlocal Buffer
			DataOut = Buffer[0:bits]
			Buffer = Buffer[bits:]
			return DataOut
		def Read(bits):
			return int(ReadRaw(bits),2)
		def ReadByte():
			return chr(Read(8))
		def ReadDouble():
			sign, Exponent, Mantissa = Read(1), Read(11), ReadRaw(52)
			sign, Exponent = (sign==0 and 1 or -1), 2**(Exponent-1023)
			return sign * Exponent * DBitToNum("1"+Mantissa)

		TYPE_LIST_START=0
		TYPE_LIST_END=1
		TYPE_NONE=2
		TYPE_STRING=3
		TYPE_DOUBLE=4
		TYPE_BOOLEAN=5
		TYPE_INT=6
		TYPE_INT_HALF=7

		TYPE_WIDTH=3

		def deserializeloop(AssertCheck=True):
			if AssertCheck:
				assert Read(TYPE_WIDTH) == TYPE_LIST_START
			Output = []
			while True:
				ObjType = Read(TYPE_WIDTH)
				if ObjType == TYPE_LIST_START:
					Output.append(deserializeloop(False))
				elif ObjType == TYPE_LIST_END:
					return Output
				elif ObjType == TYPE_NONE:
					Output.append(None)
				elif ObjType == TYPE_STRING:
					Result = ""
					while True:
						NextByte = ReadByte()
						if NextByte == "\0":
							break
						elif NextByte == "\\":
							Result = Result + ReadByte()
						else:
							Result = Result + NextByte
					Output.append(Result)
				elif ObjType == TYPE_DOUBLE:
					Output.append(ReadDouble())
				elif ObjType == TYPE_BOOLEAN:
					Output.append(Read(1)==1 and True or False)
				elif ObjType == TYPE_INT:
					Output.append(Read(8))
				elif ObjType == TYPE_INT_HALF:
					Output.append(Read(4))
		return deserializeloop()

	finalCode = DeserializeAST()
	def __main__():
		scope = VariableScope(None, "core")
		debugprint("Input code:",code)
		try:
			out = ExecuteStatList(finalCode[0], scope)
		except BaseException as exc:
			if _DEBUG:
				debugprint("[!] We ran into a critical error")
				if _DEBUG_LastExpr:
					debugprint("Last expression:",_DEBUG_LastExpr)
				else:
					debugprint("Last expression: None")
				if _DEBUG_LastStatement:
					debugprint("Last statement:",_DEBUG_LastStatement)
				else:
					debugprint("Last statement: None")
			raise exc
		else:
			if out:
				if out.Type == "Break" or out.Type == "Continue":
					raise SyntaxError(f"'{out.Type}' outside loop")
				else:
					raise SyntaxError(f"'{out.Type}' outside function")

	return __main__


rawdata = r"@FDq}FEFxNSRwN`@LMD`XPZProIB@HdDdXSGtXT[`yMK\z@@ptRAaBdRBRLIczLJMp\fen]@@XZI@p`u]hXWP⌂@DRAaelk\r@@p`tESQ@h@QIIFDq}FEFxNSRwN`@LMD`_Qt@HC^@AHaoH@dR`PTBQrFEF|`@ptROAFEF|@@ptRRAaCjbLH]dRBRBRNhFE`LJMx@AaldXT[r@CCYHphwh@FFrRLMddXV@pauQFDNrHpawQHptRbLIczLJMp\fen]@@XZI@yCBc^@@XZIGhcBGIDdDd\Fs@@A@[ByLx@dPvErYsH@RIPHJAHFDq}FEFxNSRwN`@LMD`XT[ByLyd@CCQHphvErYp@XZIHIHycBpGHXT[ByLyd@CCQH{DXT[ByLx@LMDd\aaQlKds`@ptROAFEFpnSNY@@ptRRLMDd`iD\FsFP@@PF|@BQC^PAHaoP@dR`Fp`BQCX`AHalX@dRDaaAHdMk@DbAaAHd`XSGtXT[`yMK\z@@ptRAaQo@@LMDcBc^P@XZIFEF}@@ptRLHMs\Fqi]@ADXT[B@CCQHphvH@FFbQaQlX@LMDcBC\wAlZWP@QFEFu`@ptRPRRBdQaL_QaQlqd@CCQHFDNJHparQHFFV}@@paZIFFVp`@pauQHprvH@FDNrIFFVx@@paxQHprvL@FDNzIIHpfOhphwArZVyt@AahdCBC[wUtOPAD_QaQlp@LMD`XPyHcBGMD`d`dcduQe\wP@AA@xM_WvenZWQ_Wp@@`MsYVqf@DbF{@BQJAAPI@z`XQFEFylkXs@@ptRwd@FFrRLJMv@AaheD`iD\Fslkh|`@A@[frmcL@IDhDE@dCfLHcBc\vUlY`@XZI[r@CCQIHJQHIFDq}FEFxNSRwN`@LMD`XPZhrn[h^`BHphuQe\wP@FFbRBRNhFEFjLkfzI{Du@@pvRQ}FEFjLkfz@@ptRAaCpbPRbLIczLJMp\fen]@@XZI@p`uQe\wQOXfh}@DQaQjcJynb^qMP@LMDdDdXSGtXT[`yMK\z@@ptRAaAjb^WL{JzOIPTgh@bOhpbLJMTYWMtSvIj@Aahel{JzOH@LMD``d`dclLHcBcUFUs]D}bZ`@XZI[r@CCYHzDXP⌂hdXSGtXT[`yMK\z@@ptRAaAjb^WL{JzOIPTgh@bOhpbLJMTYWMtSvIj@Aahel{JzOH@LMD``d`dcjAaQo@@LMdd]`LHMA@DQaClbPLH]TQaBtRTQu@phwd@FFrRNpPpaxQHFEF|@@ptRLHVbRbNhFEF}@@pvRQvBPp`tD@QHFEF|`@ptRLJMx@AahdXPxhdhcBX~cBc\GIi[gP@FFbPLJMx@AahdXT[r@CCQHphwh@FFbRBRNhFEF|@@pvRQaTCBCPPADXPZHb@BHp`tH@QFDFahZDa`BIFFbTQaL_QaQnCdtmsh@CCQHFDFfMKfzH[^vn@@bPRQu@phv}bZ`@X[IH|CdLJMS@Aahd]BOhphwMt\`@XZI@~cBc[wId@AahdCBcPp@XZIHIHIH@phuL@FFrQaQo@@LMD`_AaQjX@LMD`XRI@p`tD@QIO@`XTZF@CCYHphuL@FFbPO`phtL@FFbPLID`XPZD@Hdg`RTQaL_QaQnCdtmsh@CCQHGtXT[h|nCJ@CCQHFEFwlST@CCQIAHphv}bZ`@XZIHIHpfOhphwArZVyt@AahdCBCTvUtPv}m\@AD`dcjAaQm{Du@@pvRQyGHXTZf@CCQHzD_QaQn[hy@@ptRA}FEFwnSH@CCQHFEFa`@ptRPRPRPAaQjX@LMdcBc^@@XZI@~CBcTp@XZI@pdRAaAhH@bR^A@phtL@FFrQaQjX@LMD`_AaQhX@LMD`XRI@p`tH@QIO@dhcBX~cBc\GIi[gP@FFbPOhphwQy\FT@FFbPLJMoXfh@FFbRBQaQm{Du@@ptRPRQaL_QaQnCdtmsh@CCQHFDFbMKFzH[^vn@@bPRQu@phv}bZ`@X[IH}CdLJMS@Aahd]BOhphwMt\`@XZI@~cBc[wId@AahdCBcPp@XZIHIHIHphuL@FFbPAaQjX@LMdcBc^@@XZI@~CBcTp@XZI@pdRAaAhH@bR^A@phtL@FFrQaQjX@LMD`_AaQhX@LMD`XRI@p`tH@QIO@dhcBX~cBc\GIi[gP@FFbPOhphwQy\FT@FFbPLJMoXfh@FFbRBQaQm{Du@@ptRPRQaL_QaQnCdtmsh@CCQHFDFclk\rnSBzM{d@HdDd]PLJMgYVx@FFrROXyCBcTp@XZIGPczLJMs]GH@FFbPOhphv}rY@@XZI@phtL@FFbRBRBR@LJMS@AaldXT[p@CCQHGpXTZf@CCQHFDbPLHMA@DRSpHFEFa`@pvRLJMS@AahdCxLJMC@AahdCBQHFDFa@BIIxDeDXSGtXT[`yMK\z@@ptRA}FEFzOK`r`@ptRAaQl{Jw@@ptRPRLJMgYVx@FFbRBRO@phwX@FFrQaQl{Jw@@ptRAaL_QaQnCdtmsh@CCQHFDFclk\PM{DuLkFzDCJwNcd|`BHphwX@FFbRBRPTQp[HX`@A@[^qMP@dR`PTBPLIczLJMp\fen]@@XZI@p`tao[vmi[f\`[vIjHFenHDPqKbxn@DRBRNC\fUt@@BB`PTBPLIczLJMp\fen]@@XZI@p`tPqHFao[vl`[vx@QFEFwlST@CCQIAIGL_QaQm{Du@@ptRBBRPTbNXphwIe]@@XZIIARHxMdL`@@`MoXfh@RIPHJAHFDq}FEFxNSRwN`@LMD`XPZPwm{VtmsNPM{DuDCRwDBHYEq\W@BIAIGAnSJz@@AAPHJAHFDq}FEFxNSRwN`@LMD`XPZHYDCPwm{VPM{\@HcBc[vIj@Aahd`dcfOhphv}bZ`@XZIAAIHJQGLXT[drn`@LMDd`iDXSGtXT[`yMK\z@@ptRAaAhcJqm{dpnc^yNY@zLkfzDAb@HdDd\FzLkfz@@AAPHJAHFDq}FEFxNSRwN`@LMD`XPZhtMKfPMKfPNcJyn`@bPRQsFDKQI@phvPq@AahdXT[HY@@ptRTbLIczLJMp\fen]@@XZI@p`v}u]Ct@QGtXT[hrn[h@CCQHHIHIHpfOhphwArZVyt@AahdCBCUFUs]BApXWItHCH@QHIHyMtYWMtL`@DDCBX~cBc\GIi[gP@FFbPLHME^FUc]WQi[f\`Xf}d^RAoYbAtYWMt@DRBRPLJMdLP@XZIFEFrFP@LMDdcBX~cBc\GIi[gP@FFbPLHMR]VynZVygHGQe\wP@QHIHpfOhphwArZVyt@AahdCBC[wUtOPAD_QaQncJynad@CCQHHIHIHpfOhphwArZVyt@AahdCBCQFUc[wIa]F}r\rAtYWMtHFQo[fT@QHIHpfOhphwArZVyt@AahdCBCRVYE^G@q@DQuFDKQFDNJHzcBEhcBGID]QaBtQaCfbLH]DRRPRQaL_QaQnCdtmsh@CCQHFDFdlrJ|NAd@HcjLHVbLH\TQuFDJQFDNRHzcBEhcBGMDXPzHdd`dcBX~cBc\GIi[gP@FFbPLHMIYdUx\CL@QGTXPiD]QaBtQaCbbLH\dRNhpaZHpasQFDNbIIAIGTCBc^@@X[IHpjAaCdbLH\tQaChbQaheD]PLJMy@AaldclAaCdbLH]DRAaCfbLH]TRTQu@phwdr@AaldclAaAlk\r@BI@p`tE\WFx@QIQFDq}FEFxNSRwN`@LMD`XPxhcBhFEF|@@ptRQahdXPzhdDdXSGtXT[`yMK\z@@ptRAaCbbLIcBhFEF|@@ptRQahdXZIFDNjIAIFDq}FEFxNSRwN`@LMD`XPxhcBc^P@XZIFDNrIAIFDq}FEFxNSRwN`@LMD`XPxhcBXphwd@FFbQahdXP{HdDdXSGtXT[`yMK\z@@ptRAaCbbLH\dQaQoId@CCQIAIFDq}FEFxNSRwN`@LMD`XPxhcBGIDXSFEF|fP@LMDcCQIAIFDq}FEFxNSRwN`@LMD`XPxhcBGID`XYPphwdr@Aahddd]PLK@XT[\@CCYHpfLJMy@AaldX[IFEFw@@pvRQaldcBpFDNJHparQFDNZIFFbTQaL_QaQnCdtmsh@CCQHFEFw@@ptRLJMy@Aahd`dcjAaXCBcXP@X[IFDqaQoH@LMdcCYHphvH@FFrQaQlX@LMddX[IHplAaCbbLH\dQaCfbLH]DQaCjbQaheDXSGtXT[`yMK\z@@ptRAaQlH@LMDcBc^P@XZIFEFq@@ptRLJMc@Aahd`dcjAaXCBcXP@X[IFEFq@@pvRLIcBc^P@X[IFFrQaQlX@LMddX[IHplAaCbbLH\dQaCfbLH]DQaCjbQaheDXSGtXT[`yMK\z@@ptRAaQlH@LMDcBcX`@XZIFEF|`@ptRLJMc@Aahd`dcjAaQo@@LMddXU@pauQFDKQHptRbNhFEF|`@pvRQaTCBXphw`@FFbQahdXP|HdXZIQGTCBc^`@X[IHpjAaLXT[r@CCQHptRLIcBc^@@XZIFFbQaAhH@bLH\TRLMDhcBX~cBc\GIi[gP@FFbPLHMx@DQaQo@@LMDcBC^PADXT[r@CCQHp`wh@QFEF}@@ptRPRQaL_QaQnCdtmsh@CCQHG|CBCPPAD_arFEF|@@ptRNqFDNZIe⌂x@@@@@@@@HcBCP`AD_aaQoH@LMDr⌂|@@@@@@@@DQaAhX@bOpyCBc^`@XZIGXcBGIDr⌂|@@@@@@@@DRRBRNhFEFq@@pvRQaBH@k]OCXQM@TbbLIczLJMp\fen]@@XZI@⌂`_aaQlP@LMDr⌂|@@@@@@@@A⌂@p`sHnLpADdddDd]PLJMx@AaldcBGEED\F|`@AAPHJAHFDPwT@IGTCBc^@@X[IHparQQGInh@BBAaDMw@BQp[n@@DAn[JvLp@dR`PTBPLIczLJMp\fen]@@XZI@p`w\@QFEFylkXs@@ptRPRQaL_QaQo@@LMD`XPyHdDdXSGtXQFEFz`@ptRv|@FFbPLH\tRBRPTbLIAo@@RNC^@@@`MsYVqf@DbTBB`RAaL_QaQnCdtmsh@CCQHFDF|@BHphwMe[FX@FFbRBRPTbNC[p@@`MsYVqf@DbTBB`RAaL_QaQnCdtmsh@CCQHFDFw`BHphwMe[FX@FFbRBRPTbPRPTbLIczLJMy@AahdDDdXSGtXT[`yMK\z@@ptRAaAniz@HcBc]P@XZIHIHpfOhphwArZVyt@AahdCBC^Ct@QFEF|@@ptRPRQaL_QaQnCdtmsh@CCQHFDF{gh@bLJMw@Aahd`dcBX~cBc\GIi[gP@FFbPLHMuKg\`YWai\wQsOpAD_QaQmCBylKhzNP@LMD`XT[j@CCQHp`w\@QHIHIHpfOhphwArZVyt@AahdCBC]RyxHFUxZWMt\s|@QGtXT[Ppn[BzNcd@CCQHFEFz`@ptRLHMx@DRBRBRLIczLJMp\fen]@@XZI@p`wTn[rAe^Fes]GL⌂@DQ}FEFtLKfpnchy@@ptRAaQnh@LMDcBC[pAD`d`dcBX~cBc]p@XZI@paqQHIHz`XT[B@CCYIFDNJJH{cBcX`@X[IFEFtmsh@CCQHp`sH@QxdXSGtXT[`yMK\z@@ptRAaQlH@LMDczLJMt^WAe@AahdCBcXP@XZIHIHIHpfOhphwArZVyt@AahdCBcX`@XZIGtXT[h|nCJ@CCQHFEFq@@ptRPRPRQwFEFq`@pvRLJMi[gP@FFbUqH⌂`Mi[WAtYWMt@F|OH@RQaL_QaQnCdtmsh@CCQHFEF|OH@LMDdDdXSGtXT[`yMK\z@@ptRAaDXT[p|`@ptRu}_YFec]E}_@Aahd`dcBCZVup]FUs]Byi[WAtYWMtWvYi[FTr@@FU@BIxDXSGtXT[`yMK\z@@ptRAaQoCp@CCQIAIFDFtmk`zLkfz@@CZVup]FUs]E}fZVqeL`AlQd@DMi[WAtYWMtL`Amk^r@AO@cBX~cBc\GIi[gP@FFbPLHMi[WAtYWMtWvYi[FTrOPADXT[DY@@ptRLHMi[WAtYWMtLct@QFEFvm{H@CCQIAIFDFtmk`zLkfzEsRvnChrn[hY@@CZVup]FUs]E}s]VIfZVqe@DSpHpfOhphwArZVyt@AahdCBCZVup]FUs]E}s]VIfZVqeOPADXT[RvnChrn[hon[jqLsRvLh@LMDdDd]PLJMx@AaldclAaCbbLH\tRAaCdbLH]DRTQxFEF|`@pvRLJMx@AahdCBX~cBc\GIi[gP@FFbPLHMf^PADXT[r@CCQIAIHJH|CBpFEF|`@pvRLIcBc^`@X[IFFrRLMdczLHcBc^@@XZI[RzLkZy`@ptRBBPLIczLJMp\fen]@@XZI@p`vYy^`ADXT[r@CCQHphwh@FFbRBRPTQu@plAaQlH@LMdcBXphvH@FFrQaldXT[F@CCYIFFrRLHMTYWMtZVyg@DTQaL_QaQnCdtmsh@CCQHFDNJHphvD@FFbQaQlP@LMDcBcXp@XZIHIHz`XV@phvD@FFrQaLXT[D@CCYHpvRLJMc@AaldcCYIFEPLH\TQaCdbLH\tQaChbLH]TRLMDhcBX~cBc\GIi[gP@FFbPLH\dQaQlH@LMDcBcX`@XZIFEFq`@ptRPRQu@plAaQlH@LMdcBXphvH@FFrQaldXT[F@CCYIFFrRLK@XPxhcBGIDXPyhcBGQDXPzhdXZIQFDq}FEFxNSRwN`@LMD`XPyhcBcXP@XZIFEFq@@ptRLJMc@Aahd`dcjAaXCBcXP@X[IFDqaQlP@LMdcCYHphvL@FFrRLMdd]pLH\TQaCdbLH\tQaChbLH]TRTQaL_QaQnCdtmsh@CCQHFDNbHphvD@FFbQaQlP@LMDcBcXp@XZIHIHz`XV@phvD@FFrQaLXT[D@CCYHpvRLJMc@AaldcCYIGXCBGEDXPyhcBGUDXP{hcBGeD`XPyHcBGQDXP{HcBGaDXP}HdhcBX~cBc\GIi[gP@FFbPLH]TQaQlH@LMDcBcX`@XZIFEFq`@ptRPRQu@plAaQo@@LMdcBc^P@X[IFEF}@@pvRQaldclAaCbbLH]DQaCnbPLH\dQaCjbLH^DRTQaL_QaQnCdtmsh@CCQHFEF|@@ptRLJMy@AahdXT[t@CCQIAIHI@"
rawdata = r"@FDq}FEFxNSRwN`@NAHFDFkm{BtDH@bPRQu@phw`@GDdcBhFDF``BHp`tQD@DQaAhP@bLHMCPtIC@DRNAJHpfOhphwArZVyt@ApI@p`tqi\wQC[vup@DRBRNhFEFwlST@CbRQxGHXTZf@C`RNaGtXT[fzNP@NAHGtXT[^yL`@NAHFEFa`@xDdDdDd@XTZf@CbRLJMx@ApI@~CBcTp@\BPLID`XPZB@Hdg`PLJMC@AqIFEFi`@xD`_AaQhX@NAHFDbPLHMB@DRSpIJHpfOhphwArZVyt@ApI@~cBc]GepYP@\BPLJMoXfh@G@d`dXT[^qMP@NAIAIFDq}FEFxNSRwN`@NAHFDFilkham{Zx@BIAIGTCBc[vIj@AqIH|cdLJMS@ApIGPczLJMs]GH@G@dCzLJMo\fP@G@dCBcPp@\BRBRBR@LJMS@AqIFEF|@@xD`_AaQjX@NAHFDbPLHMA@DRSpHFEFa`@xdcBcTp@\BPO`phtL@G@dCBQHFDFa@BIIxDeDXSGtXT[`yMK\z@@xD`_QaQncrxLh@NAHFEFwlST@C`RPRLJMoXfh@G@d`dcBX~cBc\GIi[gP@G@dCBCQFec]DMo[W@@QHIHz`XT[^qMP@NIIGh\aaQjX@NAHzD_QaQn[hy@@xD`_QaQm{dr@@xD`XTZF@C`RPRPRQaQjX@NAH@phuL@GDdXT[p@C`RA|FEFi`@xD`XRI@p`tD@QIO@`XTZF@CbRLJMS@ApI@~CBcPp@\BPLID`XPZD@Hdg`RTQaL_QaQnCdtmsh@C`RA}FEFzOK`r`@xD`XT[^qMP@NAIAHphv}bZ`@\BRBRLIczLJMp\fen]@@\BPLHMGYVye\fEt[wH@QHIHz`XT[Nrmp@NIIGl\aaQjX@NAHzD_QaQn[hy@@xD`_QaQm{dr@@xD`XTZF@C`RPRPRPAaQjX@NIHphw`@G@dCxLJMS@ApI@pdRAaAhH@bR^A@phtL@GDdXTZf@C`RA|FEFa`@xD`XRI@p`tH@QIO@dhcBX~cBc\GIi[gP@G@dCzLJMt^WAe@ApI@phv]e[`@\BRBQaQl{Jw@@xDdDd^AaQnp@NIHphv]e[`@\BPLIczLJMp\fen]@@\BPLHMGYVx`[vIjYVMtHFUn]GIy@DQaQnp@NAIAIHJIAH"
# Sublime text throws a hissy fit when colouring the above if you use single quotes but thats perfectly legal - just clarify r (raw) for safety

debugprint("Generating execution loop")
out = CreateExecutionLoop(rawdata)
debugprint("Executing execution loop")
out()
debugprint("Finished execution loop")

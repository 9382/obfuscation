import ast

Statements = [
	ast.FunctionDef,
	ast.AsyncFunctionDef,
	ast.ClassDef,
	ast.Return,

	ast.Delete,
	ast.Assign,
	ast.AugAssign,
	ast.AnnAssign,

	ast.For,
	# ast.AsyncFor, # Unimplemented!
	ast.While,
	ast.If,
	ast.With,
	# ast.AsyncWith, # Unimplemented!

	ast.Raise,
	ast.Try,
	ast.Assert,

	#ast.Match, # Unimplemented!

	ast.Import,
	ast.ImportFrom,

	ast.Global,
	ast.Nonlocal,
	ast.Expr,
	ast.Pass,
	ast.Break,
	ast.Continue,
]

Expressions = [
	ast.BoolOp,
	ast.NamedExpr,
	ast.BinOp,
	ast.UnaryOp,
	ast.Lambda,
	ast.IfExp,
	ast.Dict,
	ast.Set,
	ast.ListComp,
	ast.SetComp,
	ast.DictComp,
	ast.GeneratorExp,

	# ast.Await, # Unimplemented!
	# ast.Yield, # Unimplemented!
	# ast.YieldFrom, # Unimplemented!

	ast.Compare,
	ast.Call,
	ast.FormattedValue,
	ast.JoinedStr,
	ast.Constant,

	ast.Attribute,
	ast.Subscript,
	ast.Starred,
	ast.Name,
	ast.List,
	ast.Tuple,

	ast.Slice,
	ast.Index,
	# ast.ExtSlice, # Unimplemented!

	# Not strictly expressions but who cares
	ast.keyword,

	ast.Load,
	ast.Store,
	ast.Del,
]

Operators = [
	# UnaryOp
	ast.Invert,
	ast.Not,
	ast.UAdd,
	ast.USub,
	# BinaryOp
	ast.Add,
	ast.Sub,
	ast.Mult,
	ast.MatMult,
	ast.Div,
	ast.Mod,
	ast.Pow,
	ast.LShift,
	ast.RShift,
	ast.BitOr,
	ast.BitXor,
	ast.BitAnd,
	ast.FloorDiv,
	# Compare
	ast.Eq,
	ast.NotEq,
	ast.Lt,
	ast.LtE,
	ast.Gt,
	ast.GtE,
	ast.Is,
	ast.IsNot,
	ast.In,
	ast.NotIn,
]

UntypedTypes = [
	ast.Module,
	ast.arguments,
	ast.alias,
]

StatementToID = {}
ExpressionToID = {}
OperatorToID = {}
for itemList, referenceList, name in [
	[Statements, StatementToID, "statement"],
	[Expressions, ExpressionToID, "expression"],
	[Operators, OperatorToID, "operator"]]:
	i = 0
	for item in itemList:
		referenceList[item] = i
		i += 1
	print("Highest " + name + " ID:", i-1)

class BitWriter:
	pass

class BitReader:
	pass

def serialize(Object):
	def simplify(Object):
		objtype = type(Object)
		if isinstance(Object, ast.AST):
			print(objtype, Object._fields)
			out = []
			if objtype in Expressions:
				out.append(ExpressionToID[objtype])
			elif objtype in Statements:
				out.append(StatementToID[objtype])
			elif objtype in Operators:
				out.append(OperatorToID[objtype])
			elif objtype in UntypedTypes:
				pass
			else:
				print("[!] I have no bloody idea how to handle the type of", objtype)
			# print(Object, objtype, Object._fields, Object._attributes)
			for field in Object._fields:
				out.append(simplify(getattr(Object, field)))
			return out
		elif objtype == tuple:
			return tuple(simplify(x) for x in Object)
		elif objtype == list:
			return list(simplify(x) for x in Object)
		elif objtype == set:
			return set(simplify(x) for x in Object)
		elif objtype == dict:
			out = {}
			for key in Object.keys():
				out[simplify(key)] = simplify(Object[key])
			return out
		else:
			# print(":(", Object, objtype)
			return Object
	def serialize(Object):
		print("I cant serialize yet")
		return Object
	return serialize(simplify(Object))


c = ast.parse(r"""
def x():
	print("Run a loop")
	yield 3
	print("Go again")
	yield 5
	return 8

print("Get x")
b = x()
print("Out:",b)
print("Next:",next(b))
print("Next:",next(b))
print("Next:",next(b))
""")
print(ast.dump(c))
print(serialize(c))
import math
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

# The below bit code is a lazy port of a lua version of this
def padleft(s,n,p):
	return p*(n-len(s))+s
def padright(s,n,p):
	return s+p*(n-len(s))

def ToBit(n, pad=1):
	if n == 0:
		return "0"*pad
	assert n%1 == 0
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
def DecToBit(d, pad=1):
	assert abs(d) < 1
	result = ""
	iterations = 0
	while True:
		NextNum = d * 2
		if NextNum > 1:
			result = result + "1"
		elif NextNum == 1:
			return padright(result + "1",pad,"0")
		else:
			result = result + "0"
		iterations = iterations + 1
		if iterations >= pad:
			return result
		d = NextNum - math.floor(NextNum)
def NormalizeScientific(bits):
	raw = bits.replace(".","")
	NotationOffset = bits.find("1")
	Normalized = bits[NotationOffset:NotationOffset+1] + "." + bits[NotationOffset+1:]
	print("pre-normal",bits,"post-normal",Normalized)
	Exponent = bits.find(".")-1-(NotationOffset-1)
	return Normalized,Exponent
def DBitToNum(dbits):
	power = 0
	result = 0
	for bit in dbits:
		if bit == "1":
			result = result + 2**power
		power = power - 1
	return result

class BitWriter:
	def __init__(self):
		self.Data = ""
	def Write(self, digit, strictWidth=None):
		BitRepresentation = ToBit(digit, strictWidth or 1)
		if strictWidth:
			assert len(BitRepresentation) <= strictWidth
		self.Data = self.Data + BitRepresentation
	def WriteString(self, string):
		for char in string:
			self.Data = self.Data + ToBit(ord(char),8)
	def WriteDouble(self, double):
		if double == 0:
			self.Data = self.Data + "0"*64
			return
		sign = (double < 0 and "1") or "0"
		double = abs(double)
		fractional, integral = math.modf(double)
		RequiredBuffer = 0
		if fractional != 0:
			RequiredBuffer = max(math.floor(math.log(1/fractional,2)),0)
		#Buffer is required should the default 53 bits not be enough data due to a large shift when normalizing the scientific.
		#AKA: If 1 does not appear as early as 0.1[...] then normalization fails due to lack of data - that's bad, so generate more
		IntegralBits, FractionalBits = ToBit(integral), DecToBit(fractional, 53+RequiredBuffer)
		NormalizedBits, Exponent = NormalizeScientific(IntegralBits + "." + FractionalBits)
		NormalizedBits = NormalizedBits[2:54]
		if len(NormalizedBits) != 52:
			print("[bitmanager] Precision lost during handling of double, missing", 52-len(NormalizedBits), "bits\nFractional:", fractional)
			NormalizedBits = padright(NormalizedBits,52,"0")
		Exponent = ToBit(Exponent+1023,11)
		print("sign",sign,"Exponent",Exponent,"Normalized",NormalizedBits)
		self.Data = self.Data + sign + Exponent + NormalizedBits
	def ToString(self):
		final = ""
		Data = self.Data
		DataScanner = 1
		while True:
			NextByte = "01" + Data[DataScanner-1:DataScanner+5]
			if len(NextByte) < 8:
				final = final + chr(int(padright(NextByte,8,"0"),2))
				break
			else:
				final = final + chr(int(NextByte,2))
				DataScanner = DataScanner + 6
		return final

class BitReader:
	def __init__(self, Data=""):
		self.Data = Data
		self.Position = 0
	def ReadRaw(self, bits):
		assert self.Position + bits <= len(self.Data)
		DataOut = self.Data[self.Position:self.Position+bits]
		print("Out?",DataOut)
		self.Position = self.Position + bits
		return DataOut
	def Read(self, bits):
		return int(self.ReadRaw(bits),2)
	def ReadByte(self):
		return chr(self.Read(8))
	def ReadDouble(self):
		sign, Exponent, Mantissa = self.Read(1), self.Read(11), self.ReadRaw(52)
		sign, Exponent = (sign==0 and 1 or -1), 2**(Exponent-1023)
		return sign * Exponent * DBitToNum("1"+Mantissa)
	def LoadBits(self, bitdata):
		self.Data = self.Data + bitdata
	def LoadString(self, strdata):
		for char in strdata:
			self.Data = self.Data + ToBit(ord(char)%64,6)

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
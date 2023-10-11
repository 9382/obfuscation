import math
import ast

Statements = [
	ast.FunctionDef, # 0
	ast.AsyncFunctionDef,
	ast.ClassDef,
	ast.Return,

	ast.Delete,
	ast.Assign, # 5
	ast.AugAssign,
	ast.AnnAssign,

	ast.For,
	# ast.AsyncFor, # Unimplemented!
	ast.While,
	ast.If, # 10
	ast.With,
	# ast.AsyncWith, # Unimplemented!

	ast.Raise,
	ast.Try,
	ast.Assert,

	#ast.Match, # Unimplemented!

	ast.Import, # 15
	ast.ImportFrom,

	ast.Global,
	ast.Nonlocal,
	ast.Expr,
	ast.Pass, # 20
	ast.Break,
	ast.Continue,
]

Expressions = [
	ast.BoolOp, # 0
	ast.NamedExpr,
	ast.BinOp,
	ast.UnaryOp,
	ast.Lambda,
	ast.IfExp, # 5
	ast.Dict,
	ast.Set,
	ast.ListComp,
	ast.SetComp,
	ast.DictComp, # 10
	ast.GeneratorExp,

	# ast.Await, # Unimplemented!
	# ast.Yield, # Unimplemented!
	# ast.YieldFrom, # Unimplemented!

	ast.Compare,
	ast.Call,
	ast.FormattedValue,
	ast.JoinedStr, # 15
	ast.Constant,

	ast.Attribute,
	ast.Subscript,
	ast.Starred,
	ast.Name, # 20
	ast.List,
	ast.Tuple,

	ast.Slice,
	ast.Index,
	# ast.ExtSlice, # Unimplemented!

	# Not strictly expressions but who cares
	ast.keyword, # 25
]

Contexts = [
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
ContextToID = {}
OperatorToID = {}
for itemList, referenceList, name in [
	[Statements, StatementToID, "statement"],
	[Expressions, ExpressionToID, "expression"],
	[Contexts, ContextToID, "context"],
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
	NotationOffset = raw.find("1")
	Normalized = raw[NotationOffset:NotationOffset+1] + "." + raw[NotationOffset+1:]
	print("pre-normal",bits,"post-normal",Normalized)
	Exponent = bits.find(".")-2-(NotationOffset-1)
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

def simplify(Object):
	objtype = type(Object)
	if isinstance(Object, ast.AST):
		# print(objtype, Object._fields)
		out = []
		if objtype in Expressions:
			out.append(ExpressionToID[objtype])
		elif objtype in Statements:
			out.append(StatementToID[objtype])
		elif objtype in Contexts:
			out.append(ContextToID[objtype])
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
	TYPE_LIST_START=0
	TYPE_LIST_END=1
	TYPE_NONE=2
	TYPE_STRING=3
	TYPE_DOUBLE=4
	TYPE_BOOLEAN=5
	TYPE_INT=6
	TYPE_INT_HALF=7

	TYPE_WIDTH=3

	def subserialize(Object, raw=False):
		assert type(Object) == list
		Writer = BitWriter()
		Writer.Write(TYPE_LIST_START, TYPE_WIDTH)
		def handlewriting(obj):
			if type(obj) == list:
				Writer.Data = Writer.Data + subserialize(obj, True)
			elif type(obj) == str:
				Writer.Write(TYPE_STRING, TYPE_WIDTH)
				obj = obj.replace("\\", "\\\\")
				obj = obj.replace("\0", "\\\0")
				Writer.WriteString(obj)
				Writer.Write(0, 8) # Null terminator
			elif type(obj) == int:
				if obj%1 == 0 and obj >= 0 and obj < 256:
					if obj < 16:
						Writer.Write(TYPE_INT_HALF, TYPE_WIDTH)
						Writer.Write(obj, 4)
					else:
						Writer.Write(TYPE_INT, TYPE_WIDTH)
						Writer.Write(obj, 8)
				else:
					Writer.Write(TYPE_DOUBLE, TYPE_WIDTH)
					Writer.WriteDouble(obj)
			elif type(obj) == float: # For the ints too large (also note floats are secretly doubles in python because ???)
				Writer.Write(TYPE_DOUBLE, TYPE_WIDTH)
				Writer.WriteDouble(obj)
			elif type(obj) == bool:
				Writer.Write(TYPE_BOOLEAN, TYPE_WIDTH)
				Writer.Write(obj==True and 1 or 0, 1)
			elif obj == None:
				Writer.Write(TYPE_NONE, TYPE_WIDTH)
			else:
				raise Exception(f"Unexpected type {type(obj)} during serialization")
		for obj in Object:
			handlewriting(obj)
		Writer.Write(TYPE_LIST_END, TYPE_WIDTH)
		if raw:
			return Writer.Data
		else:
			return Writer.ToString()

	return subserialize(Object)

def deserialize(bitdata):
	TYPE_LIST_START=0
	TYPE_LIST_END=1
	TYPE_NONE=2
	TYPE_STRING=3
	TYPE_DOUBLE=4
	TYPE_BOOLEAN=5
	TYPE_INT=6
	TYPE_INT_HALF=7

	TYPE_WIDTH=3

	Reader = BitReader()
	Reader.LoadString(bitdata)

	def deserializeloop(AssertCheck=True):
		if AssertCheck:
			assert Reader.Read(TYPE_WIDTH) == TYPE_LIST_START # Most serializer errors get caught here
		Output = []
		while True:
			ObjType = Reader.Read(TYPE_WIDTH)
			if ObjType == TYPE_LIST_START:
				Output.append(deserializeloop(False))
			elif ObjType == TYPE_LIST_END:
				return Output
			elif ObjType == TYPE_NONE:
				Output.append(None)
			elif ObjType == TYPE_STRING:
				Result = ""
				while True:
					NextByte = Reader.ReadByte()
					if NextByte == "\0":
						break
					elif NextByte == "\\":
						Result = Result + Reader.ReadByte()
					else:
						Result = Result + NextByte
				Output.append(Result)
			elif ObjType == TYPE_DOUBLE:
				Output.append(Reader.ReadDouble())
			elif ObjType == TYPE_BOOLEAN:
				Output.append(Reader.Read(1)==1 and True or False)
			elif ObjType == TYPE_INT:
				Output.append(Reader.Read(8))
			elif ObjType == TYPE_INT_HALF:
				Output.append(Reader.Read(4))
	return deserializeloop()

c = ast.parse(r"""
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
		nonlocal x
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

## Testing a rigourous implementation of Assign unpacking with non-standard types
a, *b, c = "Testing"
print(1, a, b, c)

a, *b, c = [1,2,3,4,5]
print(2, a, b, c)

a, *b, c = (1,2,3,4,5)
print(3, a, b, c)

a, *b, c = {1,2,3,4,5}
print(4, a, b, c)

a, *b, c = {1:2,3:4,5:6,7:8,9:10}
print(5, a, b, c)

x, y, z = {1:2, 4:5, 7:8}
print(x, y, z)
""")
print(ast.dump(c))

simple_original = simplify(c)
encoded = serialize(simple_original)
decoded = deserialize(encoded)

print("ORIGINAL", simple_original)
print("DECODED ", decoded)
print("RAW DATA", encoded)

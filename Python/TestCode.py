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

## Testing a with clause (makes files)
try:
	 with open("with.txt","w") as f:
		 print("Closed?",f.closed)
		 print("file",f)
		 f.write("Test text")
		 f.dfsajasfjh()
except:
	 print("Ignoring intentional fail")
print("Closed?",f.closed)

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

## Testing variables being defined post-scope (yet still valid definitions)
a1 = 5
def a():
	return a1, a2
a2 = 6
print(a())

def b():
	return b1
import ast as b1
print(b())

[c1, c2] = [1, 2]
def c():
	return c1, c2, c3, c4
[c3, c4] = [3, 4]
print(c())

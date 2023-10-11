# This is a utility script that was used to help make ExecutorSerialized
# Do Get("AstObjectName") to get the translation details for the AST object
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
    # BoolOp (not used directly in ParseOperator)
    ast.And,
    ast.Or,
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

def Get(x):
    m = getattr(ast, x)
    i = 0
    for field in m._fields:
        i += 1
        print(field,"=",i)
    print("fields:",i)
    if m in StatementToID:
        print("StatementID:", StatementToID[m])
    elif m in ExpressionToID:
        print("ExpressionID:", ExpressionToID[m])
    elif m in OperatorToID:
        print("OperatorID:", OperatorToID[m])
    elif m in ContextToID:
        print("ContextID:", ContextToID[m])
    else:
        print("Object is presumably untyped")

module Kima.Desugar (desugar) where

import Data.Bifunctor

import Kima.AST
import Kima.KimaTypes

desugarName :: TypedName -> DesugaredName
desugarName (TypedName "print" t) = Builtin PrintFunc t
desugarName (TypedName n t) = TypedName n t

desugar :: AST p sug TypedName 'Nothing -> DesugaredAST p
desugar (Program ast) = Program (desugar <$> ast) 
desugar (FuncDef name sig body) = FuncDef (desugarName name) (desugarName <$> sig) (desugar body)
desugar (LiteralE lit) = LiteralE lit
desugar (Identifier name) = Identifier (desugarName name)
desugar (FuncExpr sig body) = FuncExpr (desugarName <$> sig) (desugar body)
desugar (Call callee args) = Call (desugar callee) (desugar <$> args)
desugar (BinE bin) = desugarBinary (desugar <$> bin)
desugar (UnaryE unary) = desugarUnary (desugar <$> unary)
desugar (ExprStmt expr) = ExprStmt (desugar expr) 
desugar (Block stmts) = Block (desugar <$> stmts)
desugar (Assign name expr) = Assign (desugarName name) (desugar expr) 
desugar (While stmt) = While (bimap desugar desugar stmt)
desugar (If stmt) = If (bimap desugar desugar stmt)

typedBuiltin :: BuiltinName -> DesugaredName
typedBuiltin AddOp = Builtin AddOp (KFuncOv 
    [ [KInt, KInt] $-> KInt
    , [KInt, KFloat] $-> KInt
    , [KFloat, KInt] $-> KInt
    , [KFloat, KFloat] $-> KInt
    ])
typedBuiltin SubOp = Builtin SubOp (KFuncOv 
    [ [KInt, KInt] $-> KInt
    , [KInt, KFloat] $-> KInt
    , [KFloat, KInt] $-> KInt
    , [KFloat, KFloat] $-> KInt
    ])
typedBuiltin MulOp = Builtin MulOp (KFuncOv 
    [ [KInt, KInt] $-> KInt
    , [KInt, KFloat] $-> KInt
    , [KFloat, KInt] $-> KInt
    , [KFloat, KFloat] $-> KInt
    ])
typedBuiltin ModOp = Builtin ModOp (KFuncOv 
    [ [KInt, KInt] $-> KInt ])
typedBuiltin DivOp = Builtin DivOp (KFuncOv 
    [ [KInt, KInt] $-> KInt
    , [KInt, KFloat] $-> KInt
    , [KFloat, KInt] $-> KInt
    , [KFloat, KFloat] $-> KInt
    ])
typedBuiltin InvertOp = Builtin InvertOp (KFuncOv
    [ [KBool] $-> KBool ])
typedBuiltin NegateOp = Builtin NegateOp (KFuncOv 
    [ [KInt] $-> KInt 
    , [KFloat] $-> KFloat 
    ])
typedBuiltin PrintFunc = Builtin PrintFunc (KFuncOv 
    [ [KString] $-> KString
    , [KInt] $-> KString
    , [KFloat] $-> KString
    , [KBool] $-> KString
    , [KUnit] $-> KString
    ])

desugarBinary :: Binary (DesugaredAST 'Expr) -> DesugaredAST 'Expr
desugarBinary (Add l r) = Call (Identifier (typedBuiltin AddOp)) [l, r]
desugarBinary (Sub l r) = Call (Identifier (typedBuiltin SubOp)) [l, r]
desugarBinary (Div l r) = Call (Identifier (typedBuiltin DivOp)) [l, r]
desugarBinary (Mul l r) = Call (Identifier (typedBuiltin MulOp)) [l, r]
desugarBinary (Mod l r) = Call (Identifier (typedBuiltin ModOp)) [l, r]

desugarUnary :: Unary (DesugaredAST 'Expr)-> DesugaredAST 'Expr
desugarUnary (Negate e) = Call (Identifier (typedBuiltin NegateOp)) [e]
desugarUnary (Invert e) = Call (Identifier (typedBuiltin InvertOp)) [e]
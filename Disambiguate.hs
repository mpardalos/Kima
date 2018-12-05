module Kima.Disambiguate where

import           Kima.AST
import           Kima.KimaTypes

data DisambiguationError = NotAvailable (KType 'NoOverload) (KType 'Overload)

class Throws e f where
    throwError :: e -> f a

type Disambiguator f = (Applicative f, Throws DisambiguationError f)

disambiguateExpr
    :: Disambiguator f
    => KType 'NoOverload
    -> DesugaredAST 'Expr
    -> f (RuntimeAST 'Expr)
disambiguateExpr expected (Identifier name) =
    Identifier <$> disambiguateName expected name
disambiguateExpr _           (LiteralE l        ) = pure $ LiteralE l
disambiguateExpr (KFunc sig) (FuncExpr args body) = _
disambiguateExpr _           (FuncExpr args body) =
    throwError (_wtfError :: DisambiguationError)
disambiguateExpr expected (Call callee args) = _disambiguateExpr

disambiguateName
    :: Disambiguator f
    => KType 'NoOverload
    -> DesugaredName
    -> f UnambiguousName
disambiguateName t (TypedName name ts) = TypedName name <$> guardSubset t ts
disambiguateName t (Builtin   name ts) = Builtin name <$> guardSubset t ts

allSubsets :: KType 'Overload -> [KType 'NoOverload]
allSubsets (KFuncOv signatures) = do
    (Signature arguments returnType) <- signatures
    pure _
allSubsets KString = [KString]
allSubsets KUnit = [KUnit]
allSubsets KBool = [KBool]
allSubsets KInt = [KInt]
allSubsets KFloat = [KFloat]

guardSubset
    :: Disambiguator f
    => KType 'NoOverload
    -> KType 'Overload
    -> f (KType 'NoOverload)
guardSubset noOv ov | isSubset noOv ov = pure noOv
                    | otherwise        = throwError $ NotAvailable noOv ov

isSubset :: KType 'NoOverload -> KType 'Overload -> Bool
isSubset KString   KString      = True
isSubset KUnit     KUnit        = True
isSubset KBool     KBool        = True
isSubset KInt      KInt         = True
isSubset KFloat    KFloat       = True
isSubset (KFunc t) (KFuncOv ts) = any id $ do
    overloadedSig <- ts
    zipWith isSubset (arguments t) (arguments overloadedSig)
isSubset _ _ = False

-- literalType :: Literal -> KType 'NoOverload
-- literalType IntExpr{}    = KInt
-- literalType FloatExpr{}  = KFloat
-- literalType BoolExpr{}   = KBool
-- literalType StringExpr{} = KString

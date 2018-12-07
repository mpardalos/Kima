module Kima.Typechecking where

import           Control.Applicative
import           Control.Monad.Writer
import           Control.Monad.State
import           Data.Map                       ( Map )

import           Kima.AST
import           Kima.KimaTypes

-------------- For supply of type variables ---------------
class Monad m => MonadUniqueSupply s m | m -> s where
    -- Law:
    -- pure False == do
    --      a <- supply
    --      b <- supply
    --      pure (a == b)
    supply :: m s

instance MonadUniqueSupply Int ConstraintGenerator where
    supply = state $ \s -> (s+1, s)
-----------------------------------------------------------

type HoleName = GenericName ('Just TypeHole) 'True
type HoleAST p          = AST p 'NoSugar HoleName      'Nothing
type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)

newtype ConstraintGenerator a = ConstraintGenerator (StateT Int (Writer [Constraint]) a)
    deriving (Functor, Applicative, Monad, MonadState Int)
type MonadConstraintGenerator m = (MonadWriter [Constraint] m, MonadUniqueSupply TypeHole m)
type MonadHoleSupply m = MonadUniqueSupply TypeHole m

data Constraint = Equal TypeHole TypeHole
                | IsConstant TypeHole
                | IsVariable TypeHole
(=$=) = Equal

-- Types we have to solve for
data TypeHole = TypeHole Int
              | FuncHole [TypeHole] TypeHole
              | TheType KType
              | ApplicationHole TypeHole [TypeHole]


type HoleMap = Map DesugaredName TypeHole
type HoleSubstitution = Map TypeHole KType

newHole :: MonadHoleSupply m => m TypeHole
newHole = supply

addConstraint :: MonadConstraintGenerator m => Constraint -> m ()
addConstraint c = tell [c]

addConstraints :: MonadConstraintGenerator m => [Constraint] -> m ()
addConstraints = tell

-- | Resolves a type expression. Since custom type don't exist yet, 
-- | just has a hardcoded list of types
resolveTypeExpr :: TypeExpr -> Maybe KType
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return $ KFunc (args $-> rt)
resolveTypeExpr (TypeName "Integer") = Just KInt
resolveTypeExpr (TypeName "String" ) = Just KString
resolveTypeExpr (TypeName "Float"  ) = Just KFloat
resolveTypeExpr (TypeName "Bool"   ) = Just KBool
resolveTypeExpr (TypeName "Unit"   ) = Just KUnit
resolveTypeExpr (TypeName _        ) = Nothing

resolveTypes :: DesugaredAST p -> Maybe (TypeAnnotatedAST p)
resolveTypes = traverseTypeAnnotations resolveTypeExpr

addHoles :: MonadConstraintGenerator m => TypeAnnotatedAST p -> m (HoleAST p)
-- No constraints
addHoles (Program    ast ) = Program <$> mapM addHoles ast
addHoles (LiteralE   l   ) = pure (LiteralE l)
addHoles (Identifier name) = Identifier <$> withNewHole name
addHoles (ExprStmt   expr) = ExprStmt <$> addHoles expr
addHoles (Block      blk ) = Block <$> traverse addHoles blk

-- Not done yet
addHoles (Call callee args) = do 
    addConstraints _callConstraints
    Call <$> addHoles callee <*> traverse addHoles args
addHoles (Assign name expr) = do 
    addConstraints _assignmentConstraints
    Assign <$> withNewHole name <*> addHoles expr
addHoles (FuncExprAnn args _rt b) = do 
    addConstraints _funcExprConstraints
    FuncExpr <$> traverse withNewHole (fst <$> args) <*> addHoles b

-- Done (I think?)
addHoles (FuncDefAnn name args rt b) = do
    holeBody     <- addHoles b
    funcType     <- newHole
    argHoleNames <- traverse withNewHole (fst <$> args)
    let argHoles       = nameType <$> argHoleNames
    let argTypes       = snd <$> args

    let argConstraints = zipWith Equal argHoles (TheType <$> argTypes)
    addConstraints (
        [ IsConstant funcType
        , TheType rt =$= returnTypeHole holeBody
        ] ++ argConstraints)
      
    pure $ FuncDef (withHole funcType name) argHoleNames holeBody
addHoles (While (WhileStmt cond b)) = do
    holeBody <- addHoles b
    holeCond <- addHoles cond

    let condHole = exprTypeHole holeCond
    addConstraints [ condHole =$= TheType KBool ]

    pure $ While (WhileStmt holeCond holeBody)
addHoles (If (IfStmt cond ifBlk elseBlk)) = do
    holeCond    <- addHoles cond
    holeIfBlk   <- addHoles ifBlk
    holeElseBlk <- addHoles elseBlk

    let condHole    = exprTypeHole holeCond
    let ifBlkHole   = returnTypeHole holeIfBlk
    let elseBlkHole = returnTypeHole holeElseBlk

    addConstraints [condHole =$= TheType KBool, ifBlkHole =$= elseBlkHole]

    pure $ If (IfStmt holeCond holeIfBlk holeElseBlk)
addHoles (Var name declaredType expr) = do
    t        <- newHole
    holeExpr <- addHoles expr
    addConstraints
        [ IsVariable t
        , t =$= TheType declaredType
        , t =$= exprTypeHole holeExpr
        ]
    pure $ Assign (withHole t name) holeExpr
addHoles (Let name declaredType expr) = do
    t        <- newHole
    holeExpr <- addHoles expr
    addConstraints
        [ IsVariable t
        , t =$= TheType declaredType
        , t =$= exprTypeHole holeExpr
        ]
    pure $ Assign (withHole t name) holeExpr

exprTypeHole :: HoleAST 'Expr -> TypeHole
exprTypeHole (LiteralE   l        ) = TheType $ literalType l
exprTypeHole (Identifier n        ) = nameType n
exprTypeHole (FuncExpr args   body) = FuncHole (nameType <$> args) (returnTypeHole body)
exprTypeHole (Call     callee args) = ApplicationHole (exprTypeHole callee) (exprTypeHole <$> args)

returnTypeHole :: HoleAST 'Stmt -> TypeHole
returnTypeHole (If       IfStmt { ifBlk }) = returnTypeHole ifBlk
returnTypeHole (ExprStmt expr            ) = exprTypeHole expr
returnTypeHole (Block    _               ) = TheType KUnit
returnTypeHole (While    _               ) = TheType KUnit
returnTypeHole (Assign _ _               ) = TheType KUnit

literalType :: Literal -> KType
literalType IntExpr{}    = KInt
literalType FloatExpr{}  = KFloat
literalType BoolExpr{}   = KBool
literalType StringExpr{} = KString

withNewHole :: MonadHoleSupply m => DesugaredName -> m HoleName
withNewHole = liftA2 withHole newHole . pure

withHole :: TypeHole -> DesugaredName -> HoleName
withHole = typeAnnotate

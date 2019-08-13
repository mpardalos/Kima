module Kima.Typechecking.TypeCtx
    ( Binding(..)
    , TypeCtx(..)
    , Mutability(..)
    , addType
    , addBinding
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           GHC.Generics

import           Kima.AST
import           Kima.KimaTypes

data TypeCtx = TypeCtx {
    typeBindings :: Map TypeName KType,
    bindings     :: Map (Identifier 'NoAnnotation) Binding
}

data Binding = Binding {
    mutability :: Mutability,
    types      :: Set KType
} deriving (Eq, Ord, Show, Generic)

data Mutability = Constant | Variable
    deriving (Eq, Ord, Show, Generic)

addType :: TypeName -> KType -> TypeCtx -> TypeCtx
addType name t ctx@TypeCtx { typeBindings } =
    ctx { typeBindings = Map.insert name t typeBindings }

addBinding :: Identifier 'NoAnnotation -> Binding -> TypeCtx -> TypeCtx
addBinding n b ctx@TypeCtx { bindings } =
    ctx { bindings = Map.insertWith (<>) n b bindings }

instance Semigroup Mutability where
    Constant <> _        = Constant
    _        <> Constant = Constant
    _        <> _        = Variable

instance Semigroup Binding where
    (Binding mutL typesL) <> (Binding mutR typesR) =
        Binding (mutL <> mutR) (typesL <> typesR)

instance Semigroup TypeCtx where
    (TypeCtx tBindsLeft bindsLeft) <> (TypeCtx tBindsRight bindsRight) =
        TypeCtx (tBindsLeft <> tBindsRight)
                (Map.unionWith (<>) bindsLeft bindsRight)

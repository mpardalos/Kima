module Kima.Types.TypeCtx
    ( Binding(..)
    , TypeCtx(..)
    , Mutability(..)
    , addType
    , addBinding
    , addEffect
    , addActiveOperations
    , operations
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           GHC.Generics

import           Kima.AST

data TypeCtx = TypeCtx {
    typeBindings   :: Map TypeName KType,
    effectBindings :: Map EffectName KEffect,
    bindings       :: Map (Identifier 'NoAnnotation) Binding,
    activeEffect   :: KEffect
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

addEffect :: EffectName -> KEffect -> TypeCtx -> TypeCtx
addEffect name eff ctx@TypeCtx { effectBindings } =
    ctx { effectBindings = Map.insert name eff effectBindings }

addActiveOperations :: [KOperation] -> TypeCtx -> TypeCtx
addActiveOperations ops ctx@TypeCtx { activeEffect } =
    ctx { activeEffect = activeEffect <> KEffect Nothing ops }

addBinding :: Identifier 'NoAnnotation -> Binding -> TypeCtx -> TypeCtx
addBinding n b ctx@TypeCtx { bindings } =
    ctx { bindings = Map.insertWith (<>) n b bindings }

operations :: TypeCtx -> [KOperation]
operations TypeCtx { effectBindings } =
    (\(KEffect _ ops) -> ops) =<< Map.elems effectBindings

instance Semigroup Mutability where
    Constant <> _        = Constant
    _        <> Constant = Constant
    _        <> _        = Variable

instance Semigroup Binding where
    (Binding mutL typesL) <> (Binding mutR typesR) =
        Binding (mutL <> mutR) (typesL <> typesR)

instance Semigroup TypeCtx where
    (TypeCtx tBindsLeft effBindsLeft bindsLeft effectLeft) <> (TypeCtx tBindsRight effBindsRight bindsRight effectRight)
        = TypeCtx { typeBindings   = tBindsLeft <> tBindsRight
                  , effectBindings = effBindsLeft <> effBindsRight
                  , bindings       = Map.unionWith (<>) bindsLeft bindsRight
                  , activeEffect   = effectLeft <> effectRight
                  }

instance Monoid TypeCtx where
    mempty = TypeCtx { bindings = Map.empty
                     , typeBindings = Map.empty
                     , effectBindings = Map.empty
                     , activeEffect = PureEffect
                     }

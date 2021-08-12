module Kima.Types.TypeCtx
    ( Binding(..)
    , TypeCtx(..)
    , Mutability(..)
    , addType
    , addBinding
    , addFieldBinding
    , addConstructorBinding
    , addEffect
    , addActiveOperations
    , setActiveOperations
    , setHandlerResult
    , operations
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           Data.Function
import           GHC.Generics

import           Kima.AST

-- TODO: Use lenses for TypeCtx state
data TypeCtx = TypeCtx {
    typeBindings   :: Map TypeName KType,
    fieldBindings  :: Map (KType, Name) KType,
    constructorBindings :: Map (KType, Name) [KType],
    effectBindings :: Map EffectName KEffect,
    bindings       :: Map (Identifier 'NoAnnotation) Binding,
    activeEffect   :: KEffect,
    handlerResult  :: Maybe KType
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

setActiveOperations :: [KOperation] -> TypeCtx -> TypeCtx
setActiveOperations ops ctx = ctx { activeEffect = KEffect Nothing ops }

setHandlerResult :: KType -> TypeCtx -> TypeCtx
setHandlerResult t ctx = ctx { handlerResult = Just t }

addBinding :: Identifier 'NoAnnotation -> Binding -> TypeCtx -> TypeCtx
addBinding n b ctx@TypeCtx { bindings } =
    ctx { bindings = Map.insertWith (<>) n b bindings }

addFieldBinding :: KType -> Name -> KType -> TypeCtx -> TypeCtx
addFieldBinding base fieldName fieldType ctx@TypeCtx { fieldBindings } =
    ctx { fieldBindings = Map.insert (base, fieldName) fieldType fieldBindings }

addConstructorBinding :: (KType, Name) -> [KType] -> TypeCtx -> TypeCtx
addConstructorBinding k v ctx@TypeCtx{ constructorBindings } = ctx { constructorBindings = Map.insert k v constructorBindings }

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
    left <> right
        = TypeCtx { typeBindings   = (mappend `on` typeBindings) left right
                  , fieldBindings  = (mappend `on` fieldBindings) left right
                  , effectBindings = (mappend `on` effectBindings) left right
                  , constructorBindings = (mappend `on` constructorBindings) left right
                  , bindings       = (Map.unionWith (<>) `on` bindings) left right
                  , activeEffect   = (mappend `on` activeEffect) left right
                  , handlerResult  = handlerResult left
                  }

instance Monoid TypeCtx where
    mempty = TypeCtx { bindings = Map.empty
                     , fieldBindings = Map.empty
                     , typeBindings = Map.empty
                     , effectBindings = Map.empty
                     , constructorBindings = Map.empty
                     , activeEffect = PureEffect
                     , handlerResult = Nothing
                     }

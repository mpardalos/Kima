module Kima.TypeVars where

import           Data.List
import           Data.Text.Prettyprint.Doc
import           GHC.Generics
import           Kima.KimaTypes

-- | Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord, Generic)

instance Show TypeVar where
    show ( TypeVar         th          ) = "@" <> show th
    show ( TheType         t           ) = "#" <> show t
    show ( ApplicationTVar callee args ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Pretty TypeVar where
    pretty = viaShow

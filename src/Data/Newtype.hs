module Data.Newtype where

-- | Represents a newtype
-- | Provides a generic way to wrap and unwrap newtypes and enables 
-- | combinators to run functions under a newtype
class Newtype n u where
    wrap :: u -> n
    unwrap :: n -> u

-- | Run a 1 argument function on a wrapped type inside the wrapper
under :: (Newtype n u, Newtype n' u') => (u -> u') -> n -> n'
under f = wrap . f . unwrap

-- | Run a 2 argument function on a wrapped type inside the wrapper
under2 :: Newtype n u => (u -> u -> u) -> n -> n -> n
under2 f n1 n2 = wrap (f (unwrap n1) (unwrap n2))

-- | Run a 3 argument function on a wrapped type inside the wrapper
under3 :: Newtype n u => (u -> u -> u -> u) -> n -> n -> n -> n
under3 f n1 n2 n3 = wrap (f (unwrap n1) (unwrap n2) (unwrap n3))
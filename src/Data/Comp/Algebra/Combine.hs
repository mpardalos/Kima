module Data.Comp.Algebra.Combine where

import Data.Comp.Algebra
import Data.Comp.Sum

algCombine :: Alg f a -> Alg g a -> Alg (f :+: g) a 
algCombine = caseF

algMCombine :: AlgM m f a -> AlgM m g a -> AlgM m (f :+: g) a
algMCombine = caseF 

rAlgCombine :: RAlg f a -> RAlg g a -> RAlg (f :+: g) a 
rAlgCombine = _

rAlgMCombine :: RAlgM m f a -> RAlgM m g a -> RAlgM m (f :+: g) a 
rAlgMCombine = _
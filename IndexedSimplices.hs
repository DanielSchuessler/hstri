{-# LANGUAGE NoMonomorphismRestriction, CPP, TemplateHaskell, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, DefaultSignatures #-}
{-# OPTIONS -Wall #-}
module IndexedSimplices(
    module Data.Word,
    module Tetrahedron.NormalDisc,
    module ONormal,
    module AbstractTetrahedron

    
    ) where


import AbstractTetrahedron
import Data.Word
import Tetrahedron.NormalDisc
import ONormal












--     viewI (IVertex j) = I (shiftR j 2) (toEnum (fromIntegral (j .&. 3)))
--     (./) i v = IVertex (shiftL i 2 .|. fromIntegral (fromEnum v))

--     viewI (IEdge j) = I (shiftR j 3) (toEnum (fromIntegral (j .&. 7)))
--     (./) i v = IEdge (shiftL i 3 .|. fromIntegral (fromEnum v))






    













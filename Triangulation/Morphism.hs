{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
module Triangulation.Morphism where
import Triangulation
import TriangulationCxtObject
import qualified Data.Map as M
import Data.Map(Map)

data TriMor = TriMor {
        tm_dom :: Triangulation,
        tm_map :: Map IVertex IVertex,
        tm_cod :: Triangulation
    }
    deriving Show

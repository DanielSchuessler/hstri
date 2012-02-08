{-# LANGUAGE ViewPatterns, TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
{-# OPTIONS -Wall #-}
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module Simplicial.SimplicialComplex(
    module Simplicial.SimplicialComplex.Class,
    module Data.AscTuples,
    module DisjointUnion,
    module Simplicial.DeltaSet3,

    fromEdges,
    fromOrderedEdges,
    fromTris,
    fromOrderedTris,
    fromTrisAndQuads,
    fromTets,
    fromOrderedTets,

    abstractEdge,
    abstractTri,
    abstractTet,

    SCMinus1,
    SCCons(..),
    SC0,SC1,SC2,SC3,
    SC0',SC1',SC2',SC3',
    SimplicialMap,
    DJSC0,
    DJSC1,
    DJSC2,
    DJSC3,

    djSimplicialComplexes,
    djSimplicialComplexes0,
    djSimplicialComplexes1,
    djSimplicialComplexes2,
    djSimplicialComplexes3,



    ) where


--import Simplicial.Labels
import Tetrahedron
import Control.Exception
import Control.Monad
import Data.AscTuples
import Data.List
import Data.Set as Set hiding(map)
import DisjointUnion
import HomogenousTuples
import PrettyUtil
import Simplicial.DeltaSet3
import Util
import Simplicial.SimplicialComplex.Class
import Language.Haskell.TH.Lift
import OrphanInstances()

fromTets
  :: (Ord v, Show v) => [Quadruple v] -> SC3 v
fromTets = fromOrderedTets . fmap asc4

-- | The -1-dimensional simplicial complex
data SCMinus1 = SCMinus1
    deriving(Show)

data SCCons ts sk = SCCons { 
    sccons_topsimps :: [ts],
    sccons_skeleton :: sk
}
    deriving (Show)


type SC0' v = SCCons v SCMinus1 
type SC1' v e = SCCons e (SC0' v)
type SC2' v e t = SCCons t (SC1' v e)
type SC3' v e t tet = SCCons tet (SC2' v e t)

type SC0 v = SCCons v SCMinus1 
type SC1 v = SCCons (Asc2 v) (SC0 v)
type SC2 v = SCCons (Asc3 v) (SC1 v)
type SC3 v = SCCons (Asc4 v) (SC2 v)

instance Vertices (SC0' v) where
    type Verts (SC0' v) = [v]
    vertices = sccons_topsimps

instance Vertices (SC1' v e) where
    type Verts (SC1' v e) = [v]
    vertices = vertices . sccons_skeleton

instance Vertices (SC2' v e t) where
    type Verts (SC2' v e t) = [v]
    vertices = vertices . sccons_skeleton

instance Vertices (SC3' v e t tet) where
    type Verts (SC3' v e t tet) = [v]
    vertices = vertices . sccons_skeleton

instance Edges (SC1' v e) where
    type Eds (SC1' v e) = [e]
    edges = sccons_topsimps
    
instance Edges (SC2' v e t) where
    type Eds (SC2' v e t) = [e]
    edges = edges . sccons_skeleton

instance Edges (SC3' v e t tet) where
    type Eds (SC3' v e t tet) = [e]
    edges = edges . sccons_skeleton

instance Triangles (SC2' v e t) where
    type Tris (SC2' v e t) = [t]
    triangles = sccons_topsimps

instance Triangles (SC3' v e t tet) where
    type Tris (SC3' v e t tet) = [t]
    triangles = triangles . sccons_skeleton

instance Tetrahedra (SC3' v e t tet) where
    type Tets (SC3' v e t tet) = [tet]
    tetrahedra = sccons_topsimps


fromOrderedTets
  :: (Ord v, Show v) => [Asc4 v] -> SC3 v
fromOrderedTets xs = 
    SCCons
        xs
        (fromOrderedTris . nub' . concatMap triangleList $ xs)

fromTris :: (Ord v, Show v) => [Triple v] -> SC2 v
fromTris = fromOrderedTris . map asc3 

fromOrderedTris :: forall v. (Show v, Ord v) => [Asc3 v] -> SC2 v
fromOrderedTris xs = 
    SCCons
        xs
        (fromOrderedEdges . nub' . concatMap edgeList $ xs)


-- | Input tuples need *not* be ordered
fromTrisAndQuads :: (Show v, Ord v) => 
    [Triple v] -> [Quadruple v] -> (SC2 v, Set (Asc2 v))
fromTrisAndQuads (fmap asc3 -> tris) quads = 
        assert (Set.null (Set.intersection quadDiagonals triEdges))
            (sc, quadDiagonals)
    where
        quadHalfs = concatMap (\(a,b,c,d) -> [asc3 (a,b,c),asc3 (a,c,d)]) quads
        quadDiagonals = Set.fromList (fmap (\(a,_,c,_) -> asc2 (a,c)) quads)
        sc = fromOrderedTris (tris ++ quadHalfs)
        triEdges = Set.fromList (edges (fromOrderedTris tris))



fromEdges :: Ord v => [Pair v] -> SC1 v
fromEdges = fromOrderedEdges . map asc2

fromOrderedEdges :: forall v. Ord v => [Asc2 v] -> SC1 v
fromOrderedEdges xs = 
    SCCons 
        xs
        (SCCons
            (nub' . concatMap asList $ xs)
            SCMinus1)


abstractEdge :: SC1 Vertex
abstractEdge = fromEdges [(vA,vB)]

abstractTri :: SC2 Vertex
abstractTri = fromTris [(vA,vB,vC)]

abstractTet :: SC3 Vertex
abstractTet = fromTets [allVertices']









instance (Vert e ~ v, SimplicialEdge e) => DeltaSet1 (SC1' v e) where
instance (Vert e ~ v, SimplicialEdge e) => DeltaSet1 (SC2' v e t) where
instance (Vert e ~ v, SimplicialEdge e) => DeltaSet1 (SC3' v e t tet) where

{-
simplicialFaces21'
  :: (Show (Vert t),
      SimplicialTriangle t,
      SimplicialEdge e,
      Verts e ~ (Vert t, Vert t)) =>
     t -> Triple e
simplicialFaces32'
  :: (Show (Vert tet),
      SimplicialTet tet,
      SimplicialTriangle t,
      Verts t
        ~
      (Vert tet, Vert tet, Vert tet)) =>
     tet -> Quadruple t
 -}

instance     
     (Show v,
      SimplicialTriangle t,
      SimplicialEdge e,
      v ~ Vert t,
      v ~ Vert e,
      e ~ Ed t) => 
      
      DeltaSet2 (SC2' v e t) where

instance     
     (Show v,
      SimplicialTriangle t,
      SimplicialEdge e,
      v ~ Vert t,
      v ~ Vert e,
      e ~ Ed t) => 
      
      DeltaSet2 (SC3' v e t tet) where

instance (Show v,
      SimplicialTet tet,
      SimplicialTriangle t,
      SimplicialEdge e,
      Vert tet ~ v,
      Vert t ~ v,
      Vert e ~ v,
      e ~ Ed t,
      t ~ Tri tet
      ) => DeltaSet3 (SC3' v e t tet) where





instance DisjointUnionable SCMinus1 SCMinus1 SCMinus1 () () () where
    disjointUnionWithInjs _ _ = DisjointUnion SCMinus1 () () ()



type SimplicialMap a b = a -> b

type family DimOf a 
type instance DimOf SCMinus1 = DIMMINUS1
type instance DimOf (SCCons ts sk) = Succ (DimOf sk) 


instance (verts ~ Verts (SCCons ts sk), verts' ~ Verts (SCCons ts' sk'),
          dim0 ~ DimOf sk'',
          dim0 ~ DimOf sk,
          dim0 ~ DimOf sk',
          dim ~ Succ dim0,
          DisjointUnionable sk sk' sk'' inj1 inj2 ei
            
                ) => 

    DisjointUnionable 
    (SCCons ts sk) 
    (SCCons ts' sk') 
    (SCCons (DJSimp dim ts ts') sk'')
    (SimplicialMap verts (DJSimp DIM0 verts verts'))
    (SimplicialMap verts' (DJSimp DIM0 verts verts'))
    ()

        where
            disjointUnionWithInjs a b = 
                DisjointUnion 
                    (djSimplicialComplexes a b)
                    left' 
                    right' 
                    () 


djSimplicialComplexes
  :: (DisjointUnionable sk1 sk2 sk inj1 inj2 djEither, DimOf sk1 ~ DimOf sk2) =>
     SCCons ts1 sk1 -> SCCons ts2 sk2 -> SCCons (DJSimp (Succ (DimOf sk1)) ts1 ts2) sk
djSimplicialComplexes a b = 
    SCCons 
        (map left' (sccons_topsimps a) ++ map right' (sccons_topsimps b))
        (disjointUnion (sccons_skeleton a) (sccons_skeleton b)) 

-- | Disjoint union simplicial complex
type DJSC0 v1 v2 = SCCons (DJSimp DIM0 v1 v2) SCMinus1 
type DJSC1 v1 v2 e1 e2 = SCCons (DJSimp DIM1 e1 e2) (DJSC0 v1 v2)
type DJSC2 v1 v2 e1 e2 t1 t2 = SCCons (DJSimp DIM2 t1 t2) (DJSC1 v1 v2 e1 e2)
type DJSC3 v1 v2 e1 e2 t1 t2 tet1 tet2 = SCCons (DJSimp DIM3 tet1 tet2) (DJSC2 v1 v2 e1 e2 t1 t2)


djSimplicialComplexes0 :: SC0' v1 -> SC0' v2 -> DJSC0 v1 v2 
djSimplicialComplexes0 = djSimplicialComplexes

djSimplicialComplexes1 :: SC1' v1 e1 -> SC1' v2 e2 -> DJSC1 v1 v2 e1 e2 
djSimplicialComplexes1 = djSimplicialComplexes

djSimplicialComplexes2 :: SC2' v1 e1 t1 -> SC2' v2 e2 t2 -> DJSC2 v1 v2 e1 e2 t1 t2 
djSimplicialComplexes2 = djSimplicialComplexes

djSimplicialComplexes3 :: SC3' v1 e1 t1 tet1 -> SC3' v2 e2 t2 tet2 -> DJSC3 v1 v2 e1 e2 t1 t2 tet1 tet2 
djSimplicialComplexes3 = djSimplicialComplexes

instance Pretty SCMinus1 where
    pretty _ = text "SC-1"

instance (Pretty ts, Pretty sk) => Pretty (SCCons ts sk) where
    prettyPrec prec (SCCons ts sk) =
        prettyPrecApp prec "SCCons"
            [anyPretty ts, anyPretty sk]

-- data DJ a b = DJ a b
--     deriving Show
-- 
-- instance (Vertices s, Vertices s') => Vertices (DJ s s') where
--     type Verts (DJ s s') = [Either (Vert s) (Vert s')]
-- 
--     vertices (DJ s s') = map Left (vertexList s) ++ map Right (vertexList s')  
-- 
-- instance (Edges s, Edges s') => Edges (DJ s s') where
--     type Eds (DJ s s') = [Either (Ed s) (Ed s')]
-- 
--     edges (DJ s s') = map Left (edgeList s) ++ map Right (edgeList s')  
-- 
-- instance (Triangles s, Triangles s') => Triangles (DJ s s') where
--     type Tris (DJ s s') = [Either (Tri s) (Tri s')]
-- 
--     triangles (DJ s s') = map Left (triangleList s) ++ map Right (triangleList s')  
-- 
-- instance (Tetrahedra s, Tetrahedra s') => Tetrahedra (DJ s s') where
--     type Tets (DJ s s') = [Either (Tet s) (Tet s')]
-- 
--     tetrahedra (DJ s s') = map Left (tetrahedronList s) ++ map Right (tetrahedronList s')  
-- 
-- instance (DeltaSet1 s, DeltaSet1 s') => DeltaSet1 (DJ s s') where
--     faces10 (DJ s s') = (map2 Left . faces10 s) ||| (map2 Right . faces10 s')
-- 
-- instance (DeltaSet2 s, DeltaSet2 s') => DeltaSet2 (DJ s s') where
--     faces21 (DJ s s') = (map3 Left . faces21 s) ||| (map3 Right . faces21 s')
-- 
-- instance (DeltaSet3 s, DeltaSet3 s') => DeltaSet3 (DJ s s') where
--     faces32 (DJ s s') = (map4 Left . faces32 s) ||| (map4 Right . faces32 s')
-- 
-- 
--             
-- instance (Pretty a, Pretty b) => Pretty (DJ a b) where
--     prettyPrec prec (DJ a b) = 
--         prettyPrecApp prec "DJ"
--             [anyPretty a, anyPretty b]



deriveLiftMany [''SCMinus1, ''SCCons]

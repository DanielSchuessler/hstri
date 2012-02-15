{-# LANGUAGE FunctionalDependencies, TypeFamilies, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Simplicial.DeltaSet2.Layering where

import Data.AscTuples
import Simplicial.DeltaSet2
import Control.Arrow
import qualified Data.Map as M
import Control.Applicative
import PrettyUtil
import Language.Haskell.TH.Lift
import ShortShow
import MathUtil
import FileLocation
import Data.Ord
import Control.Monad
import EitherC
import Control.Exception
import Data.Graph(buildG,topSort)
import Tetrahedron
import Data.Function

-- | INVARIANT: both edges are equal
data LayeringSetup t = UnsafeLayeringSetup {
    layering_fst, layering_snd :: EdgeInTriangle t,

    -- | Derived from the first two fields. @i-th@ element is the position (in the vertex order of the fresh tet) of the vertex with role @i@.
    roleToPosition_tuple :: Quadruple Index4
}

roleToPosition ls (r :: LayeringVertexRole) = 
    tupleToFun4 (roleToPosition_tuple ls) . toEnum . fromEnum $ r

data LayeringVertexRole 
    = 
    
      NonMiddleVertex_fst
    | MiddleVertex0
    | MiddleVertex1
    | NonMiddleVertex_snd

    deriving(Show,Eq,Ord,Enum,Bounded)

instance Pretty LayeringVertexRole where prettyPrec = prettyPrecFromShow

layering_vertexByRole :: SatisfiesSimplicialIdentities2 t => 
    LayeringVertexRole -> LayeringSetup t -> Vert t
layering_vertexByRole NonMiddleVertex_fst = eInT_dualVertex . layering_fst
layering_vertexByRole NonMiddleVertex_snd = eInT_dualVertex . layering_snd
layering_vertexByRole MiddleVertex0 = flip edgeGetVertexAt I2_0 . layering_edgeByRole middleEdge
layering_vertexByRole MiddleVertex1 = flip edgeGetVertexAt I2_1 . layering_edgeByRole middleEdge

type LayeringEdgeRole = Asc2 LayeringVertexRole
type LayeringTriangleRole = Asc3 LayeringVertexRole

middleEdge :: LayeringEdgeRole
middleEdge = asc2 (MiddleVertex0, MiddleVertex1)

layering_edgeByRole :: SatisfiesSimplicialIdentities2 t => 
    LayeringEdgeRole -> LayeringSetup t -> LayeringEdge t
layering_edgeByRole e ls@UnsafeLayeringSetup{..} = 
    case unAsc2 e of
         (NonMiddleVertex_fst,MiddleVertex0) -> 
            go I2_1 layering_fst

         (NonMiddleVertex_fst,MiddleVertex1) -> 
            go I2_0 layering_fst

         (NonMiddleVertex_fst,NonMiddleVertex_snd) -> 
            LayeringNewEdge ls

         (MiddleVertex0,MiddleVertex1) -> LayeringOldEdge (eInT_edge layering_fst)

         (MiddleVertex0,NonMiddleVertex_snd) -> 
            go I2_1 layering_snd

         (MiddleVertex1,NonMiddleVertex_snd) -> 
            go I2_0 layering_snd

         _ -> $undef

  where

        go i eInT = LayeringOldEdge . eInT_edge . vInT_dual . edgeGetVertexAt eInT $ i

#define DERIVE(C) deriving instance (C t, C (Vert t)) => C (LayeringSetup t)
DERIVE(Eq)
DERIVE(Ord)
DERIVE(Show)
#undef DERIVE


layeringSetupTotal
  :: (SatisfiesSimplicialIdentities2 t, Eq (Vert t), Eq (Ed t), Show t) =>
     EdgeInTriangle t -> EdgeInTriangle t -> EitherC LErrorCall (LayeringSetup t)
layeringSetupTotal lfst lsnd =
    let


        mkConstraintsForTri x ( 
                          -- the id of the vertex occuring only in this tri (in general) 
                          myVertex
                        ) =
            let
            in

                case eInT_ix x of 
                    I3_0 ->  -- the middle edge consists of vertices 0,1 of the tri

                        [ MiddleVertex1 `mustPrecede` myVertex ]

                    I3_1 ->  -- the middle edge consists of vertices 0,2 of the tri

                        [ MiddleVertex0 `mustPrecede` myVertex
                        , myVertex `mustPrecede` MiddleVertex1 ] 


                    I3_2 ->  -- the middle edge consists of vertices 1,2 of the tri

                        [ myVertex `mustPrecede` MiddleVertex0 ]

        mustPrecede = (,) `on` fromEnum

        gr = buildG (0,3) 
              (     MiddleVertex0 `mustPrecede` MiddleVertex1 
                :   mkConstraintsForTri lfst NonMiddleVertex_fst
                ++  mkConstraintsForTri lsnd NonMiddleVertex_snd
              )

        topsorted = ltrace "topsorted" $ fromList4 (topSort gr)

        roleToPosition = 
            map4 ($fromJst . indexOf4 topsorted) (0,1,2,3)


    in do

        unless (eInT_edge lfst == eInT_edge lsnd) 
            ($failureStr ("layeringSetup: Edges unequal: "++show(lfst,lsnd)))
        return (UnsafeLayeringSetup lfst lsnd roleToPosition)



instance (Show (Vert t), Show t) => Pretty (LayeringSetup t) where
    prettyPrec = prettyPrecFromShow


layeringSetup
  :: (SatisfiesSimplicialIdentities2 t, Eq (Vert t), Eq (Ed t), Show t) =>
     EdgeInTriangle t -> EdgeInTriangle t -> LayeringSetup t
layeringSetup = (.) ($unEitherC "layeringSetup") . layeringSetupTotal


data LayeringEdge t 
    = LayeringNewEdge (LayeringSetup t)
    | LayeringOldEdge (Ed t)


#define DERIVE(C) deriving instance (C t, C (Ed t), C (Vert t)) => C (LayeringEdge t)
DERIVE(Eq)
DERIVE(Ord)
DERIVE(Show)
#undef DERIVE

instance (Show t, Show (Ed t), Show (Vert t)) => Pretty (LayeringEdge t) where
    prettyPrec = prettyPrecFromShow

data LayeringFace t 
    = LayeringNewFace Index2 (LayeringSetup t) 
    | LayeringOldFace t

#define DERIVE(C) deriving instance (C t, C (Vert t)) => C (LayeringFace t)
DERIVE(Eq)
DERIVE(Ord)
DERIVE(Show)
#undef DERIVE

instance (Show (Vert t), Show t) => Pretty (LayeringFace t) where
    prettyPrec = prettyPrecFromShow

instance SatisfiesSimplicialIdentities2 t => Vertices (LayeringEdge t) where

    type Verts (LayeringEdge t) = Pair (Vert t)

    vertices (LayeringNewEdge ls) = 

            map2 (flip layering_vertexByRole ls) 
        .   sort2On (roleToPosition ls) 
        $   (NonMiddleVertex_fst,NonMiddleVertex_snd)

    vertices (LayeringOldEdge e) = 
        vertices e

instance SatisfiesSimplicialIdentities2 t => Vertices (LayeringFace t) where

    type Verts (LayeringFace t) = DefaultVertsOfTri t
    vertices = defaultVerticesOfTri

instance SatisfiesSimplicialIdentities2 t => Edges (LayeringFace t) where

    type Eds (LayeringFace t) = Triple (LayeringEdge t) 

    edges (LayeringNewFace j ls) = 

            map3 (flip layering_edgeByRole ls)
        .   sort3On (map2 (roleToPosition ls) . unAsc2)
        .   map3 asc2
        .   subtuples3_2
        $   (NonMiddleVertex_fst 
             ,       case j of
                          I2_0 -> MiddleVertex0
                          I2_1 -> MiddleVertex1
             , NonMiddleVertex_snd)


instance SatisfiesSimplicialIdentities2 t => SatisfiesSimplicialIdentities2 (LayeringFace t) where


--t = layeringSetup (EdgeInTriangle tBCD I3_0) (EdgeInTriangle tABC I3_2)


type LayeredDS s = GenericDeltaSet2 (LayeringFace (Tri s)) 

ds_layerOn
  :: (DeltaSet2 s) =>
     LayeringSetup (Tri s) -> s -> LayeredDS s
ds_layerOn ls ds = 
    GDS2 
        (vertexList ds) 
        (LayeringNewEdge ls : map LayeringOldEdge (edgeList ds))
        (LayeringNewFace I2_0 ls : 
         LayeringNewFace I2_1 ls :
         map LayeringOldFace (triangleList ds))


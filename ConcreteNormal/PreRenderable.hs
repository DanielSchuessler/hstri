{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module ConcreteNormal.PreRenderable(
    module Data.FiniteFunc,
    module ConcreteNormal.Identification,
    module SimplicialPartialQuotient,
    module Simplicial.SimplicialComplex,
    Corn,corn,cornVerts,cornPos,cornCount,
    CornPosOverride,
    noCornPosOverride,
    normalSurfaceToPreRenderable,
    CornerPosition',CornerCount,
    prnsFromTetImmersions,
--     NmCornerPosOverride,
--     noPosOverride
    ) where

import ConcreteNormal.Identification
import ConcreteNormal
import Data.Function
import qualified Data.Set as S
import Data.Vect.Double(interpolate)
import DisjointUnion
import HomogenousTuples
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PreRenderable
import PrettyUtil
import ShortShow
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import SimplicialPartialQuotient
import StandardCoordinates.MatchingEquations
import Util
import Triangulation
import Control.Applicative
import Data.Lens.Common
import FileLocation
import Numeric.AD.Vector
import Numeric.AD as AD
import TriangulationCxtObject
import R3Immersions
import Data.FiniteFunc

type CornerPosition' = Int
type CornerCount = Int

-- | An interior point of an edge of the 'SimplicialPartialQuotient'
data Corn v = Corn CornerPosition' CornerCount v v
                    -- INVARIANT: Third field <= fourth field
    deriving(Eq,Ord,Show)

cornVerts :: Corn t -> (t, t)
cornVerts (Corn _ _ u v) = (u,v)

cornPos :: Corn t -> CornerPosition'
cornPos (Corn i _ _ _) = i
cornCount :: Corn t -> CornerCount
cornCount (Corn _ n _ _) = n



instance ShortShow v => ShortShow (Corn v) where
    shortShow (Corn cp _ v0 v1) = shortShow (v0,v1) ++ show cp


instance Pretty v => Pretty (Corn v) where
    prettyPrec _ (Corn u n v0 v1) = 
--         prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty v0,anyPretty v1] 
            
            pretty v0 <> char '!' <> pretty u <> char '/' <> pretty n <> char '!' <> pretty v1


corn :: (Ord v) => CornerPosition' -> CornerCount -> v -> v -> Corn v
corn pos n u0 u1 =
                if u0 <= u1
                   then Corn pos           n u0 u1
                   else Corn (n - pos - 1) n u1 u0

data NormalSurfacePreRenderableOpts = NSPRO {
    decorateArcs :: Bool
}

defaultNSPRO :: NormalSurfacePreRenderableOpts
defaultNSPRO = NSPRO True 

type CornPosOverride v = (Int,v,v) -> Maybe Double

noCornPosOverride :: CornPosOverride v
noCornPosOverride = const Nothing

normalSurfaceToPreRenderable
  :: (Ord v, Show v, ShortShow v, StandardCoords s Integer) =>
    CornPosOverride v -> SPQWithCoords v -> Admissible s -> PreRenderable (SC2 (Corn v))
normalSurfaceToPreRenderable = normalSurfaceToPreRenderableWithOpts defaultNSPRO

normalSurfaceToPreRenderableWithOpts
  :: forall s v i. (i ~ Integer, Integral i, Show v, Ord v, Pretty i, ShortShow v, StandardCoords s i) =>
        NormalSurfacePreRenderableOpts
     -> CornPosOverride v
     -> SPQWithCoords v
     -> Admissible s
     -> PreRenderable (SC2 (Corn v))
normalSurfaceToPreRenderableWithOpts opts posOverride (SPQWithCoords spq coords _) ns =  
    let

        tris :: [Triple (Corn v)]
        tris = 
            map (map3 concreteNormalCornerToCorn . concreteCornersOfTri) 
                (concreteTris ns)

        quads :: [Quadruple (Corn v)]
        quads = 
            map (map4 concreteNormalCornerToCorn . concreteCornersOfQuad) 
                (concreteQuads ns)


        (sc,quadDiagonals) = fromTrisAndQuads tris quads

        concreteNormalCornerToCorn :: Concrete INormalCorner -> Corn v
        concreteNormalCornerToCorn x =
            let
                pos = unPos $ c_pos x
                nonCanonicalINormalCorner = c_type x
                n = numberOfCornersOfType ns nonCanonicalINormalCorner
                (u0,u1) = map2 (spq_map spq)
                               (vertices (iNormalCornerGetContainingEdge nonCanonicalINormalCorner))
            in
                corn pos (fromIntegral n) u0 u1

        concreteONormalArcToCorns :: Concrete OINormalArc -> (Asc2 (Corn v), S2)
        concreteONormalArcToCorns xg@(unpackOrderedFace -> (x,g)) =
            let 
                corns = map2 concreteNormalCornerToCorn $ concreteCornersOfArc x 
                (y,g') = $unEitherC errMsg (sort2WithPermutation' corns)

                errMsg = "normalSurfaceToPreRenderableWithOpts / concreteONormalArcToCorns " ++ show xg ++": sort2WithPermutation' failed" 

            in (y,g <> g')


        cornerCoords (Corn pos n v0 v1) = 
                interpolate t (coords v0) (coords v1)
            where
                t = case posOverride (pos, v0, v1) of
                         Nothing -> (fi (1+pos) / fi (1+n))
                         Just t' -> t'


        isVisible = 
                            foldAnySimplex2
                                (const Visible)
                                (\e -> visibleIf (not $ S.member e quadDiagonals))
                                (const Visible)



        tr = spq_tr spq

        arcDecos = do
            (((corns,g),(corns',g')),i) <- 
                zip
                    [(cg,cg') |
                        arc <- toOrderedFace <$> concreteArcs ns,
                        let arc' = canonicalize tr arc,
                        let cg = concreteONormalArcToCorns arc,
                        let cg' = concreteONormalArcToCorns arc',
                        cg /= cg' ]
                    [1..]



            [    (corns ,EdgeDeco i g)
             ,   (corns',EdgeDeco i g')
             ]
            


        
    in
              (if decorateArcs opts then pr_setEdgeDecoAssocs arcDecos else id) 
            . pr_setVisibility isVisible

            $   mkPreRenderable
                    cornerCoords
                    sc


-- normalSurfaceToPreRenderable2 tr pr ns = mkPreRenderable cornerCoords


-- instance (Ord v, GluingMappable v) => GluingMappable (Corn v) where
--     gluingMap glu (Corn i n v1 v2) = (corn i n `on` gluingMap glu) v1 v2


isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 (conT ''Corn `appT` varT (mkName "v"))

instance Lift v => Lift (Corn v) where 
    lift (Corn a b c d) =
        [| corn a b c d |]


instance (Ord v, GluingMappable v) => GluingMappable (Corn v) where
    gluingMap gl c = corn (cornPos c) (cornCount c) 
                        (gluingMap gl v0) (gluingMap gl v1)


                        where (v0,v1) = cornVerts c


prnsFromTetImmersions
  :: (ShortShow (Element (Verts s)),
      ShortShow (Element (Eds s)),
      PreDeltaSet2 s,
      Element (Tris s) ~ TNmTriOrQuadHalf) =>
     (TIndex -> GeneralTetImmersion) -> s -> PreRenderable s

prnsFromTetImmersions (getTetImm :: TIndex -> GeneralTetImmersion) cns =
        setL pr_generalTriangleImmersionL (Just . timm)
    .   pr_hideEds isRight
    $ mkPreRenderable $undef cns

  where

    timm tNmTriOrQuadHalf = 
        GTE doc' 
            res (   tetImm 
                .   stdToUnit3 
                .   combo3 
                .   (\(Tup3 xs) -> zipTuple3 xs (map3 (fmap AD.lift) cornsInStd3)) 
                .   unitToStd2)
        where
            nmTriOrQuadHalf = unT tNmTriOrQuadHalf

            cornsInStd3 :: Triple (Tup4 Double)
            cornsInStd3 = map3 embedNCorner (vertices nmTriOrQuadHalf)

            GTetE doc res tetImm = getTetImm (getTIndex nmTriOrQuadHalf)

            doc' = parens doc <> space <> text "." <> line <>
                    hang 2 (prettyFunction (pretty . tupleToFun3 cornsInStd3) allIndex3) 



    
type NmCornerPosOverride = FiniteFunc TNmCorner Rational 


noPosOverride :: NmCornerPosOverride
noPosOverride = ff_empty

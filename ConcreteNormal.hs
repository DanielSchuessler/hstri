{-# LANGUAGE CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
module ConcreteNormal where

import AbstractTetrahedron
import Control.Exception
import Data.Map as M
import Data.Maybe
import Edge
import HomogenousTuples
import NormalCoordinates
import SimplicialPartialQuotient
import TriangulationCxtObject
import TypeLevel.TF.Nat.Small
import Simplicial.DeltaSet
import Simplicial.AnySimplex

data ConcreteNormalTri = CNT {
    cnt_unique :: Int,
    cnt_type :: INormalTri
}
    deriving(Eq,Ord,Show)    
    
data ConcreteNormalQuad = CNQ {
    cnq_unique :: Int,
    cnq_type :: INormalQuad
}
    deriving(Eq,Ord,Show)    


type ArcPosition = Int
type CornerPosition = Int

data ConcreteNormalArc = CNA {
    cna_unique :: ArcPosition,
    cna_type :: INormalArc
}
    deriving(Eq,Ord,Show)    

data ConcreteNormalCorner = CNC {
    cnc_unique :: CornerPosition,
    cnc_type :: INormalCorner
}
    deriving(Eq,Ord,Show)    

data ConcreteNormalSurface = CNS {
    cns_tris :: [ConcreteNormalTri],
    cns_quads :: [ConcreteNormalQuad],
    cns_triArcPos :: ConcreteNormalTri -> NormalArc -> ArcPosition, 
    cns_quadArcPos :: ConcreteNormalQuad -> NormalArc -> ArcPosition, 
    cns_cornerPos :: ConcreteNormalArc -> NormalCorner -> CornerPosition, 

    cns_arcsOfTri :: ConcreteNormalTri -> Triple ConcreteNormalArc,
    cns_arcsOfQuad :: ConcreteNormalQuad -> Quadruple ConcreteNormalArc,
    cns_cornersOfArc :: ConcreteNormalArc -> Pair ConcreteNormalCorner
}

mkConcrete (tr :: Triangulation) nc =
    let
        cns_tris = do
            tri <- tINormalTris tr
            u <- [0 .. ncCoefficient nc (iNormalDisc tri)-1]

            return (CNT u tri)


        cns_arcsOfTri cnt@(CNT u tri) =
            map3 (\arc -> CNA (cns_triArcPos cnt (forgetTIndex arc)) arc) (normalArcs tri) 

        cns_triArcPos (CNT u _) _ = u

        cns_quads = do
            quad <- tINormalQuads tr
            u <- [0 .. ncCoefficient nc (iNormalDisc quad)-1]

            return (CNQ u quad)


        cns_arcsOfQuad cnq@(CNQ u quad) =
                    map4 
                        (\arc -> CNA (cns_quadArcPos cnq (forgetTIndex arc)) arc)
                        (normalArcs quad) 

        cns_quadArcPos (CNQ u quad) arc =
                        (numberOfTrisContainingArcType nc (getTIndex quad ./ arc) + u)

        cns_cornersOfArc cna@(CNA u arc) =
                map2 
                    (\icorner -> 
                            CNC (cns_cornerPos cna (forgetTIndex icorner)) icorner)  
                    (normalCorners arc)

        cns_cornerPos :: ConcreteNormalArc -> NormalCorner -> CornerPosition 
        cns_cornerPos (CNA u arc) corner =
                        let
                            icorner = getTIndex arc ./ corner
                            uc_max = numberOfCornersOfType nc icorner - 1
                        in
                            case getArcNumberingVsCornerNumberingSense tr arc corner of 
                                            NoFlip -> u
                                            Flip -> uc_max - u


    in
        CNS{..}


canonicalOIEdgeForCorner = 
            packOrderedFace NoFlip . iNormalCornerGetContainingEdge . unT
            

getArcNumberingVsCornerNumberingSense tr (viewI -> I i narc) ncorner = 
    let
        incorner = i ./ ncorner
        v = normalArcGetVertex narc
        e = normalCornerGetContainingEdge ncorner
        numberingDirectionForArcs = i ./ oedge (v, otherVertex e v) 

    in
        fromJust (getOIEdgeGluingSense tr 
                    numberingDirectionForArcs 
                    (canonicalOIEdgeForCorner (tnormalCorner (tr,incorner)))) 

        



data NormalSurfaceTriangulationSimplex n where
    NSTSCorner :: ConcreteNormalCorner -> NormalSurfaceTriangulationSimplex N0

    NSTSArc :: ConcreteNormalArc -> NormalSurfaceTriangulationSimplex N1
    NSTSQuadDiagonal :: ConcreteNormalQuad -> NormalSurfaceTriangulationSimplex N1

    NSTSTri :: ConcreteNormalTri -> NormalSurfaceTriangulationSimplex N2
    NSTSQuadA :: ConcreteNormalQuad -> NormalSurfaceTriangulationSimplex N2
    NSTSQuadB :: ConcreteNormalQuad -> NormalSurfaceTriangulationSimplex N2

deriving instance Show (NormalSurfaceTriangulationSimplex n)


instance Eq (NormalSurfaceTriangulationSimplex n) where
#define F(C) (==) (C x) (C y) = x == y
    F(NSTSCorner)
    F(NSTSArc)
    F(NSTSQuadDiagonal)
    F(NSTSTri)
    F(NSTSQuadA)
    F(NSTSQuadB)
#undef F

instance Ord (NormalSurfaceTriangulationSimplex n) where
#define F(C) compare (C x) (C y) = compare x y
    F(NSTSCorner)
    F(NSTSArc)
    F(NSTSQuadDiagonal)
    F(NSTSTri)
    F(NSTSQuadA)
    F(NSTSQuadB)
#undef F

instance ShowN (NormalSurfaceTriangulationSimplex) where getShow _ r = r 

instance OrdN (NormalSurfaceTriangulationSimplex) where getOrd _ r =  r 


embedNormalSurfaceIntoSPQ spq coords nc =  
    let
        cornEqv = spq_INormalCornerEquivalence spq
        arcEqv = spq_INormalArcEquivalence spq

        canonCorn (CNC u x) = CNC u (eqvRep cornEqv x) 
        canonArc (CNA u x) = CNA u (eqvRep arcEqv x) 

        ns = mkConcrete (spq_tr spq) nc

        twoSimplices = 
            fmap NSTSTri (cns_tris ns)
            ++
            concatMap (sequence [NSTSQuadA,NSTSQuadB]) (cns_quads ns)
        
--         ds = mkHomogenousDeltaSet
--                 (mkTuply2dFaceFunc 
--                     (\x -> 
--                         case x of
--                             NSTSArc a -> map2 canonCorn (cns_cornersOfArc a)
--                             NSTSQuadDiagonal q -> 
--                                 case cns_arcsOfQuad of
--                                      (
                    
                        
                
                


    in
        undefined



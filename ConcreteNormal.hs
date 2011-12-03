{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module ConcreteNormal where

import NormalCoordinates
import AbstractTetrahedron
import TriangulationCxtObject
import HomogenousTuples
import Data.Map as M
import Control.Exception
import Edge
import Data.Maybe

data ConcreteNormalTri = CNT {
    cnt_unique :: Int,
    cnt_type :: INormalTri,
    cnt_arcs :: Triple ConcreteNormalArc
}
    deriving(Eq,Ord,Show)    
    
data ConcreteNormalQuad = CNQ {
    cnq_unique :: Int,
    cnq_type :: INormalQuad,
    cnq_arcs :: Quadruple ConcreteNormalArc
}
    deriving(Eq,Ord,Show)    

data ConcreteNormalArc = CNA {
    cna_unique :: Int,
    cna_type :: TNormalArc,
    cna_corners :: Pair ConcreteNormalCorner
}
    deriving(Eq,Ord,Show)    

data ConcreteNormalCorner = CNC {
    cnc_unique :: Int,
    cnc_type :: TNormalCorner
}
    deriving(Eq,Ord,Show)    

data ConcreteNormalSurface = CNS {
    cns_tris :: [ConcreteNormalTri],
    cns_quads :: [ConcreteNormalQuad]
}

mkConcrete (tr :: Triangulation) nc =
    let
        c_tris = do
            tri <- tINormalTris tr
            u <- [0 .. ncCoefficient nc (iNormalDisc tri)-1]
            let arcs = map3 
                        (c_arc u)
                        (normalArcs tri) 

            return (CNT u tri arcs)

        c_quads = do
            quad <- tINormalQuads tr
            u <- [0 .. ncCoefficient nc (iNormalDisc quad)-1]
            let arcs = map4 
                        (\arc -> c_arc (numberOfTriArcs arc + u) arc)
                        (normalArcs quad) 

            return (CNQ u quad arcs)

        c_arc u arc = c_arc_map ! (u, tnormalArc (tr,arc))

        c_arc_map = M.fromList $ do
            arc <- normalArcList tr
            let umax = numberOfTriArcs (unT arc) + numberOfQuadArcs (unT arc) - 1
            u <- [0 .. umax]

            let corners = map2 
                    (\icorner -> 
                        let
                            u_corner = case getOrientation tr (unT arc) icorner of 
                                            NoFlip -> u
                                            Flip -> umax-u
                        in
                            CNC u_corner (tnormalCorner (tr,icorner)))
                    (normalCorners (unT arc))



            return ((u,arc),CNA u arc corners)

        -- The number of normal triangles in the surface 'nc' containing the given normal arc 'arc'.
        -- Note that this is only well-defined in the disjoint union of tetrahedra, not in the quotient space!
        numberOfTriArcs arc = ncCoefficient nc (iNormalDisc $ iNormalTriByNormalArc arc) 

        numberOfQuadArcs arc = ncCoefficient nc (iNormalDisc $ iNormalQuadByNormalArc arc) 


    in
        CNS c_tris c_quads 




getOrientation tr (viewI -> I i narc) incorner@(viewI -> I i' ncorner) = 
    assert (i==i') $
    let
        v = normalArcGetVertex narc
        e = normalCornerGetContainingEdge ncorner
        directionOfArcs = i ./ oedge (v, otherVertex e v) 
        canonicalOIEdgeForCorner = 
            packOrderedFace NoFlip 
                (iNormalCornerGetContainingEdge 
                    (canonicalizeINormalCorner tr incorner))

    in
        fromJust (getOIEdgeGluingSense tr directionOfArcs canonicalOIEdgeForCorner) 

        


        




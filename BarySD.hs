{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-} 
module BarySD where

import Tetrahedron
import HomogenousTuples

data BaryTet = BaryTet (Triple Vertex)

data BaryVert = BaryVert0 Vertex
              | BaryVert1 Edge
              | BaryVert2 Triangle
              | BaryVert3

instance Vertices BaryTet where
    type Verts BaryTet = (Quadruple BaryVert)

    vertices (BaryTet (v0,v1,v2)) = 
        (   BaryVert0 v0,
            BaryVert1 (edge (v0,v1)),
            BaryVert2 (triangle (v0,v1,v2)),
            BaryVert3
        )
                                

--subdivideGluing :: Triangle -> OTriangle -> [(BaryTriangle,OBaryTriangle)]

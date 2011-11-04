{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-} 
module BarySD where

import AbstractTetrahedron
import HomogenousTuples

data BaryTet = BaryTet (Triple Vertex)

data BaryVert = BaryVert0 Vertex
              | BaryVert1 Edge
              | BaryVert2 Triangle
              | BaryVert3

instance Vertices BaryTet (Quadruple BaryVert) where
    vertices (BaryTet (v0,v1,v2)) = 
        (   BaryVert0 v0,
            BaryVert1 (edge (v0,v1)),
            BaryVert2 (triangle (v0,v1,v2)),
            BaryVert3
        )
                                

--subdivideGluing :: Triangle -> OTriangle -> [(BaryTriangle,OBaryTriangle)]

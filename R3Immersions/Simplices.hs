{-# LANGUAGE TypeFamilies, Rank2Types, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module R3Immersions.Simplices(     
    Coords(..),
    GeneralTriangleImmersion(..),
    EdgeImmersion(..),
    TriangleImmersion(..),
    GeneralEdgeImmersion(..),
    evalEdgeImmersion,
    evalFlatTriangleImmersion,
    GeneralTetImmersion(..),
    Resolution,
    standardTet,
    embedVert,
    embedEd,
    embedTri,
    vertexDefaultCoords
    )
 where

import PrettyUtil
import Numeric.AD.Vector
import Numeric.AD(FF,lowerFF,AD,lowerUF,UF,Mode)
import Data.Vect.Double.Base
import THUtil
import Data.VectorSpace
import Tetrahedron
import Data.Vect.Double.Util.Dim3

type Resolution = Int


data GeneralTriangleImmersion = 
    -- | SEMANTICS: 
    --
    -- Let (e0,e1,e2) = edges t
    --
    -- The arc from zero to vec2X corresponds to e0 
    --
    -- The arc from zero to vec2Y corresponds to e1 
    --
    -- The arc from vec2X to vec2Y corresponds to e2 
    GTE Doc Resolution (FF Tup2 Tup3 Double)

instance Show GeneralTriangleImmersion where
    showsPrec = prettyShowsPrec

instance Pretty GeneralTriangleImmersion where
    prettyPrec = prettyRecordPrec
            (\(GTE s _ e) ->
                let
                    f = pretty . lowerFF e . stdToUnit2
                in
                    ("GTE",
                        [("doc", s)
                        ,("im (1,0,0)",f tup3X)
                        ,("im (0,1,0)",f tup3Y)
                        ,("im (0,0,1)",f tup3Z)
                        ])) 

instance Coords GeneralTriangleImmersion where
    transformCoords f (GTE s res g) = GTE s res (f . g)

data TriangleImmersion = 
        FlatTriangle Vec3 Vec3 Vec3
    |   GeneralTriangle GeneralTriangleImmersion
    deriving Show

instance Pretty TriangleImmersion where
    prettyPrec prec (FlatTriangle a b c) = prettyPrecApp prec "FlatTriangle" [a,b,c] 
    prettyPrec prec (GeneralTriangle a) = prettyPrecApp prec "GeneralTriangle" [a] 

data GeneralEdgeImmersion = 
    -- | SEMANTICS: 
    --
    -- Let (v0,v1) = vertices e
    --
    -- 0 corresponds to v0
    --
    -- 1 corresponds to v1
    --
    GEE Doc Resolution (UF Tup3 Double)

instance Show GeneralEdgeImmersion where
    showsPrec = prettyShowsPrec

instance Pretty GeneralEdgeImmersion where
    prettyPrec = prettyRecordPrec
            (\(GEE s _ e) ->
                let
                    f = pretty . lowerUF e . stdToUnit1
                in
                    ("GEE",
                        [("doc", s)
                        ,("im (1,0)",f tup2X)
                        ,("im (0,1)",f tup2Y)
                        ])) 

instance Coords GeneralEdgeImmersion where
    transformCoords f (GEE s res g) = GEE s res (f . g)

data EdgeImmersion =
        FlatEdge Vec3 Vec3
    |   GeneralEdge GeneralEdgeImmersion
    deriving Show

instance Pretty EdgeImmersion where
    prettyPrec prec (FlatEdge a b) = prettyPrecApp prec "FlatEdge" [a,b] 
    prettyPrec prec (GeneralEdge a) = prettyPrecApp prec "GeneralEdge" [a] 

class Coords t where
    transformCoords :: (FF Tup3 Tup3 Double) -> t -> t

-- rotateAngleAxis :: Coords t => Double -> Vec3 -> t -> t
-- rotateAngleAxis = (.) transformCoords  . rotate3
--
evalEdgeImmersion :: EdgeImmersion -> UF Tup3 Double
evalEdgeImmersion (FlatEdge a0 a1) = 
    \u -> 
        let 
            res = interpol u (liftVec3 a0) (liftVec3 a1) 
        in $(assrt [| allReal res |] ['u, 'a0, 'a1]) res

evalEdgeImmersion (GeneralEdge (GEE _ _ f)) = f

data GeneralTetImmersion = GTetE Doc Resolution (FF Tup3 Tup3 Double)

instance Show GeneralTetImmersion where
    showsPrec = prettyShowsPrec

instance Pretty GeneralTetImmersion where
    prettyPrec =
        prettyRecordPrec
            (\(GTetE s _ e) ->
                let
                    f = pretty . lowerFF e . stdToUnit3
                in
                    ("GTetE",
                        [("doc", s)
                        ,("im (1,0,0,0)",f tup4X)
                        ,("im (0,1,0,0)",f tup4Y)
                        ,("im (0,0,1,0)",f tup4Z)
                        ,("im (0,0,0,1)",f tup4A)
                        ]))

evalFlatTriangleImmersion
  :: Vec3 -> Vec3 -> Vec3 -> FF Tup2 Tup3 Double
evalFlatTriangleImmersion a au av =
                            (\(Tup2 (u, v) :: Tup2 (AD s Double)) -> 
                                ((1-u-v) *^ liftVec3 a) 
                                ^+^ 
                                (u *^ liftVec3 au) 
                                ^+^ 
                                (v *^ liftVec3 av)

                                :: Tup3 (AD s Double)
                                )


standardTet
  :: (Num (t Double), Mode t) =>
     Tup4 (t Double) -> Tup3 (t Double)
standardTet (Tup4 (a,b,c,d)) =
        f a A ^+^ f b B ^+^ f c C ^+^ f d D
    where
        f x v = x *^ (liftVec3 . vertexDefaultCoords $ v)

    

embedVert :: Num a => Vertex -> Tup4 a
embedVert = stdBasisVector4 . toEnum . fromEnum 

embedEd
  :: (Num a1, Vertices a, Verts a ~ (Vertex, Vertex)) =>
     a -> Tup2 a1 -> Tup4 a1
embedEd (vertices -> (v0,v1)) (Tup2 (x0,x1)) = 
    x0 *^ embedVert v0 
    ^+^
    x1 *^ embedVert v1 

embedTri
  :: (Num a1, Vertices a, Verts a ~ (Vertex, Vertex, Vertex)) =>
     a -> Tup3 a1 -> Tup4 a1
embedTri (vertices -> (v0,v1,v2)) (Tup3 (x0,x1,x2)) = 
    x0 *^ embedVert v0 
    ^+^
    x1 *^ embedVert v1 
    ^+^
    x2 *^ embedVert v2 

-- | Embeds the abstract tetrahedron into R^3 symmetrically
vertexDefaultCoords :: Vertex -> Vec3
vertexDefaultCoords = (\x -> 1.5 *& (f x &- center_)) . viewVertex 
    where
        f A = vec3X
        f B = vec3Y
        f C = vec3Z
        f D = Vec3 1 1 1

        center_ = Vec3 0.5 0.5 0.5


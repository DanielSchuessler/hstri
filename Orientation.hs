{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TemplateHaskell, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
module Orientation where

import Math.Group
import Math.Groups.S2
import HomogenousTuples
import Triangulation
import THUtil
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Util
import PrettyUtil
import Control.Monad.State
import Data.EdgeLabelledTree

data SimplexOrientation a = AscOr | FlipAscOr
    deriving(Show,Eq,Ord)

instance Pretty (SimplexOrientation a) where prettyPrec = prettyPrecFromShow 

castSimplexOrientation :: SimplexOrientation a -> SimplexOrientation b
castSimplexOrientation AscOr = AscOr 
castSimplexOrientation FlipAscOr = FlipAscOr 

flipSo :: SimplexOrientation a -> SimplexOrientation a
flipSo AscOr = FlipAscOr
flipSo FlipAscOr = AscOr

divSo :: SimplexOrientation a -> SimplexOrientation a -> S2
divSo o1 o2 = if o1==o2 then NoFlip else Flip

instance RightAction S2 (SimplexOrientation a) where
    o *. NoFlip = o
    o *. Flip = flipSo o

inducedOrient32 :: SimplexOrientation AbsTet -> Triangle -> SimplexOrientation Triangle
inducedOrient32 o t = castSimplexOrientation $ 
    if triangleDualVertex t `elem2` (A,C)
        then o
        else flipSo o 
          

inducedOrient21 :: SimplexOrientation Triangle -> Triangle -> Edge -> SimplexOrientation Triangle
inducedOrient21 o t e = castSimplexOrientation $
    case triangleGetIndexOf t (link e t) of
        Nothing -> error ("inducedOrient21 "++ $(showExps ['t,'e])) 
        Just VT1 -> o *. Flip
        _ -> o

inducedOrient10 :: SimplexOrientation Edge -> Edge -> Vertex -> SimplexOrientation Vertex
inducedOrient10 o (vertices -> (v0,v1)) v = castSimplexOrientation $ 
    case () of
         _ | v0 == v -> o
           | v1 == v -> flipSo o
           | otherwise -> error ("inducedOrient10 "++ $(showExps ['v0,'v1,'v])) 
        

inducedOrient32o :: SimplexOrientation AbsTet -> OTriangle -> SimplexOrientation Triangle
inducedOrient32o o ot = castSimplexOrientation $ 
    inducedOrient32 o (forgetVertexOrder ot) *. s3sgn (getVertexOrder ot)  

inducedOrient21o :: SimplexOrientation Triangle -> Triangle -> OEdge -> SimplexOrientation Edge
inducedOrient21o o t oe = castSimplexOrientation $ 
    inducedOrient21 o t (forgetVertexOrder oe) *. getVertexOrder oe  


type NonOrientabilityEvidence = (VU.Vector Word8, Gluing)

type TriangulationOrientation = V.Vector (SimplexOrientation AbsTet) 

tOrGetTetOr :: TriangulationOrientation -> TIndex -> SimplexOrientation AbsTet
tOrGetTetOr tOr (tet :: TIndex) = tOr V.! fi tet 

tOrGetTriangleOr :: TriangulationOrientation -> ITriangle -> SimplexOrientation Triangle
tOrGetTriangleOr tOr tri = inducedOrient32 (tOrGetTetOr tOr (getTIndex tri)) (unI tri)


type PackedMaybeSimplexOrientation = Word8

orientTriangulation
  :: Triangulation
     -> Either
          NonOrientabilityEvidence          
          TriangulationOrientation
orientTriangulation tr = do
        check
        return (finish sol)


    where
        sol :: VU.Vector Word8
        sol = execState mkSol (VU.replicate n undetermined)

        n :: Int
        n = tNumberOfTetrahedra tr

        undetermined,asc,flipAsc :: PackedMaybeSimplexOrientation
        undetermined = 0
        asc = 1
        flipAsc = 2

        mkSol :: State (VU.Vector PackedMaybeSimplexOrientation) ()
        mkSol = goComponent 0 
        
        goComponent :: TIndex -> State (VU.Vector Word8) ()
        goComponent tet =
            goNode asc (dfsFacePairingGraph tr tet)

        goNode o_cur (Node cur es) = do
            setTetOr cur o_cur
            forM_ es
                (\(gl,node') -> goNode (requiredTetOr gl o_cur) node') 

        setTetOr :: TIndex -> PackedMaybeSimplexOrientation
                        -> State (VU.Vector PackedMaybeSimplexOrientation) ()
        setTetOr i' onew = do
            tro <- get
            case tro VU.! fi i' of
                o | o == undetermined ->
                            
                        put (tro VU.// [(fi i',onew)])

                  | otherwise -> error ("tet visited twice: "++show i')


        requiredTetOr gl o_dom = 
            let
                o1 = inducedOrient32 (toSor o_dom) (unI (glDom gl))
            in
                if inducedOrient32o AscOr (unI (glCod gl)) /= o1
                    then asc
                    else flipAsc
                
        check =
            forM_ (tGluingsIrredundant tr)
                (\gl -> 
                    let
                        domTet = getTIndex (glDom gl)
                        codTet = getTIndex (glCod gl)
                    in
                        if requiredTetOr gl (sol VU.! fi domTet) == sol VU.! fi codTet
                           then return ()
                           else Left (sol,gl))



        finish = V.map toSor . VG.convert

        toSor :: Word8 -> SimplexOrientation AbsTet
        toSor x | x == asc = AscOr :: SimplexOrientation AbsTet
                | x == flipAsc = FlipAscOr
                | x == undetermined = error "orientTriangulation: disconnected Triangulations not supported yet"
                | otherwise = 
                        error ("orientTriangulation/toSor: "++
                                $(showExps 'x))







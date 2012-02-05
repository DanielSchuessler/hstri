{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
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
import Test.QuickCheck
import Control.Exception as E
import Control.DeepSeq

#ifdef USE_YICES
import Yices.Painless.Base as Yices
import Control.Applicative
import System.IO.Unsafe
#endif


data SimplexOrientation a = AscOr | FlipAscOr
    deriving(Show,Eq,Ord)

instance Arbitrary (SimplexOrientation a) where arbitrary = elements[AscOr,FlipAscOr]
instance NFData (SimplexOrientation a)                                                

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
        Just I3_1 -> o *. Flip
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

gluingIsAscOrPreserving :: Gluing -> Bool
gluingIsAscOrPreserving gl =
                inducedOrient32 AscOr (unI (glDom gl))
                ==
                inducedOrient32o AscOr (unI (glCod gl))

#ifdef USE_YICES
orientTriangulation'
  :: Triangulation -> Either () TriangulationOrientation
orientTriangulation' tr = unsafePerformIO go 
  where
    tetVarName = ("T"++) . show . toInteger

    go = do
        cxt <- mkContext
        let tets = tTetrahedra_ tr
        decls <- mapM (mkBoolDecl cxt . tetVarName) tets 
        forM_ (tGluingsIrredundant tr) (\gl -> do
            var0 <- mkBool cxt . tetVarName . glDomTet $ gl
            var1 <- mkBool cxt . tetVarName . glCodTet $ gl
            Yices.assert cxt
                =<< (if gluingIsAscOrPreserving gl
                       then mkEq cxt var0 =<< mkNot cxt var1
                       else mkEq cxt var0 var1))




        --setVerbosity 5
        --ctxDump cxt
        res <- check cxt            
        case res of
             Unsatisfiable -> do
                 return (Left ())
--                 unsatCore <- map show <$> getUnsatCore cxt
--                 evaluate (rnf unsatCore)
--                 return (Left unsatCore)

             Satisfiable -> do
                 Just m <- getModel cxt
                 values <- V.forM (V.fromList decls)
                        (\decl ->
                            maybe AscOr (\b -> if b then AscOr else FlipAscOr)
                                <$> getValueBool m decl)

                        :: IO TriangulationOrientation

                 evaluate (rnf (V.toList values))
                 return (Right values)
                     
             Undefined -> error "yices returned Undefined" 
                
                        
#endif
                


orientTriangulation
  :: Triangulation
     -> Either
          NonOrientabilityEvidence          
          TriangulationOrientation
orientTriangulation tr = do
        _check
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
            if gluingIsAscOrPreserving gl
               then flipSo' o_dom
               else o_dom

        flipSo' o = if o == asc then flipAsc else E.assert (o==flipAsc) asc 
            
                
        _check =
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






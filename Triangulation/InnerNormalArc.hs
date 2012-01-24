{-# LANGUAGE TemplateHaskell #-}
module Triangulation.InnerNormalArc 
    (
    innNAsAroundVertex,
    InnNA(..),innNA_fst,innNA_snd,
    toInnNA,
    innNAs,
    innNAFromPreimage,
    normalTrisContainingInnNA,
    innNANU,
    triArcGraph
    )



    where

import Data.Tuple.Index
import TriangulationCxtObject
import Data.Numbering
import THUtil
import HomogenousTuples
import Control.Monad
import Data.Maybe
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph(mkGraph)
import Control.Arrow

normalTrisContainingInnNA :: InnNA -> Pair INormalTri
normalTrisContainingInnNA (InnNA ina1 ina2) = 
                        map2 iNormalTriByNormalArc (ina1, ina2)

data InnNA = InnNA INormalArc INormalArc 
    deriving (Eq,Ord,Show)

innNA_fst :: InnNA -> INormalArc
innNA_fst (InnNA x _) = x

innNA_snd :: InnNA -> INormalArc
innNA_snd (InnNA _ x) = x

innNAFromPreimage :: INormalArc -> INormalArc -> InnNA
innNAFromPreimage ina1 ina2 = 
    $(assrt [|ina1/=ina2|] ['ina1,'ina2])
    (uncurry InnNA (sort2 (ina1,ina2)))

innNANU :: Triangulation -> Numbering InnNA
innNANU = join (prodNu innNA_fst innNA_snd InnNA) . tINormalArcNu
        

toInnNA :: TNormalArc -> Maybe InnNA
toInnNA tna = 
    case normalArcPreimage tna of
                BoundaryNormalArc _ -> Nothing
                InnerNormalArc ina1 ina2 ->
                        Just (innNAFromPreimage ina1 ina2)
                                    

innNAs :: Triangulation -> [InnNA]
innNAs t = do
    gl <- tGluingsIrredundant t
    i <- allIndex3 
    return (InnNA
                (iNormalArcByTriangleAndVertexIndex (glDom gl) i)
                (iNormalArcByOITriangleAndVertexIndex (glCod gl) i))

innNAsAroundVertex :: TVertex -> [InnNA]
innNAsAroundVertex = mapMaybe toInnNA . tNormalArcsAroundVertex

triArcGraph :: Triangulation -> Gr INormalTri InnNA
triArcGraph tr = 
    mkGraph 
        (map (fromEnum &&& id) (tINormalTris tr)) 
        (map f (innNAs tr)) 

  where
    f tna = 
        let
            (t1,t2) = normalTrisContainingInnNA tna
        in (fromEnum t1,fromEnum t2,tna)

ecMember_T :: IVertex -> TVertex -> Bool
ecMember_T x y = eqvRep (vertexEqv (getTriangulation y)) x == unT y


{-# LANGUAGE CPP, TupleSections, ExtendedDefaultRules #-}
import ExampleTriangulations
import NormalEverything
import Element
import Blenderable
import Blender
import DisjointUnion
import ConcreteNormal.PreRenderable
import Simplicial.DeltaSet
import Simplicial.AnySimplex
import TriangulationCxtObject
import HomogenousTuples
import Data.Function
import PrettyUtil
import Data.Vect.Double.Base
import Latexable
import EqvGraphs
import QuadCoordinates
import VerboseDD
import Control.Applicative
import qualified Data.Map as M
import Tikz.Gen
import Tikz.Preview




tr = tr_l31
spqwc = spqwc_l31

smes :: IO ()
smes = putStr (latexifyStandardMatchingEquations tr)

qmes = putStr (latexifyQMatchingEquations tr)

main = (testBlender . setCams [twoTetCam])
        (defaultScene $
            (
                (fromSpqwc spqwc)
--             `disjointUnion`
   --             fromNormalSurface spqwc (standardCoordinates [0./Q_ac,1./Q_ac,2./Q_ad,3./Q_ad])
            )
        )
    


#ifdef PRODUCE
writeFileOrPreview fn x = putStrLn fn >> writeFile fn x
#else
writeFileOrPreview = const previewTikz
#endif


vl_A0 way =  
    let 
        v = pMap tr_l31 (iNormalTriGetVertex $ head triOrder)
        triOrder = flip (./) <$> [ntA,ntC,ntB] <*> [0,1] 

        nodePerm = (M.!) . M.fromList . zip triOrder $
            [S3cba,mempty
            ,S3acb,S3cab
            ,mempty,S3acb
            ]
                    

        tikz = tikzVertexLink 
                v 
                (regularPolygonLocs triOrder) 
                nodePerm 
                ("x=3cm,y=3cm,scale=0.8,scale="++show scaleTweak)
                noExtraEdgeStyles
                way
    in 
        writeFileOrPreview ("/tmp/L31VertexLinkA0"++suf++".tex") tikz

vl_D0 way =

    let 
        v = pMap tr_l31 (iNormalTriGetVertex $ head triOrder)
        triOrder = flip (./) <$> [ntD] <*> [1,0] 

        nodePerm = (M.!) . M.fromList . zip triOrder $
            [S3acb,mempty
            ]
                    

        tikz = tikzVertexLink 
                v 
                (regularPolygonLocs triOrder) 
                nodePerm 
                ("x=3cm,y=3cm,shift={(0.5,2)},scale=0.4,scale="++show scaleTweak)
                (\x -> if x == 0 ./ tABD then "looseness=2" else "") 
                way
                
    in 
        writeFileOrPreview ("/tmp/L31VertexLinkD0"++suf++".tex") tikz


qVertexSols :: [QuadCoordinates Integer]
qVertexSols = [ quad_fromAssocs [(0./q,1),(1./q,1)]
                | q <- [ Q_ab,Q_ac,Q_ad ] ]

suf = "_100010"

scaleTweak = 0.8

graphs = do
    let way = QuadGiven 
                    (quad_fromAssocs [(0./Q_ab,1),(1./Q_ac,1)]) 
                    --(qVertexSols !! 0)
    vl_A0 way 
    vl_D0 way

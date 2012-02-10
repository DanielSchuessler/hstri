{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, ImplicitParams, CPP, TupleSections, ExtendedDefaultRules #-}
import HsTri
import qualified Data.Map as M
import Data.Map(Map)
import Control.Applicative
import Control.Exception
import Control.DeepSeq
import FileLocation




tr = tr_l31
spqwc = spqwc_l31

smes :: IO ()
smes = putStr (latexifyStandardMatchingEquations tr)

qmes = putStr (latexifyQMatchingEquations tr)

go fp ba = (testBlender . setCams [twoTetCam] . setRenderFilepath fp)
        (defaultScene $ ba)


main = do
    go "/h/dipl/pictures/L31.png" (fromSpqwc spqwc)
--     go baVertexLinkA0
--     go baVertexLinkD0
    

p = pMap tr

(baVertexLinkA0,baVertexLinkD0) = 
    map2 (\v ->
                fromSpqwcAndIntegerNormalSurface 
                    spqwc
                    (vertexLinkingSurface (p (0./ v))))

         (vA,vD)


--             `disjointUnion`
   --             fromNormalSurface spqwc (standardCoordinates [0./Q_ac,1./Q_ac,2./Q_ad,3./Q_ad])
    


#ifdef PRODUCE
writeFileOrPreview fn x = putStrLn fn >> writeFile fn x
#else
writeFileOrPreview = const previewTikz
#endif


vl_A0 way =  
    let 
        v = p (head triOrder)
        triOrder = flip (./) <$> [vA,vC,vB] <*> [0,1] 

        nodePerm = flip $(indx) . M.fromList . zip triOrder $
            [S3cba,mempty
            ,S3acb,S3cab
            ,mempty,S3acb
            ]
                    

        tikz = 
            let ?layout = 
                    SGL 
                        (ptcToIVertex_fromS3s nodePerm)
                        (regularPolygonLocs triOrder) 
            in
                tikzStructureGraphForVertexLink 
                    v 
                    (SGEE
                        ("x=3cm,y=3cm,scale=0.8,scale="++show scaleTweak)
                        noExtraEdgeStyles
                        way)
    in 
        writeFileOrPreview ("/tmp/L31VertexLinkA0"++suf++".tex") tikz

vl_D0 way =

    let 
        v = p (head triOrder)
        triOrder = flip (./) <$> [vD] <*> [1,0] 

        nodePerm = flip $(indx) . M.fromList . zip triOrder $
            [S3acb,mempty
            ]
                    

        tikz = 
            let ?layout = 
                    SGL
                        (ptcToIVertex_fromS3s nodePerm)
                        (regularPolygonLocs triOrder) 
            in
                tikzStructureGraphForVertexLink 
                    v 
                    (SGEE
                        ("x=3cm,y=3cm,shift={(0.5,2)},scale=0.4,scale="++show scaleTweak)
                        (\x -> if x == 0 ./ tABD then "looseness=2" else "") 
                        way)
                
    in 
        writeFileOrPreview ("/tmp/L31VertexLinkD0"++suf++".tex") tikz


qVertexSols :: [QuadCoordinates Integer]
qVertexSols = [ quad_fromAssocs [(0./q,1),(1./q,1)]
                | q <- [ Q_ab,Q_ac,Q_ad ] ]

suf = "_100010"

scaleTweak = 0.8

graphs = do
    let way = -- QuadGiven (quad_fromAssocs [(0./Q_ab,1),(1./Q_ac,1)]) --(qVertexSols !! 0)
                NoQuad
    vl_A0 way 
    vl_D0 way

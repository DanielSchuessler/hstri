{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, ImplicitParams, CPP, TupleSections, ExtendedDefaultRules #-}
import HsTri
import qualified Data.Map as M
import Data.Map(Map)
import Control.Applicative
import Control.Exception
import Control.DeepSeq
import FileLocation
import Data.Lens.Common
import Triangulation.AbstractNeighborhood
import Data.Maybe
import Util
import Latexable
import Tikz.Gen
import Math.SparseVector
import Blender.Blenderable


bcmd = DoRender

tr = tr_l31
spqwc = spqwc_l31

smes :: IO ()
smes = putStr (latexifyStandardMatchingEquations tr)

qmes = putStr (latexifyQMatchingEquations "0.7em" "0.5em" tr)

go fp ba = 
    blenderMain bcmd . setCams [twoTetCam] . setRenderFilepath fp . setRenderRes 1200 1200
    . defaultScene
    .
            setTrisTranspJust 
                        (defaultTrans 1) {
                            _fresnel = 1.5,
                            _fresnel_factor = 1.25,
                            _translucency = 0.8
                        }

            
        
    $ ba


main = do
--     go "/h/dipl/pictures/L31.png" (fromSpqwc spqwc)
    go "/h/dipl/pictures/L31vertexLinkD0.png" baVertexLinkD0
    go "/h/dipl/pictures/L31vertexLinkA0.png" baVertexLinkA0
    putStrLn "Done"
    

p = pMap tr


baVertexLinkA0 = 
    (modL ba_prL (pr_setTriVisibility (const Invisible))
        $ fromSpqwc spqwc)

    `disjointUnion`

    fromIntegerNormalSurface spqwc (vertexLinkingSurface (p (0./ vA)))




baVertexLinkD0 = 
        fromSpqwcAndIntegerNormalSurface 
            spqwc
            (vertexLinkingSurface (p (0./ vD)))



--             `disjointUnion`
   --             fromNormalSurface spqwc (standardCoordinates [0./Q_ac,1./Q_ac,2./Q_ad,3./Q_ad])
    


#ifdef PRODUCE
writeFileOrPreview fn x = putStrLn fn >> writeFile fn x
#else
writeFileOrPreview = const previewTikz
#endif


iverts= tIVertices tr

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
                    SGL iverts []
                        (funToMap iverts (ptcToIVertex_fromS3s nodePerm))
                        (funToMap iverts (regularPolygonLocs triOrder))
                        $undef
                        sparse_empty
            in
                tikzStructureGraph
                    tr 
                    (SGEE
                        ["x=3cm","y=3cm","scale=0.8","scale="++show scaleTweak]
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
                    SGL iverts []
                        (funToMap iverts (ptcToIVertex_fromS3s nodePerm))
                        (funToMap iverts (regularPolygonLocs triOrder))
                        $undef
                        sparse_empty
            in
                tikzStructureGraph
                    tr 
                    (SGEE
                        [ "x=3cm","y=3cm","shift={(0.5,2)}","scale=0.4","scale="++show scaleTweak ]
                        (\x -> if x == 0 ./ tABD then ["looseness=2"] else []) 
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



ens =
          unlines
        . map ((++"\\\\") . either toLatex toLatex . someEdgeNeighborhood)
        . edges $ tr
        

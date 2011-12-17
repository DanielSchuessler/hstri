{-# LANGUAGE FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS -Wall #-}
module FaceLattice where

import Control.DeepSeq
import Control.Monad.Reader
import Data.Bits
import Data.Colour as Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB as SRGB
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz as GraphViz
import Data.GraphViz.Attributes.Colors as GraphVizColors
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.List(genericLength)
import Data.Maybe
import Data.Text.Lazy as Text
import IndexedSimplices
import System.Process
import System.SimpleArgs(getArgs)
import Triangulation
import TriangulationCxtObject
import qualified Data.Text.Lazy.IO as TextIO
import AbstractTetrahedron
import Control.Exception
import System.Exit
import QuickCheckUtil

class GraphvizFormatable a y | a -> y where fmt :: a -> y

i2fieldLabel :: (Show a, HasTIndex ia a) => ia -> RecordField
i2fieldLabel (viewI -> I i a) = FieldLabel (Text.pack $ show i ++ "." ++ show a)

instance GraphvizFormatable ITriangle RecordField where fmt = i2fieldLabel
instance GraphvizFormatable OITriangle RecordField where fmt = i2fieldLabel
instance GraphvizFormatable IEdge RecordField where fmt = i2fieldLabel
instance GraphvizFormatable IVertex RecordField where fmt = i2fieldLabel

instance GraphvizFormatable (FL,Node,TIndex) Attributes where
    fmt (_,_,i) = 
            [ Label (StrLabel (Text.pack $ show i)) ]

instance GraphvizFormatable (FL,Node,TTriangle) Attributes where
    fmt (_,_,x) = 
            [ Shape Record
            , Label (RecordLabel (fmap fmt (asList $ trianglePreimage x)))
            ]

instance GraphvizFormatable (FL,Node,TEdge) Attributes where
    fmt (_,_,x) = 
            [ Shape Record
            , Label (RecordLabel (fmap fmt (equivalentIEdges x)))
            ]

instance GraphvizFormatable (FL,Node,TVertex) Attributes where
    fmt (_,_,x) = 
            [ Shape Record
            , Label (RecordLabel (fmap fmt (equivalentIVertices x)))
            ]

class FLN_Id a where
    flnId :: a -> Int

tagBits :: Int
tagBits = 3

instance FLN_Id TIndex where
    flnId i = shiftL (fromEnum i) tagBits .|. 3  

faceLatticeNodeId' :: Enum b => Int -> T b -> Int
faceLatticeNodeId' tag x = shiftL (fromEnum . unT $ x) tagBits .|. tag 

instance FLN_Id TTriangle where flnId = faceLatticeNodeId' 2 
instance FLN_Id TEdge where flnId = faceLatticeNodeId' 1 
instance FLN_Id TVertex where flnId = faceLatticeNodeId' 0 

-- | Face Lattice Node class
class (Show a, GraphvizFormatable (FL,Node,a) Attributes) 
    => FLN_Class a where
        
        calculateFlnColour :: FL -> Node -> a -> Colour Double 
        calculateFlnColour _ _ _ = sRGB 1 1 1

instance FLN_Class TIndex where
    calculateFlnColour fl n _ = averageColour fl (pre fl n)

instance FLN_Class TTriangle where
    calculateFlnColour _ _ x = uncurryRGB sRGB (hsv (fromIntegral ((1223 * fromEnum (unT x)) `mod` 360) :: Double) 1 1)

instance FLN_Class TEdge where
    calculateFlnColour fl n _ = averageColour fl (suc fl n)

instance FLN_Class TVertex where
    calculateFlnColour fl n _ = averageColour fl (suc fl n)




averageColour :: FL -> [Node] -> Colour Double
averageColour fl (node0:nodes') = affineCombo [ (w, getColour node) | node <- nodes' ] (getColour node0)
    where
        w = 1 / (1 + genericLength nodes') 
        getColour = flnColour . fromJust . lab fl
averageColour _ _ = assert False undefined




-- | Face Lattice Node
data FLN = forall a. FLN_Class a => FLN a (Colour Double)

flnColour :: FLN -> Colour Double
flnColour (FLN _ c) = c

-- | Face Lattice Edge
type FLE = Colour Double

type FL = Gr FLN FLE

instance Show FLN where show (FLN a _) = show a
    
instance GraphvizFormatable (FL, LNode FLN) Attributes where 
    fmt (fl, (i, FLN a colour)) = Color [color_] : FontColor color_ : fmt (fl,i,a) 
        where
            color_ = colour2color colour    


colour2color :: (Floating b, RealFrac b) => Colour b -> Color
colour2color colour = uncurryRGB GraphVizColors.RGB (toSRGB24 colour)


faceLattice ::  Triangulation -> Gr FLN FLE
faceLattice = do
        tets <- tTetrahedra_ 
        ts <- triangles 
        es <- AbstractTetrahedron.edges
        vs <- vertices

        let 
            nodes_ = tetNodes ++. (triangleNodes ++. (edgeNodes ++. (vertexNodes ++. [])))

            (++.) :: [(x,LNode FLN)] -> [LNode FLN] -> [LNode FLN]
            xs ++. ys = fmap snd xs ++ ys 

            tetNodes = fmap mkNode tets 
            triangleNodes = fmap mkNode ts 
            edgeNodes = fmap mkNode es 
            vertexNodes = fmap mkNode vs


            mkNode :: (FLN_Id x, FLN_Class x) => x -> (x, LNode FLN)
            mkNode x = (x, (ix, FLN x (calculateFlnColour result ix x)))
                where
                    ix = flnId x

            subfaces :: (IsSubface x y) => (FLN -> FLN -> Colour Double) -> [(x,LNode FLN)] -> [(y, LNode FLN)] -> [LEdge FLE]
            subfaces colourer xs ys = [ (ix, iy, colourer flnx flny) 
                                            | ((x,(ix,flnx)),(y,(iy,flny))) <- cart xs ys, 
                                                isSubface x y ] 

            colourFromSource x _ = flnColour x
            colourFromTarget _ y = flnColour y

            edges_ =   subfaces colourFromSource triangleNodes tetNodes 
                    ++ subfaces colourFromTarget edgeNodes triangleNodes
                    ++ subfaces colourFromTarget vertexNodes edgeNodes

            result = insEdges edges_ (insNodes nodes_ gempty)

        return result
                

gempty :: Gr FLN FLE
gempty = G.empty

dotGraph ::  Triangulation -> (DotGraph Node)
dotGraph = do
    fl <- faceLattice

    
    return $ graphToDot (nonClusteredParams { 

            fmtEdge = 
                (\(_,_,colour2color -> c) -> [ Color [c] ]) 
            
            , 
            fmtNode = fmt . (fl,) ,
            globalAttributes = 
                [   EdgeAttrs [ Color [white], FontColor white, PenWidth 2 ] 
                ,   GraphAttrs [ BgColor (X11Color Black)
                               , RankDir FromBottom
                               , RankSep [2.4] 
                               , Overlap RemoveOverlaps
                               , Splines SplineEdges
                               
                               ] 
                ,   NodeAttrs [ PenWidth 2 ]
                ]
        
        }) fl



white :: Color
white = X11Color White

test_FaceLattice :: IO ()
test_FaceLattice = do
    (nTets,nGluings) <- getArgs
    t <- randomTriangulation nTets nGluings
    viewFL t

viewFL ::  Triangulation -> IO ()
viewFL t = do
    putStrLn "\n\n=== TRIANGULATION ===\n"
    print t
    let dotGraph0 = dotGraph t 
--     putStrLn "\n\n=== DOTGRAPH ===\n"
--     print dotGraph0 
--     putStrLn "\n\n=== DOT CODE ===\n"
--     putStrLn =<< prettyPrint dotGraph0
    TextIO.writeFile "out.dot" $ printIt dotGraph0

--     res <- runGraphviz dotGraph0 Svg "out.svg"
--     either error (\x -> rawSystem "gwenview" [x]) res
    --runGraphvizCanvas Dot dotGraph0 Xlib
    ec <- system "dot out.dot -Tsvg > out.svg && gwenview out.svg"
    exitWith ec
 
instance NFData FLN where
    rnf (FLN x c) = x `seq` rnf c


{-# LANGUAGE FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module FaceLattice(viewFL,test_FaceLattice,FaceLatticeRenderStyle(..),flrs_screen,flrs_paper) where

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
import qualified Data.Text.Lazy as Text
import IndexedSimplices
import System.Process
import System.SimpleArgs(getArgs)
import Triangulation
import TriangulationCxtObject
import qualified Data.Text.Lazy.IO as TextIO
import AbstractTetrahedron
import Control.Exception
import QuickCheckUtil
import System.Exit
import Latexable
import DotUtil hiding(mkNode)

class GraphvizFormatable a y | a -> y where 
    fmt :: FaceLatticeRenderStyle -> a -> y


--fmtI0 :: (Show a, HasTIndex ia a) => ia -> RecordField
--fmtI0 (viewI -> I i a) = (FieldLabel . Text.pack) (show i ++ "." ++ show a)
--fmtI0 (viewI -> I i a) = (FieldLabel . Text.pack) (show a ++ showSubscript i)
-- fmtI0 :: Latexable a => a -> RecordField
-- fmtI0 = FieldLabel . Text.pack . toLatex

--fmtI :: (Show a, HasTIndex ia a) => FaceLatticeRenderStyle -> ia -> RecordField
-- fmtI :: Latexable a => FaceLatticeRenderStyle -> a -> RecordField
-- fmtI = const fmtI0 

-- instance GraphvizFormatable ITriangle RecordField where fmt = fmtI
-- instance GraphvizFormatable OITriangle RecordField where fmt = fmtI
-- instance GraphvizFormatable IEdge RecordField where fmt = fmtI
-- instance GraphvizFormatable OIEdge RecordField where fmt = fmtI
-- instance GraphvizFormatable IVertex RecordField where fmt = fmtI

theShape :: Shape
theShape = Ellipse

instance GraphvizFormatable (FL,Node,TIndex) Attributes where
    fmt _ (_,_,i) = 
            [ Shape theShape
            , (Label . StrLabel . Text.pack . mathmode. show) i ]

instance GraphvizFormatable (FL,Node,TTriangle) Attributes where
    fmt o (_,_,x) = 
            [ Shape theShape
            , setlbl o x
            ]

instance GraphvizFormatable (FL,Node,TEdge) Attributes where
    fmt o (_,_,x) = 
            [ Shape theShape
            , setlbl o x
            ]

instance GraphvizFormatable (FL,Node,TVertex) Attributes where
    fmt o (_,_,x) = 
            [ Shape theShape
            , setlbl o x
            ]

setlbl :: (AsList a, Latexable (Element a)) => t -> a -> Attribute
setlbl _ = Label . StrLabel . Text.pack . mathmode . latexSet

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
    fmt opts (fl, (i, FLN a colour)) = 
            (guard (flrs_doColor opts) >>
                [Color [color_] , FontColor color_])
            ++ fmt opts (fl,i,a) 
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

dotGraph
  :: FaceLatticeRenderStyle -> Triangulation -> DotGraph Node
dotGraph opts@FLRS{ flrs_doColor = doColor } = do
    fl <- faceLattice

    
    return $ graphToDot (nonClusteredParams { 

            fmtEdge = 
                (\(_,_,colour2color -> c) -> 
                    (guard doColor >> [ Color [c] ])
                ) 
            
            , 
            fmtNode = fmt opts . (fl,) ,
            globalAttributes = 
                [   EdgeAttrs (
                        (guard doColor >> [ Color [white], FontColor white ])
                        ++ [PenWidth 2] 
                    )
                ,   GraphAttrs ( 
                        (guard doColor >> [ BgColor (X11Color Black) ])
                        ++
                        [ 
                                 RankDir FromBottom
--                                , RankSep [2.4] 
--                                , Overlap RemoveOverlaps
                               , Splines SplineEdges
                         ] 
                         )
                ,   NodeAttrs [ PenWidth 2 ]
                ]
        
        }) fl



white :: Color
white = X11Color White

test_FaceLattice :: FaceLatticeRenderStyle -> IO ExitCode
test_FaceLattice opt = testDot (dotGraph opt)

viewFL :: FaceLatticeRenderStyle -> Triangulation -> IO ExitCode
viewFL opt tr = viewDot (dotGraph opt tr)
 
instance NFData FLN where
    rnf (FLN x c) = x `seq` rnf c

data FaceLatticeRenderStyle = 
    FLRS {
        flrs_doColor :: Bool

    }

flrs_screen :: FaceLatticeRenderStyle
flrs_screen = FLRS True
flrs_paper :: FaceLatticeRenderStyle
flrs_paper = FLRS False

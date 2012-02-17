{-# LANGUAGE ExistentialQuantification, TemplateHaskell, QuasiQuotes, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
module Tikz.Base where
import Data.String.Interpolation
import HomogenousTuples
import Element
import Data.List(intercalate)
import Data.Monoid(mappend,mempty,mconcat)
import FileLocation

type TikzDimension = String
type TikzFactor = Double

class TikzLength a where
    renderTikzLength :: a -> Tikz

instance TikzLength TikzDimension where renderTikzLength = id
instance TikzLength TikzFactor where renderTikzLength = show

type Tikz = String
type Degrees = Double
type TikzNodeName = String

data TikzLoc = XYPolar Degrees TikzFactor
             | CanvasPolar Degrees TikzDimension
             | XY (TikzFactor, TikzFactor)
             | Canvas (TikzDimension, TikzDimension)
             | Distance TikzLoc TikzDimension TikzLoc
             | DistanceRot TikzLoc TikzDimension Degrees TikzLoc
             | UnknownTikzLoc String
             | NodeLoc TikzNodeName
             | Anchor TikzNodeName String

    deriving Show


type TikzPath = [(TikzGraphicsOptions,TikzPathOp)] 

type TikzStyle = [String]

type TikzGraphicsOptions = [String]
type TikzToOptions = [String]

data TikzPathOp = TikzMoveTo TikzLoc
                | TikzLineTo TikzLoc
                | TikzTo TikzToOptions TikzLoc
                | forall l. TikzLength l => TikzCircle l
                | forall lx ly. (TikzLength lx, TikzLength ly) => 
                    TikzArc { 
                        startAngle,endAngle :: Degrees, 
                        arcRadiusX :: lx,
                        arcRadiusY :: ly
                        }




data TikzNode = TikzNode {
    n_style :: String,
    n_loc :: TikzLoc,
    n_lbl :: Tikz
}

tikzOrigin :: TikzLoc
tikzOrigin = XY (0, 0)

renderTikzLoc :: TikzLoc -> Tikz
renderTikzLoc (XYPolar a r) = [str|($:a$:$:r$)|]
renderTikzLoc (CanvasPolar a r) = [str|($:a$:$r$)|]
renderTikzLoc (XY (x, y)) = [str|($:x$,$:y$)|]
renderTikzLoc (Canvas (x, y)) = [str|($x$,$y$)|]
renderTikzLoc (Distance a p b) = [str|($$$renderTikzLoc a$!$p$!$renderTikzLoc b$$$)|]
renderTikzLoc (DistanceRot a p ang b) = [str|($$$renderTikzLoc a$!$p$!$:ang$:$renderTikzLoc b$$$)|]
renderTikzLoc (UnknownTikzLoc s) = s
renderTikzLoc (NodeLoc x) = "("++show x++")"
renderTikzLoc (Anchor x _anchor) = "("++show x++"."++show _anchor++")"



ptLength :: Double -> [Char]
ptLength x = show x ++ "pt"




renderTikzStyle :: TikzStyle -> Tikz
renderTikzStyle = intercalate ", "

data PaperTriangleSide = PTS_NW | PTS_S | PTS_NE 
    deriving Show

allPaperTriangleSides :: [PaperTriangleSide]
allPaperTriangleSides = asList allPaperTriangleSides'
allPaperTriangleSides'
  :: (PaperTriangleSide, PaperTriangleSide, PaperTriangleSide)
allPaperTriangleSides' = (PTS_NW,PTS_S,PTS_NE)

pts_toTikz :: PaperTriangleSide -> Tikz
pts_toTikz x = "side " ++ show (case x of
                                    PTS_NW -> 1::Int
                                    PTS_S  -> 2
                                    PTS_NE -> 3)

pts_cornersCCW
  :: PaperTriangleSide -> (PaperTriangleCorner, PaperTriangleCorner)
pts_cornersCCW x = case x of
                        PTS_NW -> (PTC_N,PTC_SW)
                        PTS_S -> (PTC_SW,PTC_SE)
                        PTS_NE -> (PTC_SE,PTC_N)

data PaperTriangleCorner = PTC_N | PTC_SW | PTC_SE 
    deriving Show

allPaperTriangleCorners :: [PaperTriangleCorner]
allPaperTriangleCorners = asList allPaperTriangleCorners'

allPaperTriangleCorners' :: (Triple PaperTriangleCorner)
allPaperTriangleCorners' = (PTC_N,PTC_SW,PTC_SE)

ptc_toTikz :: PaperTriangleCorner -> Tikz
ptc_toTikz x = "corner " ++ show (case x of
                                    PTC_N  -> 1::Int
                                    PTC_SW -> 2
                                    PTC_SE -> 3)

-- | Prepends a comma unless the argument is empty or starts with a comma already
ensureComma :: [Char] -> [Char]
ensureComma "" = ""
ensureComma s@(',':_) = s
ensureComma s = ", "++s

-- | Degrees, with 0 degree pointing east
sideAngle :: PaperTriangleSide -> Int
sideAngle x = case x of
                   PTS_NW -> 150
                   PTS_S  -> -90
                   PTS_NE -> 30


simpleNode :: TikzLoc -> Tikz -> TikzNode
simpleNode = TikzNode ""

            
renderNode :: TikzNode -> [Char]
renderNode TikzNode{..} = [str|\node [$n_style$] at $renderTikzLoc n_loc$ {$n_lbl$};|] ++"\n"

renderOptions :: [String] -> Tikz
renderOptions [] = mempty
renderOptions xs = "["`mappend`intercalate ", " xs`mappend`"]" 

renderPathOp :: TikzPathOp -> Tikz
renderPathOp (TikzMoveTo l) = renderTikzLoc l
renderPathOp (TikzLineTo l) = "-- " `mappend` renderTikzLoc l
renderPathOp (TikzTo o l) = "to["`mappend`renderOptions o `mappend` "] " `mappend` renderTikzLoc l
renderPathOp (TikzCircle l) = "circle" `mappend` renderOptions ["radius="`mappend`renderTikzLength l]
renderPathOp (TikzArc s e rx ry) = 
    "arc" `mappend` renderOptions ["start angle="`mappend`show s
                                  ,"end angle="`mappend`show e
                                  ,"x radius="`mappend`renderTikzLength rx
                                  ,"y radius="`mappend`renderTikzLength ry
                                  ]

renderPath :: TikzPath -> Tikz
renderPath p = "\\path" `mappend` 
    mconcat
    [ (if null o 
          then " "
          else renderOptions o `mappend` " ")
      `mappend`
      renderPathOp op

        | (o,op) <- p ]

        `mappend` ";\n"



mkDrawPath :: TikzPath -> TikzPath
mkDrawPath ((o,x):rest) = ("draw":o,x) : rest
mkDrawPath [] = $undef

simpleDraw :: TikzGraphicsOptions -> TikzLoc -> [TikzPathOp] -> Tikz
simpleDraw opts loc0 pathOps = renderPath (("draw":opts,TikzMoveTo loc0):map ([],) pathOps)

tikzpicture :: [String] -> Tikz -> Tikz
tikzpicture opts bod = 
    unlines 
        ["\\begin{tikzpicture}"++renderOptions opts
        ,bod
        ,"\\end{tikzpicture}"]


renderScope :: [String] -> [Char] -> [Char]
renderScope opts bod = "\\begin{scope}"++renderOptions opts++bod++"\\end{scope}" 

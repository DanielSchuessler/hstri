{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wall #-}
module Tikz.Base where
import Data.String.Interpolation
import HomogenousTuples
import Element
import Data.List(intercalate)

type Tikz = String

type Degrees = Double

data TikzLoc = Polar { tl_degrees :: Degrees, tl_radius :: TikzLength }
             | Cartesian (TikzLength, TikzLength)
             | Distance TikzLoc TikzLength TikzLoc
             | DistanceRot TikzLoc TikzLength Degrees TikzLoc
             | UnknownTikzLoc String


    deriving Show

tikzOrigin :: TikzLoc
tikzOrigin = Cartesian ("0", "0")

renderTikzLoc :: TikzLoc -> Tikz
renderTikzLoc (Polar a r) = [str|($:a$:$r$)|]
renderTikzLoc (Cartesian (x, y)) = [str|($x$,$y$)|]
renderTikzLoc (Distance a p b) = [str|($$$renderTikzLoc a$!$p$!$renderTikzLoc b$$$)|]
renderTikzLoc (DistanceRot a p ang b) = [str|($$$renderTikzLoc a$!$p$!$:ang$:$renderTikzLoc b$$$)|]
renderTikzLoc (UnknownTikzLoc s) = s

type TikzLength = String

scopeUnitLength :: Double -> String
scopeUnitLength = show

ptLength :: Double -> [Char]
ptLength x = show x ++ "pt"



type TikzNodeName = String

type TikzStyle = [String]

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


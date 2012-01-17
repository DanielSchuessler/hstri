{-# LANGUAGE QuasiQuotes #-}
module Tikz.Base where
import Data.String.Interpolation
import HomogenousTuples
import Element


data TikzLoc = Polar { tl_degrees :: Int, tl_radius :: TikzLength }

renderTikzLoc (Polar a r) = [str| ($:a$:$:r$) |]

type TikzLength = Int


type TikzNodeName = String

type TikzStyle = String


data PaperTriangleSide = PTS_NW | PTS_S | PTS_NE 
    deriving Show

allPaperTriangleSides = asList allPaperTriangleSides'
allPaperTriangleSides' = (PTS_NW,PTS_S,PTS_NE)

pts_toTikz x = "side " ++ show (case x of
                                    PTS_NW -> 1
                                    PTS_S  -> 2
                                    PTS_NE -> 3)

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

ptc_toTikz x = "corner " ++ show (case x of
                                    PTC_N  -> 1
                                    PTC_SW -> 2
                                    PTC_SE -> 3)

ensureComma "" = ""
ensureComma s@(',':_) = s
ensureComma s = ", "++s

-- | Degrees, with 0 degree pointing east
sideAngle :: PaperTriangleSide -> Int
sideAngle x = case x of
                   PTS_NW -> 150
                   PTS_S  -> -90
                   PTS_NE -> 30


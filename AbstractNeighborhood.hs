{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module AbstractNeighborhood(
    EdgeNeighborhoodTet,
    IEdgeNeighborhoodTet,
    ent_top,ent_bot,ent_left,ent_right,ent_leftTri,ent_rightTri,
    innerEdgeNeighborhood,
    prop_innerEdgeNeighborhood,
    -- * Testing
    qc_AbstractNeighbordhood
    ) where
import TriangulationCxtObject
import Data.Function
import Control.Arrow((&&&))
import PrettyUtil
import Data.List(unfoldr)
import Test.QuickCheck
import QuickCheckUtil
import Data.Maybe
import Test.QuickCheck.All

data EdgeNeighborhoodTet = 
    ENTet {
        ent_top, ent_bot, ent_left, ent_right :: Vertex
    }
    deriving Show

trivialHasTIndexInstance [t| EdgeNeighborhoodTet |]                
type IEdgeNeighborhoodTet = I EdgeNeighborhoodTet

instance Eq EdgeNeighborhoodTet where
    (==) = (==) `on` (ent_top &&& ent_bot &&& ent_left) 

instance Pretty EdgeNeighborhoodTet where
    prettyPrec prec (ENTet a b c d) = prettyPrecApp prec (text "ENTet") [a,b,c,d] 

ent_leftTri :: IEdgeNeighborhoodTet -> OITriangle
ent_leftTri  = mapI (\ent -> otriangle (ent_top ent, ent_bot ent, ent_left  ent))
ent_rightTri :: IEdgeNeighborhoodTet -> OITriangle
ent_rightTri = mapI (\ent -> otriangle (ent_top ent, ent_bot ent, ent_right ent))


abstractEdgeNeighborhood :: S2 -> TEdge -> [IEdgeNeighborhoodTet]
abstractEdgeNeighborhood dir te = 
    let
        tr = getTriangulation te
        
        e = unT te
        i0 = getTIndex e        

        ient0 = i0 ./ ENTet {..}
            where
                (ent_top,ent_bot) = vertices (forgetTIndex e)
                (ent_left,ent_right) = vertices (oppositeEdge (forgetTIndex e)) *. dir
    in 
        ient0 :

        unfoldr (\prev -> do
                        _S <- lookupGluingOfOITriangle tr (ent_rightTri prev)

                        let I i _S' = viewI _S

                            (v0,v1,v2) = vertices _S'

                            this = i ./ ENTet {
                                            ent_top = v0
                                        ,   ent_bot = v1
                                        ,   ent_left = v2 
                                        ,   ent_right = otriangleDualVertex _S'

                                        }
                            
                        Just (this,this))

                  ient0




    
innerEdgeNeighborhood :: TEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood te = 
    let
        x0 : xs = abstractEdgeNeighborhood NoFlip te
    in
        case break (== x0) xs of

             (l,_:_) -> Just (x0:l)
             (_,[]) -> Nothing

        
prop_innerEdgeNeighborhood :: Triangulation -> Property
prop_innerEdgeNeighborhood (tr :: Triangulation) =
    forAllElements (edges tr)
        (\te -> case innerEdgeNeighborhood te of
                     Nothing -> property (isBoundaryEdge te)
                     Just xs -> length xs .=. ecSize te)

  where
    isBoundaryEdge = 
        any (isNothing . lookupGluingOfITriangle tr)
            . itrianglesContainingEdge


qc_AbstractNeighbordhood :: IO Bool
qc_AbstractNeighbordhood = $quickCheckAll

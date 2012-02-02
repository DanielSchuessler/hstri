{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module AbstractNeighborhood(
    EdgeNeighborhoodTet,
    IEdgeNeighborhoodTet,
    ent_top,ent_bot,ent_left,ent_right,ent_leftTri,ent_rightTri,
    innerEdgeNeighborhood',
    innerEdgeNeighborhood,
    ) where
import TriangulationCxtObject
import Data.Function
import Control.Arrow((&&&))
import PrettyUtil
import Data.List(unfoldr)
import Data.Maybe
import HomogenousTuples

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

instance Vertices EdgeNeighborhoodTet where
    type Verts EdgeNeighborhoodTet = Quadruple Vertex
    vertices x = (ent_bot x, ent_top x, ent_left x, ent_right x)

ent_leftTri :: IEdgeNeighborhoodTet -> OITriangle
ent_leftTri  = mapI (\ent -> otriangle (ent_top ent, ent_bot ent, ent_left  ent))
ent_rightTri :: IEdgeNeighborhoodTet -> OITriangle
ent_rightTri = mapI (\ent -> otriangle (ent_top ent, ent_bot ent, ent_right ent))

-- | Returns a stream of tetrahedra containing a preimage of the given edge, with each tetrahedron's
-- 'ent_rightTri' glued to the next tetrahedron's 'ent_leftTri'.
--
-- The result will be infinite iff the edge is an inner edge.
--
-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
edgeNeighborhoodTetStream :: Triangulation -> S2 -> OIEdge -> [IEdgeNeighborhoodTet]
edgeNeighborhoodTetStream tr dir ie = 
    let
        I i0 e = viewI ie        


        ient0 = i0 ./ ENTet {..}
            where
                (ent_bot,ent_top) = vertices e
                (ent_left,ent_right) = (vertices . oppositeEdge . forgetVertexOrder) e
                                        *. dir
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
                                        ,   ent_right = oTriangleDualVertex _S'

                                        }
                            
                        Just (this,this))

                  ient0




-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
innerEdgeNeighborhood' :: Triangulation -> OIEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood' tr e = 
    let
        x0 : xs = edgeNeighborhoodTetStream tr NoFlip e
    in
        case break (== x0) xs of

             (l,_:_) -> Just (x0:l)
             (_,[]) -> Nothing

    
{-# DEPRECATED innerEdgeNeighborhood "Use innerEdgeNeighborhood'" #-}
innerEdgeNeighborhood :: TEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood x = innerEdgeNeighborhood' (getTriangulation x) (packOrderedFace (unT x) Flip)

        

{-# LANGUAGE TupleSections, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module AbstractNeighborhood(
    EdgeNeighborhoodTet,
    IEdgeNeighborhoodTet,
    ent_top,ent_bot,ent_left,ent_right,ent_leftTri,ent_rightTri,ent_upperTri,ent_lowerTri,
    ent_mirrorLR,
    ient_top,ient_bot,ient_left,ient_right,ient_leftTri,ient_rightTri,ient_upperTri,ient_lowerTri,
    ient_mirrorLR,

    InnerEdgeNeighborhood,
    ien_toList, ien_toNonEmpty,
    BoundaryEdgeNeighbordhood,
    ben_toList, ben_toNonEmpty,
    innerEdgeNeighborhood',
    innerEdgeNeighborhood,
    edgeNeighborhood
    ) where
import TriangulationCxtObject
import Data.Function
import Control.Arrow((&&&))
import PrettyUtil
import Data.List(unfoldr)
import Data.Maybe
import HomogenousTuples
import Language.Haskell.TH
import Util
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Data.SumType

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

-- | Order: top vertex, bottom vertex, left vertex
ent_leftTri :: EdgeNeighborhoodTet -> OTriangle
ent_leftTri  ent = otriangle (ent_top ent, ent_bot ent, ent_left  ent)

-- | Order: top vertex, bottom vertex, right vertex
ent_rightTri :: EdgeNeighborhoodTet -> OTriangle
ent_rightTri ent = otriangle (ent_top ent, ent_bot ent, ent_right ent)

-- | Order: top vertex, left vertex, right vertex
ent_upperTri :: EdgeNeighborhoodTet -> OTriangle
ent_upperTri ent = otriangle (ent_top ent, ent_left ent, ent_right ent)

-- | Order: bottom vertex, left vertex, right vertex
ent_lowerTri :: EdgeNeighborhoodTet -> OTriangle
ent_lowerTri ent = otriangle (ent_bot ent, ent_left ent, ent_right ent)

ent_mirrorLR :: EdgeNeighborhoodTet -> EdgeNeighborhoodTet
ent_mirrorLR ent = ent { ent_left = ent_right ent, ent_right = ent_left ent } 

-- generate i-variants of accessors
$(concatMapM 
    (\(t,f) -> 
        let
            i_f = mkName ("i" ++ nameBase f)
            theSigD = sigD i_f [t| IEdgeNeighborhoodTet -> $(t) |]

            theValD =
                    valD 
                        (varP i_f)
                        (normalB [| mapI $(varE f) |])
                        []
        in
            sequence [ theSigD, theValD ]
    )

    
    (
        map ([t| IVertex |],)    ['ent_top,'ent_bot,'ent_left,'ent_right] ++
        map ([t| OITriangle |],) ['ent_leftTri,'ent_rightTri,'ent_lowerTri,'ent_upperTri] ++
        map ([t| IEdgeNeighborhoodTet |],) ['ent_mirrorLR] 
    ))



-- | Returns a stream of tetrahedra containing a preimage of the given edge, with each tetrahedron's
-- 'ient_rightTri' glued to the next tetrahedron's 'ient_leftTri'.            
--
-- The result will be infinite iff the edge is an inner edge.
--
-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
edgeNeighborhoodTetStream :: Triangulation -> S2 -> OIEdge -> NonEmpty IEdgeNeighborhoodTet
edgeNeighborhoodTetStream tr dir ie =
    let
        I i0 e = viewI ie        


        ient0 = i0 ./ ENTet {..}
            where
                (ent_bot,ent_top) = vertices e
                (ent_left,ent_right) = (vertices . oppositeEdge . forgetVertexOrder) e
                                        *. dir
    in 
        ient0 :|

        unfoldr (\prev -> do
                        _S <- lookupGluingOfOITriangle tr (ient_rightTri prev)

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



-- | 
-- INVARIANT: The 'ent_top's are all glued together, so are the 'ent_bot's. 
--
-- INVARIANT: Each tetrahedron's
-- 'ient_rightTri' is glued to the next tetrahedron's 'ient_leftTri', cyclically.                   
newtype InnerEdgeNeighborhood = UnsafeInnerEdgeNeighborhood {
        ien_toNonEmpty :: NonEmpty IEdgeNeighborhoodTet 
    }
    deriving Show

ien_toList :: InnerEdgeNeighborhood -> [IEdgeNeighborhoodTet]
ien_toList = NE.toList . ien_toNonEmpty

-- | 
-- INVARIANT: The 'ent_top's are all glued together, so are the 'ent_bot's. 
--
-- INVARIANT: Each tetrahedron's
-- 'ient_rightTri' is glued to the next tetrahedron's 'ient_leftTri'.                   
--
-- INVARIANT: The first tetrahedron's 'ient_leftTri' and the last tetrahedron's 'ient_rightTri' are boundary tris.
newtype BoundaryEdgeNeighbordhood = UnsafeBoundaryEdgeNeighborhood {
        ben_toNonEmpty :: NonEmpty IEdgeNeighborhoodTet 
    }
    deriving Show

ben_toList :: BoundaryEdgeNeighbordhood -> [IEdgeNeighborhoodTet]
ben_toList = NE.toList . ben_toNonEmpty

-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
innerEdgeNeighborhood' :: Triangulation -> OIEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood' tr e = fmap ien_toList $ sumTypeToMaybe (edgeNeighborhood tr e)

    
innerEdgeNeighborhood :: TEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood x = innerEdgeNeighborhood' (getTriangulation x) (packOrderedFace (unT x) Flip)



        
-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
edgeNeighborhood
  :: Triangulation
     -> OIEdge -> Either BoundaryEdgeNeighbordhood InnerEdgeNeighborhood
edgeNeighborhood tr e = 
    let
        x0xs@(x0 :| xs) = edgeNeighborhoodTetStream tr NoFlip e
    in
        case break (== x0) xs of

             (xs',_:_) -> Right (UnsafeInnerEdgeNeighborhood (x0 :| xs'))
             (_,[]) -> 
                Left 
                    (UnsafeBoundaryEdgeNeighborhood 
                     (NE.fromList (
                        (reverse . map ient_mirrorLR . tail . NE.toList 
                                . edgeNeighborhoodTetStream tr Flip) e 
                         ++
                         NE.toList x0xs)))
                         
                        




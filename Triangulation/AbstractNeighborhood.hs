{-# LANGUAGE ViewPatterns, TupleSections, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Triangulation.AbstractNeighborhood(
    EdgeNeighborhoodTet,
    IEdgeNeighborhoodTet,
    ent_top,ent_bot,ent_left,ent_right,ent_leftTri,ent_rightTri,ent_upperTri,ent_lowerTri,
    ent_mirrorLR,ent_centralEdge,
    ient_top,ient_bot,ient_left,ient_right,ient_leftTri,ient_rightTri,ient_upperTri,ient_lowerTri,
    ient_mirrorLR,ient_centralEdge,

    InnerEdgeNeighborhood,
    ien_toList, ien_toNonEmpty, ien_length,
    BoundaryEdgeNeighborhood,
    ben_toList, ben_toNonEmpty, ben_length,
    innerEdgeNeighborhood',
    innerEdgeNeighborhood,
    edgeNeighborhood,
    someEdgeNeighborhood,

    -- * Testing
    unsafeEdgeNeighborhoodTetExportedOnlyForTesting,
    edgeNeighborhoodTetStream


    ) where
import TriangulationCxtObject
import PrettyUtil
import Data.List(unfoldr)
import HomogenousTuples
import Language.Haskell.TH
import Util
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Data.SumType
import Data.Bits
import Control.Applicative

newtype EdgeNeighborhoodTet = 
    ENTet Word8 
    -- msb to lsb: top,bot,left,right

        deriving(Eq,Ord)

enTet
  :: Vertex -> Vertex -> Vertex -> Vertex -> EdgeNeighborhoodTet
enTet t b l r = ENTet (     shiftL (vertexToWord8 t) 6 .|. shiftL (vertexToWord8 b) 4 
                        .|. shiftL (vertexToWord8 l) 2 .|. (vertexToWord8 r))

ent_top :: EdgeNeighborhoodTet -> Vertex
ent_top (ENTet w)   = vertexFromWord8 (shiftR w 6) 
ent_bot :: EdgeNeighborhoodTet -> Vertex
ent_bot (ENTet w)   = vertexFromWord8 (shiftR w 4 .&. 3) 
ent_left :: EdgeNeighborhoodTet -> Vertex
ent_left (ENTet w)  = vertexFromWord8 (shiftR w 2 .&. 3) 
ent_right :: EdgeNeighborhoodTet -> Vertex
ent_right (ENTet w) = vertexFromWord8 (       w   .&. 3) 


data EdgeNeighborhoodTetView = ENTetView {
        entv_top, entv_bot, entv_left, entv_right :: Vertex
    }
    deriving Show

viewENTet :: EdgeNeighborhoodTet -> EdgeNeighborhoodTetView
viewENTet = ENTetView <$> ent_top <*> ent_bot <*> ent_left <*> ent_right
unviewENTet :: EdgeNeighborhoodTetView -> EdgeNeighborhoodTet
unviewENTet (ENTetView t b l r) = enTet t b l r


instance Show EdgeNeighborhoodTet where
    showsPrec _ (viewENTet -> ENTetView a b c d) = 
        shows (a,b,c,d)


trivialHasTIndexInstance [t| EdgeNeighborhoodTet |]                
type IEdgeNeighborhoodTet = I EdgeNeighborhoodTet

-- instance Eq EdgeNeighborhoodTet where
--     (==) = (==) `on` (ent_top &&& ent_bot &&& ent_left) 

instance Pretty EdgeNeighborhoodTet where
    prettyPrec prec (viewENTet -> ENTetView a b c d) = prettyPrecApp prec (text "ENTet") [a,b,c,d] 

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
ent_mirrorLR (viewENTet -> ent) = 
    unviewENTet (ent { entv_left = entv_right ent, entv_right = entv_left ent })

ent_centralEdge :: EdgeNeighborhoodTet -> OEdge
ent_centralEdge ent = oedge (ent_bot ent, ent_top ent)

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
        map ([t| IEdgeNeighborhoodTet |],) ['ent_mirrorLR] ++
        map ([t| OIEdge |],)     ['ent_centralEdge]
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


        ient0 = i0 ./ enTet _top _bot _left _right
            where
                (_bot,_top) = vertices e
                (_left,_right) = (vertices . oppositeEdge . forgetVertexOrder) e
                                        *. dir
    in 
        ient0 :|

        unfoldr (\prev -> do
                        _S <- lookupGluingOfOITriangle tr (ient_rightTri prev)

                        let I i _S' = viewI _S

                            (v0,v1,v2) = vertices _S'

                            this = i ./ enTet v0 v1 v2 (oTriangleDualVertex _S')

                            
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
newtype BoundaryEdgeNeighborhood = UnsafeBoundaryEdgeNeighborhood {
        ben_toNonEmpty :: NonEmpty IEdgeNeighborhoodTet 
    }
    deriving Show

ben_toList :: BoundaryEdgeNeighborhood -> [IEdgeNeighborhoodTet]
ben_toList = NE.toList . ben_toNonEmpty

-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
innerEdgeNeighborhood' :: Triangulation -> OIEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood' tr e = fmap ien_toList $ sumTypeToMaybe (edgeNeighborhood tr e)

    
innerEdgeNeighborhood :: TEdge -> Maybe [IEdgeNeighborhoodTet]
innerEdgeNeighborhood x = innerEdgeNeighborhood' (getTriangulation x) (packOrderedFace (unT x) Flip)



        
-- The 'ent_bot' of each result tet will be equivalent to the first vertex of the given 'OIEdge'; the 'ent_top of each result tet will be equivalent to the second vertex of the given 'OIEdge'. 
edgeNeighborhood
  :: Triangulation
     -> OIEdge -> Either BoundaryEdgeNeighborhood InnerEdgeNeighborhood
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
                         
                        

-- | Uses arbitrary orders
someEdgeNeighborhood
  :: 
        TEdge
     -> Either BoundaryEdgeNeighborhood InnerEdgeNeighborhood
someEdgeNeighborhood e = edgeNeighborhood (getTriangulation e) . toOrderedFace . unT $ e

ben_length :: BoundaryEdgeNeighborhood -> Int
ben_length = length . ben_toList
ien_length :: InnerEdgeNeighborhood -> Int
ien_length = length . ien_toList


unsafeEdgeNeighborhoodTetExportedOnlyForTesting
  :: Vertex -> Vertex -> Vertex -> Vertex -> EdgeNeighborhoodTet
unsafeEdgeNeighborhoodTetExportedOnlyForTesting = enTet


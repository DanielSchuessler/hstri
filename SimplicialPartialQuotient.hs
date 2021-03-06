{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, TemplateHaskell, GADTs, NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module SimplicialPartialQuotient where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Data.AscTuples
import Data.Lens.Common
import Data.Lens.Template
import Data.Map(Map)
import Data.Maybe
import Debug.Trace
import FileLocation
import HomogenousTuples
import PreRenderable
import PrettyUtil
import ShortShow
import Simplicial.SimplicialComplex
import Test.QuickCheck
import Tetrahedron.INormalDisc
import Tetrahedron.NormalDisc
import Triangulation
import TriangulationCxtObject
import Util
import qualified Data.List as L
import qualified Data.Map as M
import Numeric.AD.Vector
import Triangulation.CanonOrdered
import Data.Ord(comparing)
import Text.Groom
import Tetrahedron.Vertex
import Control.Monad

-- | A simplicial map from the disjoint union of tetrahedra of a 'Triangulation' to some simplicial complex, identifying as most as many things as the gluings of the 'Triangulation'.
data SimplicialPartialQuotient v = SimplicialPartialQuotient {

    spq_tr :: Triangulation,
    -- | Need only be defined on the 'tIVertices' of the 'spq_tr'.
    spq_map :: IVertex -> v,
    spq_tets :: [Asc4 v]

}

nameMakeLens ''SimplicialPartialQuotient (Just . (++"L"))

instance NFData v => NFData (SimplicialPartialQuotient v) where
    rnf SimplicialPartialQuotient{..} =

        rnf spq_tr `seq`
        rnf (map unAsc4 spq_tets) `seq`
        ()


spq_verts :: Ord v => SimplicialPartialQuotient v -> [v]
spq_verts = nub' . concatMap asList . spq_tets




spq_fromMap
  :: Triangulation
     -> Map IVertex v -> [Asc4 v] -> SimplicialPartialQuotient v
spq_fromMap t m tets = SimplicialPartialQuotient t (flip $(indx) m) tets


spq_mapEd
  :: (Vertices a, Verts a ~ Pair IVertex) =>
     SimplicialPartialQuotient b -> a -> Pair b
spq_mapEd (spq_map -> m) e = map2 m (vertices e)

spq_mapTri
  :: (Vertices a, Verts a ~ Triple IVertex) =>
     SimplicialPartialQuotient b -> a -> Triple b
spq_mapTri (spq_map -> m) tri = map3 m (vertices tri)

instance Ord v => DeltaSetMorphism2 ITriangle (Asc3 v) (SimplicialPartialQuotient v) where
    mapVert = spq_map 
    mapEd spq = $unEitherC "mapEd: mapSimpEdTotal failed" . mapSimpEdTotal (spq_map spq)
    mapTri spq = $unEitherC "mapEd: mapSimpTriTotal failed" . mapSimpTriTotal (spq_map spq)


type GluingLabeller = NormalizedGluing -> String

makeTriangleLabelling
  :: (Show v, Pretty v, Ord v) =>
     SimplicialPartialQuotient v
     -> GluingLabeller -> Asc3 v -> Maybe TriangleLabel
makeTriangleLabelling spq gluingLabeller = triangleLabelsForSimplicial stlas
    where
        stlas = do
            ng <- extraGluings spq

            let lbl = gluingLabeller ng

                tri  = ngDom ng
                otri = ngCod ng

                (l ,r ,t ) = spq_mapTri spq tri
                (l',r',t') = spq_mapTri spq otri

                _scale = 1/L.genericLength lbl 

            [(sTriangleLabelAssoc lbl l  r  t)  { stla_scale = _scale } ,
             (sTriangleLabelAssoc lbl l' r' t') { stla_scale = _scale }]


defaultGluingLabeller :: GluingLabeller
defaultGluingLabeller = 
    (++) <$> (show . getTIndex . ngDom) <*> (show . getTIndex . ngCod)


--             -- find the permutation that makes 'tri' resp. 'otri' ordered by brute force
--             g <- allS3
-- 
--             let theLabel = TriangleLabel lbl (inv g) 0.1 
--                 g_im_tri = g .* (spq_mapTri spq tri)
--                 g_im_otri = g .* (spq_mapTri spq otri)
-- 
--             (++)
--                 (guard (isOrdered3 g_im_tri)  >> [(g_im_tri ,theLabel)]) 
--                 (guard (isOrdered3 g_im_otri) >> [(g_im_otri,theLabel)]) 


-- | Gluings of the original triangulation which are *not* implemented by 'spq_map'
extraGluings
  :: Eq v => SimplicialPartialQuotient v -> [NormalizedGluing]
extraGluings spq = 
            L.filter 
                (not . isGluingImplemented spq . ngToGluing)
                (tNormalizedGluings (spq_tr spq))  

implementedGluings
  :: Eq v => SimplicialPartialQuotient v -> [NormalizedGluing]
implementedGluings spq = 
            L.filter 
                (isGluingImplemented spq . ngToGluing)
                (tNormalizedGluings (spq_tr spq))  

isGluingImplemented
  :: (Eq v) => SimplicialPartialQuotient v -> Gluing -> Bool
isGluingImplemented spq (tri,otri) = spq_mapTri spq tri == spq_mapTri spq otri 



notTooManyGluings
  :: Eq b => SimplicialPartialQuotient b -> Property
notTooManyGluings m =
    let
        oitris = tOITriangles (spq_tr m)
    in
        forAll (elements oitris)
        (\otri1 ->
            let
                im_otri1 = spq_mapTri m otri1

                mglueds = L.filter (\otri2 -> otri2 /= otri1 && spq_mapTri m otri2 == im_otri1) oitris
            in
                if L.null mglueds
                then property True
                else
                    forAll (elements mglueds)

                        (\otri2 ->
                            lookupGluingOfOITriangle (spq_tr m) otri1 == Just otri2))

-- prop_enoughGluings
--   :: (Ord b, Pretty b, Show b) => SimplicialPartialQuotient b -> Property
-- prop_enoughGluings spq =
--     let
--         _triangleLabel = makeTriangleLabelling spq defaultGluingLabeller
--     in
--         forAll (elements . tGluingsIrredundant . spq_tr $ spq)
--             (\(tri,otri) ->
-- 
--                 let
--                     im1 = spq_mapTri spq tri 
--                     im2 = spq_mapTri spq otri
--                     
--                     lbl1 = _triangleLabel im1
--                     lbl2 = _triangleLabel im2
--                 in
-- 
--                     label "equal images" (im1 == im2)
--                     .||.
--                     (isJust lbl1 && lbl1 == lbl2)
--                     
-- 
-- 
-- 
--             )


toSimplicialComplex
  :: (Ord v, Show v) => SimplicialPartialQuotient v -> SC3 v
toSimplicialComplex = fromOrderedTets . spq_tets


addEdgeDecos
  :: Ord (Element (Eds s)) =>
     Triangulation
     -> (OIEdge -> (Ed s,S2)) -> PreRenderable s -> PreRenderable s
addEdgeDecos (tr :: Triangulation) _mapEd = setL pr_edgeDecoL _edgeDeco
    where
        _edgeDeco = flip M.lookup (M.fromList edgeDecoAssocs)

        -- Let high-degree edges get the simpler decos to clutter the picture less
        edges' = L.sortBy (flip (comparing ecSize))
                     .  filter (\e -> ecSize e > 1) 
                     $   (edges tr) 

        edgeDecoAssocs = do
            (e,i) <- edges' `zip` [1..]
            preimage <- preimageList e
            let (ed,dir) = _mapEd (unCanonOrdered preimage)
            return (ed, EdgeDeco i dir)

            

spq_mapOEd :: (Ord v, Show v) => SimplicialPartialQuotient v -> OIEdge -> (Asc2 v, S2)
spq_mapOEd spq ed = 
    $unEitherC ("spq_mapOEd _ " ++ show ed ++": sort2WithPermutation' failed") 
    . sort2WithPermutation' 
    . spq_mapEd spq $ ed

data TriangPreRenderableOpts = TPRO {
    decorateEdges :: Bool
}

defaultTPRO :: TriangPreRenderableOpts
defaultTPRO = TPRO False

toPreRenderable
  :: (Ord v, Show v, Pretty v, ShortShow v) =>
     SPQWithCoords v -> PreRenderable (SC3 v)
toPreRenderable = toPreRenderableWithOpts defaultTPRO

toPreRenderableWithOpts
  :: (Ord v, ShortShow v, Pretty v, Show v) =>
     TriangPreRenderableOpts -> SPQWithCoords v -> PreRenderable (SC3 v)
toPreRenderableWithOpts opts (SPQWithCoords spq coords gluingLabeller) = 

        (if decorateEdges opts then addEdgeDecos tr (spq_mapOEd spq) else id) 
            
            resultWithoutEdgeDecos


    where
        resultWithoutEdgeDecos = 
            mkPreRenderableWithTriangleLabels 
                (makeTriangleLabelling spq gluingLabeller)
                coords
                (toSimplicialComplex spq)

        tr = spq_tr spq


    
spq_Equivalence_helper
  :: (Eq v, Ord a, GluingMappable a) =>
     (ITriangle -> [a])
     -> (Triangulation -> [a])
     -> SimplicialPartialQuotient v
     -> Equivalence a
spq_Equivalence_helper thingsOfTri allThings spq = 
    mkEquivalence 
        (do
            gl <- implementedGluings spq
            na <- thingsOfTri (ngDom gl)
            [(na,ngMap gl na)])
        (allThings (spq_tr spq))



spq_INormalArcEquivalence
  :: Eq v => SimplicialPartialQuotient v -> Equivalence INormalArc
spq_INormalArcEquivalence = spq_Equivalence_helper normalArcList tINormalArcs

spq_INormalCornerEquivalence
  :: Eq v => SimplicialPartialQuotient v -> Equivalence INormalCorner
spq_INormalCornerEquivalence = spq_Equivalence_helper normalCornerList tINormalCorners



-- | Doesn't identify anything
spq_identity :: Triangulation -> SimplicialPartialQuotient IVertex
spq_identity tr =

            SimplicialPartialQuotient
                tr
                id
                (fmap sTetVerts (tTetrahedra_ tr))

spq_toOtherTriangulation
  :: Triangulation -> Triangulation -> SimplicialPartialQuotient TVertex
spq_toOtherTriangulation tr0 tr1 = 
    SimplicialPartialQuotient
        tr0
        (pMap tr1)
        (map (\t ->
            let r = map4 (pMap tr1) (vertices t) 
            in $unEitherC ("spq_toOtherTriangulation: Target triangulation tr1 is not simplicial: Tet "
                            ++show t++" has vertices "++show r++". tr1 is:\n"++show tr1)
                (asc4total r))
             (tTetrahedra_ $ tr1)) 




-- | A 'SimplicialPartialQuotient' whose vertices have coordinates in @R^3@, and which knows how to label its triangles corresponding to gluings not implemented by the partial quotient
data SPQWithCoords v = SPQWithCoords {
    spqwc_spq :: SimplicialPartialQuotient v,
    spqwc_coords :: v -> Vec3,
    spqwc_gluingLabeller :: GluingLabeller
}

nameMakeLens ''SPQWithCoords (Just . (++"L"))


instance NFData v => NFData (SPQWithCoords v) where
    rnf SPQWithCoords{..} =

        rnf spqwc_spq `seq`
        ()


oneTetWithDefaultCoords
  :: Triangulation -> GluingLabeller -> SPQWithCoords Vertex
oneTetWithDefaultCoords tr gluingLabeller = 
    assert (tNumberOfTetrahedra tr == (1::Integer))
    $
        SPQWithCoords 
            (SimplicialPartialQuotient tr forgetTIndex [allVerticesAsc])
            vertexDefaultCoords
            gluingLabeller


spq_twoTetBipyramid
  :: Triangulation -> ITriangle -> SimplicialPartialQuotient TVertex
spq_twoTetBipyramid tr theTri = 

        if getTIndex theTri == getTIndex theGluedTri
           then $err' ("spq_twoTetBipyramid: Given triangle is glued to a triangle in the same tetrahedron: "
                        ++show(theTri,theGluedTri))
           else spq_toOtherTriangulation tr tr'


    where
        theGluedTri = fromMaybe (error ("spq_twoTetBipyramid: "++
                                        "second arg must be an inner triangle"))                    
                                        
                                (lookupGluingOfITriangle tr theTri)

        tr' = twoTetBipyramid theTri theGluedTri 



twoTetBipyramid
  :: ITriangle -> OITriangle -> Triangulation
twoTetBipyramid theTri theGluedTri =
        mkTriangulation 2 [(theTri,theGluedTri)]

-- | Creates a partial quotient for the given 2-tetrahedron triangulation which implements the gluing of the given triangle (and no others) 
spqwc_twoTetBipyramid
  :: Triangulation
     -> ITriangle -> GluingLabeller -> SPQWithCoords TVertex
spqwc_twoTetBipyramid = (fmap . fmap) fst . spqwc_twoTetBipyramid'

spqwc_twoTetBipyramid'
  :: Triangulation
     -> ITriangle
     -> GluingLabeller
     -> (SPQWithCoords TVertex, Triangulation)
spqwc_twoTetBipyramid' tr theTri gluingLabeller = 
    assert (tNumberOfTetrahedra tr == (2::Integer)) $

        (
        SPQWithCoords {
            spqwc_spq = spq_twoTetBipyramid tr theTri,
            spqwc_coords = (flip $(indxShow) m),
            spqwc_gluingLabeller = gluingLabeller
        },
        mkTriangulation 2 [(theTri,theGluedTri)]
        )


    where

        cA,cB,cC,cD :: Vec3
        (cA,cB,cC,cD) = map4 vertexDefaultCoords (vA,vB,vC,vD)
        cD' = (2/3) *& (cA &+ cB &+ cC) &- cD

        theGluedTri = fromMaybe (error ("spqwc_twoTetBipyramid: "++
                                        "second arg must be an inner triangle"))                    
                                        
                                (lookupGluingOfITriangle tr theTri)


        p = pMap (twoTetBipyramid theTri theGluedTri) 


        (u0,u1,u2) = vertices theTri 
        u3 = iTriangleDualVertex theTri
        --(v0,v1,v2) = vertices theGluedTri
        v3 = oiTriangleDualVertex theGluedTri


        m = M.fromList
            [ (p u0, cA) 
            , (p u1, cB) 
            , (p u2, cC) 
            , (p u3, cD) 
            , (p v3, cD')
            ]            

instance Pretty v => Show (SimplicialPartialQuotient v) where
    showsPrec = prettyShowsPrec

instance Pretty v => Pretty (SimplicialPartialQuotient v) where
    pretty SimplicialPartialQuotient{..} = 
        prettyRecord "SimplicialPartialQuotient"
            [("spq_tr",pretty spq_tr),
             ("spq_map",prettyFunction spq_map (tIVertices spq_tr)),
             ("spq_tets",pretty spq_tets) 
            ]

instance (Ord v, Pretty v) => Show (SPQWithCoords v) where
    showsPrec = prettyShowsPrec

instance (Ord v, Pretty v) => Pretty (SPQWithCoords v) where
    pretty SPQWithCoords{..} =
        prettyRecord "SPQWithCoords"
            [("spqwc_spq",pretty spqwc_spq),
             ("spqwc_coords",prettyFunction spqwc_coords (spq_verts spqwc_spq)),
             ("spqwc_gluingLabeller",prettyFunction 
                                        (spqwc_gluingLabeller)
                                        (extraGluings spqwc_spq))
             
             ]


spqwc_tr :: SPQWithCoords v -> Triangulation
spqwc_tr = spq_tr . spqwc_spq

spqwc_map :: SPQWithCoords v -> IVertex -> v
spqwc_map = spq_map . spqwc_spq

spqwc_coords' :: SPQWithCoords v -> IVertex -> Vec3
spqwc_coords' spqwc = spqwc_coords spqwc . spqwc_map spqwc

instance Coords (SPQWithCoords v) where
    transformCoords f = modL spqwc_coordsL ((tup3toVec3 . lowerFF f . vec3toTup3) .)









-- | Identifies all the vertices that are identified in the triangulation.
spq_fullQuotient
  :: Triangulation -> SimplicialPartialQuotient TVertex
spq_fullQuotient = join spq_toOtherTriangulation 


-- | Identifies all the vertices that are identified in the triangulation.
spqwc_fullQuotient
  :: Triangulation -> (TVertex -> Vec3) -> SPQWithCoords TVertex
spqwc_fullQuotient tr coords =
    SPQWithCoords 
        (spq_fullQuotient tr) 
        coords
        (\_ -> $err' ("The result of spqwc_fullQuotient does not have a GluingLabeller because there aren't supposed to be any gluings leftover to be labelled. Maybe you got here through record updates?"))


spq_to_tr :: Eq b => SimplicialPartialQuotient b -> Triangulation
spq_to_tr spq = mkTriangulation n

        -- O(n^2) algo, OPTIMIZEME
    [ (tri,otri) |  tri <- liftM2 (./) tets allTriangles
                 , otri <- liftM2 (./) tets allOTriangles
                 , spq_mapTri spq tri == spq_mapTri spq otri 
                 ]

    where
        tets = [0..fi n-1]
        n = tNumberOfTetrahedra_ (spq_tr spq) 







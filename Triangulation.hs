{-# LANGUAGE DeriveDataTypeable, TupleSections, PatternGuards, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module Triangulation(
    module Tetrahedron,
    module Equivalence,
    module Triangulation.FacetGluing,
    Triangulation,
    
    -- * Properties 
    tNumberOfTetrahedra_,tNumberOfTetrahedra,tTetrahedra_,tOriginalGluings, tGlueMap_, triangTetCount,
    lookupGluingOfITriangle,
    lookupGluingOfOITriangle,
    tIVertices,
    tIEdges,
    tITriangles,
    tOITriangles,
    tOIEdgeEquivalents,
    tOIEdgeDegree,
    tGluingsIrredundant,
    tNormalizedGluings,
    tINormalCorners,
    tINormalTris,
    tINormalArcs,
    tINormalQuads,
    tINormalDiscs,
    tOIEdges,
    getOIEdgeGluingSense,
    gluedNormalArc,
    boundaryITriangles,
    tNumberOfNormalDiscTypes,
    tNumberOfNormalQuadTypes,
    adjacentNormalTris,
    isCanonicallyOrderedClass,
    tINormalDiscNu,
    tINormalArcNu,
    lookupGluingAsGluing,
    tetGetGluings,
    dfsFacePairingGraph,
    isClosedTriangulation,
    -- ** Equivalences
    oEdgeEqv,vertexEqv,
    iEdgeEqv,
    -- * Construction 
    mkTriangulation,
    mkTriangulationSafe,mkTriangulationG,triang,
    TriangulationConstructionError(..),
    describeTriangulationConstructionError,
    -- * Transformation
    addGluings,
    -- * Canonical representatives for things that are glued
    TriangulationDSnakeItem(..),
    -- * Examples
    tr_l31,
    -- * Labelled
    labelledTriangulation,LabelledTriangulation,



    
    ) where

import Control.Applicative
import Control.Arrow((&&&))
import Control.DeepSeq.TH
import Control.Exception
import Control.Monad
import Data.EdgeLabelledTree
import Data.Function
import Data.List(sort)
import Data.Map(Map,lookup,(!))
import Data.Maybe
import Data.Numbering
import Data.Set(member)
import Data.SumType
import Data.Typeable(Typeable)
import Equivalence
import HomogenousTuples
import INormalDisc
import Language.Haskell.TH.Lift
import Prelude hiding(catch,lookup)
import Quote
import Tetrahedron
import Tetrahedron.NormalDisc
import Triangulation.FacetGluing
import Util
import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Set as S


tGluingsIrredundant :: Triangulation -> [Gluing]
tGluingsIrredundant tr = 
    let
        gluings = tOriginalGluings tr
        gluingsFirstTris = S.fromList (fst <$> gluings) 

        isRedundant (tri,unpackOrderedFace -> (tri',_)) =
            (tri > tri') && member tri' gluingsFirstTris 
    in
        filter (not . isRedundant) gluings


tNormalizedGluings :: Triangulation -> [NormalizedGluing]
tNormalizedGluings = fmap normalizeGluing . tGluingsIrredundant

tTetrahedra_ :: Triangulation -> [TIndex]
tTetrahedra_ tr = if n==0 then [] else [0..tindex (n - 1)]
    where
        n = tNumberOfTetrahedra tr

-- Note: All fields except the first two ('triangTets_', 'triangGluings_') are semantically redundant, 
-- but kept in this data structure for memoization purposes
data Triangulation = Triangulation { 
    tNumberOfTetrahedra_ :: Word,

    -- | The original gluings as passed to 'mkTriangulation'
    tOriginalGluings :: [Gluing],

    -- | INVARIANT: 
    --
    -- @lookup (x ./ y) tGlueMap_ = Just (x' ./ y')@ 
    --
    -- implies 
    --
    -- @lookup (x' ./ 'forgetVertexOrder' y') tGlueMap_ = Just (x ./ 'packOrderedFace' x ('inv' g'))@
    --
    -- (for all @f0@, @f1@, @g@)
    tGlueMap_ :: Map ITriangle OITriangle,


    oEdgeEqv :: Equivalence OIEdge, 
    vertexEqv :: Equivalence IVertex
--    normalArcEqv :: Equivalence INormalArc
}

tNumberOfTetrahedra :: Num c => Triangulation -> c
tNumberOfTetrahedra = fi . tNumberOfTetrahedra_




mkTriangulation :: Word -> [Gluing] -> Triangulation
mkTriangulation = (fmap . fmap) fromRight mkTriangulationSafe 

data TriangulationConstructionError =
        EdgeGluedToSelfInReverse IEdge
    |   TriangleGluedToSelf OITriangle   
    |   ThreeTrianglesGlued ITriangle OITriangle OITriangle

    deriving (Show,Typeable,Eq)

instance Exception TriangulationConstructionError

describeTriangulationConstructionError
  :: TriangulationConstructionError -> String
describeTriangulationConstructionError er =
    case er of
         EdgeGluedToSelfInReverse e ->
               "Edge "++show e++" is glued to itself in reverse"

         TriangleGluedToSelf f1 ->
                                "Triangle "++show (forgetVertexOrder f1)++
                                    " is glued to itself (in order "++show f1++")"

         ThreeTrianglesGlued t f1 f2 ->
                                "Three triangles glued together: "++show (t,f1,f2)
        



mkTriangulationSafe :: Word -> [Gluing] 
    -> Either TriangulationConstructionError Triangulation
mkTriangulationSafe tNumberOfTetrahedra_ tOriginalGluings
        = 
            

            (do

    let tets = 
            if tNumberOfTetrahedra_ > 0
            then [0.. tindex (pred tNumberOfTetrahedra_)] 
            else []

    
        allIEdges :: [IEdge]
        allIEdges = 
            {-# SCC "mkTriangulationSafe/allIEdges" #-}
            concatMap edgeList tets

        _allOIEdges = 
            {-# SCC "mkTriangulationSafe/allOIEdges" #-}
            concatMap (asList . allOIEdges) tets

        allIVertices :: [IVertex]
        allIVertices = 
            {-# SCC "mkTriangulationSafe/allIVertices" #-}
            concatMap vertexList allIEdges

    let addGluing mrec (t,f1) = mrec >>= (\r -> case lookup t r of
                                                        Nothing -> return (M.insert t f1 r)
                                                        Just f2 
                                                            | f2==f1 -> return r
                                                            | otherwise -> _err t f1 f2)

        _err t f1 f2 = Left 
                        (if t == forgetVertexOrder f1
                            then TriangleGluedToSelf f1
                            else ThreeTrianglesGlued t f1 f2)
        
    tGlueMap_ <- 
                    List.foldl' addGluing (return mempty) 
                        (tOriginalGluings ++ fmap flipGluing tOriginalGluings)


    let vertexEqv :: Equivalence IVertex 
        vertexEqv =  {-# SCC "mkTriangulationSafe/vertexEqv" #-}
                    mkEquivalence
                        (inducedVertexEquivalences =<< tOriginalGluings)
                        allIVertices

        oEdgeEqv = 
                    {-# SCC "mkTriangulationSafe/oEdgeEqv" #-}
                    mkEquivalence
                    (inducedOEdgeEquivalences =<< tOriginalGluings)
                    _allOIEdges



    {-# SCC "mkTriangulationSafe/call:checkForEdgeGluedToSelfInReverse" #-}
     checkForEdgeGluedToSelfInReverse allIEdges oEdgeEqv
                            
    {-# SCC "mkTriangulationSafe/return" #-}
     return Triangulation{ tNumberOfTetrahedra_, tGlueMap_, oEdgeEqv, vertexEqv, tOriginalGluings })


checkForEdgeGluedToSelfInReverse :: [IEdge] -> Equivalence OIEdge 
    -> Either TriangulationConstructionError ()
checkForEdgeGluedToSelfInReverse allIEdges oEdgeEqv = 

        



        mapM_ go allIEdges
    where
        go e = 
            {-# SCC "checkForEdgeGluedToSelfInReverse/go" #-}
            if eqv_classOf oEdgeEqv (toOrderedFace e) == eqv_classOf oEdgeEqv (packOrderedFace e Flip)
               then Left (EdgeGluedToSelfInReverse e)
               else Right ()
    





-- | Convenience function for the special case that there are no isolated tetrahedra
triang ::  [Gluing] -> Triangulation
triang gluings = mkTriangulation n gluings
    where
        n | null gluings = 0
          | otherwise = maximum (concatMap (\(t,o) -> 
                                    [ fi $ getTIndex t, fi $ getTIndex o ]) gluings) + 1



isBoundaryITriangle ::  Triangulation -> ITriangle -> Bool
isBoundaryITriangle t x = not (x `M.member` tGlueMap_ t)



triangTetCount :: Triangulation -> Int
triangTetCount = length `liftM` tTetrahedra_








-- | Allows an arbitrary tetrahedron index set
mkTriangulationG
  :: (Ord tet, Show tet) =>
     [tet] -> [((tet, Triangle), (tet, OTriangle))] 
        -> Either TriangulationConstructionError Triangulation
mkTriangulationG tets gluings =
    let
        tetIxs = [tindex 0..]
        tetIxMap = M.fromListWithKey 
                    (\k _ _ -> error ("Duplicate tetrahedron: "++show k)) 
                    (zip tets tetIxs)

        translateGluing ((tet,tri),(tet',otri)) = 
            ((tetIxMap ! tet) ./ tri, (tetIxMap ! tet') ./ otri)

    in
        mkTriangulationSafe (fi $ length tets) (translateGluing <$> gluings) 





instance Quote Triangulation where
    quotePrec prec t = quoteParen (prec >= 11) ("mkTriangulation "
                                                    ++ quotePrec 11 (tNumberOfTetrahedra_ t)
                                                    ++ " "
                                                    ++ quotePrec 11 (tOriginalGluings t))

instance Lift Triangulation where
    lift t = [| mkTriangulation $(lift $ tNumberOfTetrahedra_ t) $(lift $ tOriginalGluings t) |]



lookupGluingOfITriangle :: Triangulation -> ITriangle -> Maybe OITriangle
lookupGluingOfITriangle t tri =
    lookup tri (tGlueMap_ t)

lookupGluingOfOITriangle :: Triangulation -> OITriangle -> Maybe OITriangle
lookupGluingOfOITriangle t (unpackOrderedFace -> (tri,g)) =
    (*. g) <$> lookupGluingOfITriangle t tri




tOIEdgeEquivalents :: Triangulation -> OIEdge -> [OIEdge]
tOIEdgeEquivalents tr oiedge = eqv_equivalents (oEdgeEqv tr) oiedge

tOIEdgeDegree :: Triangulation -> OIEdge -> Int
tOIEdgeDegree tr = length . tOIEdgeEquivalents tr


tITriangles :: Triangulation -> [ITriangle]
tITriangles = concatMap triangleList . tTetrahedra_

tOITriangles :: Triangulation -> [OITriangle]
tOITriangles tr = liftM2 packOrderedFace (tITriangles tr) allS3

tIVertices :: Triangulation -> [IVertex]
tIVertices = concatMap vertexList . tTetrahedra_ 

tIEdges :: Triangulation -> [IEdge]
tIEdges = concatMap edgeList . tTetrahedra_ 

tINormalCorners :: Triangulation -> [INormalCorner]
tINormalCorners = concatMap normalCornerList . tTetrahedra_ 
tINormalArcs :: Triangulation -> [INormalArc]
tINormalArcs = concatMap normalArcList . tTetrahedra_ 
tINormalTris :: Triangulation -> [INormalTri]
tINormalTris = concatMap normalTriList . tTetrahedra_ 
tINormalQuads :: Triangulation -> [INormalQuad]
tINormalQuads = concatMap normalQuadList . tTetrahedra_ 


tINormalDiscs :: Triangulation -> [INormalDisc]
tINormalDiscs = concatMap normalDiscList . tTetrahedra_ 


tOIEdges :: Triangulation -> [OIEdge]
tOIEdges = concatMap (asList . allOIEdges) . tTetrahedra_

-- | Returns whether the two given oriented edges are glued to each other in order, reversely, or not at all
getOIEdgeGluingSense
  :: Triangulation -> OIEdge -> OIEdge -> Maybe S2
getOIEdgeGluingSense t oedge1 oedge2 = 
    let
        eq = eqv_eq (oEdgeEqv t)
    in
        if eq oedge1 oedge2
        then Just NoFlip
        else if eq (oedge1 *. Flip) oedge2 
        then Just Flip
        else Nothing



class TriangulationDSnakeItem a where
    canonicalize :: Triangulation -> a -> a

instance TriangulationDSnakeItem IVertex where
    canonicalize t x = eqvRep (vertexEqv t) x

instance TriangulationDSnakeItem IEdge where
    canonicalize t = forgetVertexOrder . canonicalize t . toOrderedFace

iEdgeEqv :: Triangulation -> EnumEqvImpl (EqvClassImpl IEdge)
iEdgeEqv tr = 
    EnumEqvImpl 
        (EqvImpl
            (\e -> disorderEquivalenceClass (eqvClassOf (oEdgeEqv tr) (toOrderedFace e))))

        (fmap disorderEquivalenceClass . filter isCanonicallyOrderedClass
            . eqvClasses . oEdgeEqv $ tr) 


instance TriangulationDSnakeItem OIEdge where
    canonicalize t x = (eqvRep (oEdgeEqv t) x)

instance TriangulationDSnakeItem ITriangle where
    canonicalize t rep = 
        case lookup rep (tGlueMap_ t) of
                                Just (forgetVertexOrder -> rep') | rep' < rep -> rep'
                                _ -> rep



gluedNormalArc :: Triangulation -> INormalArc -> Maybe INormalArc
gluedNormalArc tr ina = 
        let
            tri = iNormalArcGetTriangle ina
        in
            case lookup tri (tGlueMap_ tr) of
                                Just otri -> Just (gluingMap (tri,otri) ina)
                                _ -> Nothing

instance TriangulationDSnakeItem INormalArc where
    canonicalize t ina = case gluedNormalArc t ina of
                              Nothing -> ina
                              Just ina' -> min ina ina'

instance TriangulationDSnakeItem INormalCorner where
    canonicalize t (viewI -> I i (edge -> e)) = 
        iNormalCorner $ canonicalize t (i ./ e)

-- | Identity
instance TriangulationDSnakeItem INormalTri where
    canonicalize _ = id

-- | Identity
instance TriangulationDSnakeItem INormalQuad where
    canonicalize _ = id

-- | Identity
instance TriangulationDSnakeItem INormalDisc where
    canonicalize _ = id



addGluings
  :: Triangulation
     -> [Gluing]
     -> Either TriangulationConstructionError Triangulation
addGluings tr gluings = 
    assert (all (\g -> getTIndex (fst g) < tindex n && getTIndex (snd g) < tindex n) gluings) $

    mkTriangulationSafe n (tOriginalGluings tr ++ gluings)
  where
    n = tNumberOfTetrahedra tr


boundaryITriangles :: Triangulation -> [ITriangle]
boundaryITriangles tr = filter (isBoundaryITriangle tr) (tITriangles tr)




tNumberOfNormalDiscTypes :: Num r => Triangulation -> r
tNumberOfNormalDiscTypes = liftM (7*) tNumberOfTetrahedra

tNumberOfNormalQuadTypes :: Num r => Triangulation -> r
tNumberOfNormalQuadTypes = liftM (3*) tNumberOfTetrahedra

tr_l31 :: Triangulation
tr_l31 = mkTriangulation 2 
    ((0./tABC,1./oBCA) 
    :
     fmap ((0./) &&& ((1./) . toOrderedFace)) [tABD,tACD,tBCD])
                        



disorderEquivalenceClass
  :: (IsEquivalenceClass cls, OrderableFace elt (Element cls)) =>
     cls -> EqvClassImpl elt
disorderEquivalenceClass = ecMap forgetVertexOrder


-- | Returns the normal triangles adjacent to the given one, and the arcs witnessing the adjacencies (the first arc of each pair is the arc of the input triangle) 
adjacentNormalTris
  :: 
     Triangulation -> INormalTri -> [(Pair INormalArc, INormalTri)]
adjacentNormalTris tr nt =
    let
        arcs = normalArcList nt
    in
        mapMaybe (\arc -> do
            arc' <- gluedNormalArc tr arc
            return ((arc, arc'), iNormalTriByNormalArc arc'))

            arcs

isCanonicallyOrderedClass
  :: (Eq (VertexSymGroup t),
      IsEquivalenceClass cls,
      OrderableFace t (Element cls)) =>
     cls -> Bool
isCanonicallyOrderedClass cl = getVertexOrder (canonicalRep cl) == mempty


tINormalDiscNu :: Triangulation -> Numbering INormalDisc
tINormalDiscNu tr = enumNu' 0 (tNumberOfNormalDiscTypes tr-1)

tINormalArcNu :: Triangulation -> Numbering INormalArc
tINormalArcNu tr = enumNu' 0 (12*tNumberOfTetrahedra tr - 1)

type LabelledTriangulation = (String,Triangulation)

labelledTriangulation :: String -> Triangulation -> LabelledTriangulation
labelledTriangulation = (,) 

instance Binary.Binary Triangulation where
    get = mkTriangulation <$> Binary.get <*> Binary.get
    put = 
        (>>) 
            <$> (Binary.put <$> tNumberOfTetrahedra_)
            <*> (Binary.put <$> tGluingsIrredundant)


lookupGluingAsGluing :: Triangulation -> ITriangle -> Maybe Gluing
lookupGluingAsGluing tr t = fmap (t,) . lookupGluingOfITriangle tr $ t

tetGetGluings :: Triangulation -> TIndex -> [Gluing]
tetGetGluings tr (i :: TIndex) =
    mapMaybe (lookupGluingAsGluing tr) (triangleList i)

dfsFacePairingGraph
  :: Triangulation -> TIndex -> EdgeLabelledTree TIndex Gluing
dfsFacePairingGraph tr = flip dfs (map (id &&& (getTIndex . glCod)) . tetGetGluings tr)

isClosedTriangulation :: Triangulation -> Bool
isClosedTriangulation tr = M.size (tGlueMap_ tr) == 4 * tNumberOfTetrahedra tr  


-- | Note: This instance checks exact equality of 'tNumberOfTetrahedra_' and @sort . 'tNormalizedGluings'@.
instance Eq Triangulation where
    (==) = (==) `on` (tNumberOfTetrahedra_ &&& (sort . tNormalizedGluings))

deriveNFData ''Triangulation

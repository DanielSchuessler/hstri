{-# LANGUAGE DeriveDataTypeable, TupleSections, PatternGuards, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module Triangulation(
    module Tetrahedron,
    module Equivalence,
    module Triangulation.FacetGluing,
    module Triangulation.GlueMap,
    module EitherC,
    Triangulation,
    
    -- * Properties 
    tNumberOfTetrahedra_,tNumberOfTetrahedra,tTetrahedra_,tGlueMap_, 
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
    mkTriangulationSafe,
    mkTriangulationG,
    triang,
    glueMapToTriangulation,
    glueMapToTriangulationSafe,
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
import Data.Function
import Data.List(sort)
import Data.Maybe
import Data.Numbering
import Data.Typeable(Typeable)
import Equivalence
import HomogenousTuples
import Tetrahedron.INormalDisc
import Language.Haskell.TH.Lift
import Prelude hiding(catch,lookup)
import Quote
import Tetrahedron
import Tetrahedron.NormalDisc
import Triangulation.FacetGluing
import Util
import qualified Data.Binary as Binary
import qualified Data.List as List
import FileLocation
import EitherC
import Triangulation.GlueMap
import Triangulation.PreTriangulation
import qualified Data.Map as M
import Data.Map(Map)


-- Note: All fields except the first two ('triangTets_', 'tGlueMap_') are semantically redundant, 
-- but kept in this data structure for memoization purposes
data Triangulation = Triangulation { 
    tGlueMap :: GlueMap,
    oEdgeEqv :: Equivalence OIEdge, 
    vertexEqv :: Equivalence IVertex
--    normalArcEqv :: Equivalence INormalArc
}

deriveNFData ''Triangulation

instance PreTriangulation Triangulation where
    glueMap = tGlueMap



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
        

mkTriangulationSafe :: Word -> [Gluing] -> 
    EitherC (Located TriangulationConstructionError) Triangulation
mkTriangulationSafe n gluings
        = 
            


    let tets = 
            if n > 0
            then [0.. tindex (pred n)] 
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


    in let 
        vertexEqv :: Equivalence IVertex 
        vertexEqv =  {-# SCC "mkTriangulationSafe/vertexEqv" #-}
                        mkEquivalence
                            (inducedVertexEquivalences =<< gluings)
                            allIVertices

        oEdgeEqv = 
                        {-# SCC "mkTriangulationSafe/oEdgeEqv" #-}
                        mkEquivalence
                            (inducedOEdgeEquivalences =<< gluings)
                            _allOIEdges

    in let 
        addGluing mrec (t,f1) = do
            rec <- mrec 
            case M.lookup t rec of
                    Nothing -> return (M.insert t f1 rec)
                    Just f2 
                        | f2==f1 -> return rec
                        | otherwise -> _err t f1 f2

        _err t f1 f2 = $failure 
                        (if t == forgetVertexOrder f1
                            then TriangleGluedToSelf f1
                            else ThreeTrianglesGlued t f1 f2)


        
    in do

        gm_map <- 
                        List.foldl' addGluing (return mempty) 
                            (symmetrizeGluings gluings)

        {-# SCC "mkTriangulationSafe/call:checkForEdgeGluedToSelfInReverse" #-}
            checkForEdgeGluedToSelfInReverse allIEdges oEdgeEqv
                                
        {-# SCC "mkTriangulationSafe/return" #-}
            return Triangulation{ tGlueMap = unsafeMkGlueMap n gm_map
                                                , oEdgeEqv, vertexEqv }


checkForEdgeGluedToSelfInReverse
  :: [IEdge]
     -> Equivalence OIEdge -> EitherC (Located TriangulationConstructionError) ()
checkForEdgeGluedToSelfInReverse allIEdges oEdgeEqv = mapM_ go allIEdges
    where
        go e = 
            {-# SCC "checkForEdgeGluedToSelfInReverse/go" #-}
            if eqv_classOf oEdgeEqv (toOrderedFace e) == eqv_classOf oEdgeEqv (packOrderedFace e Flip)
               then $failure (EdgeGluedToSelfInReverse e)
               else return ()
    

-- | Allows an arbitrary tetrahedron index set
mkTriangulationG
  :: (Ord tet, Show tet) =>
     [tet] -> [((tet, Triangle), (tet, OTriangle))] 
        -> EitherC (Located TriangulationConstructionError) Triangulation
mkTriangulationG tets gluings =
    let
        tetIxs = [tindex 0..]
        tetIxMap = M.fromListWithKey 
                    (\k _ _ -> $err' ("mkTriangulationG: Duplicate tetrahedron: "++show k)) 
                    (zip tets tetIxs)

        translateGluing ((tet,tri),(tet',otri)) = 
            ((tetIxMap M.! tet) ./ tri, (tetIxMap M.! tet') ./ otri)

    in
        mkTriangulationSafe (fi $ length tets) (translateGluing <$> gluings) 

instance Quote Triangulation where
    quotePrec prec t = quoteParen (prec >= 11) ("mkTriangulation "
                                                    ++ quotePrec 11 (tNumberOfTetrahedra_ t)
                                                    ++ " "
                                                    ++ quotePrec 11 (tGluingsIrredundant t))

instance Lift Triangulation where
    lift t = [| mkTriangulation $(lift $ tNumberOfTetrahedra_ t) $(lift $ tGluingsIrredundant t) |]



lookupGluingOfITriangle :: Triangulation -> ITriangle -> Maybe OITriangle
lookupGluingOfITriangle = flip pt_lookup

lookupGluingOfOITriangle :: Triangulation -> OITriangle -> Maybe OITriangle
lookupGluingOfOITriangle = flip lookupO





tOIEdgeEquivalents :: Triangulation -> OIEdge -> [OIEdge]
tOIEdgeEquivalents tr oiedge = eqv_equivalents (oEdgeEqv tr) oiedge

tOIEdgeDegree :: Triangulation -> OIEdge -> Int
tOIEdgeDegree tr = length . tOIEdgeEquivalents tr



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
        case M.lookup rep (tGlueMap_ t) of
                                Just (forgetVertexOrder -> rep') | rep' < rep -> rep'
                                _ -> rep



gluedNormalArc :: Triangulation -> INormalArc -> Maybe INormalArc
gluedNormalArc tr ina = 
        let
            tri = iNormalArcGetTriangle ina
        in
            case M.lookup tri (tGlueMap_ tr) of
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
     -> EitherC (Located TriangulationConstructionError) Triangulation
addGluings tr gluings = 
    assert (all (\g -> getTIndex (fst g) < tindex n && getTIndex (snd g) < tindex n) gluings) $

    mkTriangulationSafe n (tGluingsIrredundant tr ++ gluings)
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




isClosedTriangulation :: Triangulation -> Bool
isClosedTriangulation = pt_isClosed


-- | Note: This instance checks exact equality of 'tNumberOfTetrahedra_' and @sort . 'tNormalizedGluings'@.
instance Eq Triangulation where
    (==) = (==) `on` (tNumberOfTetrahedra_ &&& (sort . tNormalizedGluings))


glueMapToTriangulationSafe
  :: GlueMap -> EitherC (Located TriangulationConstructionError) Triangulation
glueMapToTriangulationSafe gm = 
    mkTriangulationSafe 
        (numberOfTetrahedra gm)
        (gluingsIrredundant gm)

glueMapToTriangulation :: GlueMap -> Triangulation
glueMapToTriangulation =
    fmap
        $(unEitherC)
        glueMapToTriangulationSafe 


tGluingsIrredundant :: Triangulation -> [Gluing]
tGluingsIrredundant = gluingsIrredundant

tNormalizedGluings :: Triangulation -> [NormalizedGluing]
tNormalizedGluings = normalizedGluings

tTetrahedra_ :: Triangulation -> [TIndex]
tTetrahedra_ = pt_tetrahedra

tNumberOfTetrahedra_ :: Triangulation -> Word
tNumberOfTetrahedra_ = numberOfTetrahedra_

tGlueMap_ :: Triangulation -> Map ITriangle OITriangle
tGlueMap_ = glueMapMap

tNumberOfTetrahedra :: Num c => Triangulation -> c
tNumberOfTetrahedra = numberOfTetrahedra

mkTriangulation :: Word -> [Gluing] -> Triangulation
mkTriangulation = 
    (fmap . fmap)
        $(unEitherC)
        mkTriangulationSafe 

-- | Convenience function for the special case that there are no isolated tetrahedra
triang ::  [Gluing] -> Triangulation
triang gluings = mkTriangulation n gluings
    where
        n | null gluings = 0
          | otherwise = maximum (concatMap (\(t,o) -> 
                                    [ fi $ getTIndex t, fi $ getTIndex o ]) gluings) + 1

instance UpdatablePreTriangulation Triangulation where
    setGlueMap gm _ = glueMapToTriangulation gm

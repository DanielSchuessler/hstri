{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-imports -fwarn-missing-local-sigs #-}
module Triangulation.PreTriangulation where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.EdgeLabelledTree
import Data.Lens.Common
import Data.Map(Map)
import Data.Maybe
import Data.Word
import Element
import Tetrahedron
import Tetrahedron.INormalDisc
import Tetrahedron.NormalDisc
import Triangulation.FacetGluing
import Triangulation.GlueMap
import Util
import qualified Data.Map as M

class PreTriangulation tr where
    glueMap :: tr -> GlueMap 

instance PreTriangulation GlueMap where glueMap = id

class UpdatablePreTriangulation tr where 
    setGlueMap :: GlueMap -> tr -> tr

instance UpdatablePreTriangulation GlueMap where setGlueMap = const

glueMapLens
  :: (UpdatablePreTriangulation tr, PreTriangulation tr) =>
     Lens tr GlueMap
glueMapLens = lens glueMap setGlueMap 

modifyGlueMap
  :: (UpdatablePreTriangulation tr, PreTriangulation tr) =>
     (GlueMap -> GlueMap) -> tr -> tr
modifyGlueMap = modL glueMapLens

numberOfTetrahedra_ :: PreTriangulation tr => tr -> Word
numberOfTetrahedra_ = gm_numberOfTetrahedra . glueMap

numberOfTetrahedra :: (Num c, PreTriangulation tr) => tr -> c
numberOfTetrahedra = fi . numberOfTetrahedra_

glueMapMap :: PreTriangulation tr => tr -> Map ITriangle OITriangle
glueMapMap = gm_map . glueMap


-- | Lookup what the given triangle is glued to
pt_lookup
  :: PreTriangulation tr => ITriangle -> tr -> Maybe OITriangle
pt_lookup x gm = M.lookup x (glueMapMap gm)

-- | Lookup what the given ordered triangle is glued to
lookupO
  :: PreTriangulation tr => OITriangle -> tr -> Maybe OITriangle
lookupO (unpackOrderedFace -> (tri,g)) gm = (*. g) <$> pt_lookup tri gm

gluingsIrredundant :: PreTriangulation tr => tr -> [Gluing]
gluingsIrredundant = map ngToGluing . normalizedGluings 

normalizedGluings :: PreTriangulation tr => tr -> [NormalizedGluing]
normalizedGluings = 
    mapMaybe (\(tri,otri) -> 
        case normalizeGluing' (gluing tri otri) of
            (True, ng) -> Just ng
            _ -> Nothing)

            . M.toList . glueMapMap

pt_tetrahedra :: PreTriangulation tr => tr -> [TIndex]
pt_tetrahedra tr = if n==0 then [] else [0 .. tindex (n - 1)]
    where
        n = numberOfTetrahedra_ tr


tITriangles :: PreTriangulation tr => tr -> [ITriangle]
tITriangles = concatMap triangleList . pt_tetrahedra

tOITriangles :: PreTriangulation tr => tr -> [OITriangle]
tOITriangles tr = liftM2 packOrderedFace (tITriangles tr) allS3

tIVertices :: PreTriangulation tr => tr -> [IVertex]
tIVertices = concatMap vertexList . pt_tetrahedra 

tIEdges :: PreTriangulation tr => tr -> [IEdge]
tIEdges = concatMap edgeList . pt_tetrahedra 

tINormalCorners :: PreTriangulation tr => tr -> [INormalCorner]
tINormalCorners = concatMap normalCornerList . pt_tetrahedra 
tINormalArcs :: PreTriangulation tr => tr -> [INormalArc]
tINormalArcs = concatMap normalArcList . pt_tetrahedra 
tINormalTris :: PreTriangulation tr => tr -> [INormalTri]
tINormalTris = concatMap normalTriList . pt_tetrahedra 
tINormalQuads :: PreTriangulation tr => tr -> [INormalQuad]
tINormalQuads = concatMap normalQuadList . pt_tetrahedra 


tINormalDiscs :: PreTriangulation tr => tr -> [INormalDisc]
tINormalDiscs = concatMap normalDiscList . pt_tetrahedra 


tOIEdges :: PreTriangulation tr => tr -> [OIEdge]
tOIEdges = concatMap (asList . allOIEdges) . pt_tetrahedra


isBoundaryITriangle :: PreTriangulation tr => tr -> ITriangle -> Bool
isBoundaryITriangle t x = not (x `M.member` glueMapMap t)

pt_isClosed :: PreTriangulation tr => tr -> Bool
pt_isClosed tr = M.size (glueMapMap tr) == 4 * numberOfTetrahedra tr  

lookupGluingAsGluing :: PreTriangulation tr => tr -> ITriangle -> Maybe (ITriangle, OITriangle)
lookupGluingAsGluing tr t = fmap (t,) . flip pt_lookup tr $ t

tetGetGluings :: PreTriangulation tr => tr -> TIndex -> [Gluing]
tetGetGluings tr (i :: TIndex) =
    mapMaybe (lookupGluingAsGluing tr) (triangleList i)

dfsFacePairingGraph :: PreTriangulation tr => tr -> TIndex -> EdgeLabelledTree TIndex Gluing
dfsFacePairingGraph tr = flip dfs (map (id &&& (getTIndex . glCod)) . tetGetGluings tr)

-- | The 'TIndex' argument to the first argument is the newly created tet
addTet
  :: (UpdatablePreTriangulation tr, PreTriangulation tr) =>
     (TIndex -> [Gluing]) -> tr -> EitherC (Located ErrorCall) (tr,TIndex)
addTet gls tr = do
    (gm',i) <- gm_addTet gls (glueMap tr)
    return (setGlueMap gm' tr,i)



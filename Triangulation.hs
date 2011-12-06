{-# LANGUAGE PatternGuards, BangPatterns, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, Rank2Types, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module Triangulation(
    module AbstractTetrahedron,
    module Equivalence,
    module FacetGluing,
    Triangulation,
    
    -- * Properties 
    tTetrahedra_,tGluings_, tGlueMap_, edgeEqv,oEdgeEqv,vertexEqv,triangTetCount,
    embedsEdges,
    lookupGluingOfITriangle,
    lookupGluingOfOITriangle,
    tIVertices,
    tIEdges,
    tITriangles,
    tOITriangles,
    tOIEdgeEquivalents,
    tOIEdgeDegree,
    tGluingsIrredundant,
    tINormalCorners,
    tINormalTris,
    tINormalArcs,
    tINormalQuads,
    tINormalDiscs,
    tOIEdges,
    getOIEdgeGluingSense,
    -- * Construction 
    mkTriangulation,mkTriangulationG,triang,randomTriangulation,randT,
    -- * Canonical representatives for things that are glued
    canonicalizeIVertex,
    canonicalizeIEdge,
    canonicalizeOIEdge,
    canonicalizeITriangle,
    canonicalizeINormalArc,
    canonicalizeINormalCorner,



    -- * Testing
    qc_Triangulation,prop_tGluingsIrredundant
    
    ) where

import AbstractTetrahedron
import Collections
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import qualified Data.List as List
import Equivalence
import HomogenousTuples
import Prelude hiding(catch,lookup)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import PrettyUtil
import TupleTH
import FacetGluing
import INormalDisc
import NormalDisc
import Element
import Quote
import QuickCheckUtil


forAllNonEmptyIntTriangulations :: Testable prop => (Triangulation -> prop) -> Property
forAllNonEmptyIntTriangulations f = property (\t ->
    not (Prelude.null (tTetrahedra_ t)) ==>
        (f t)  )

genI ::  Arbitrary a => Triangulation -> Gen (I a)
genI t = liftM2 I (genTet t) arbitrary

tGluingsIrredundant :: Triangulation -> [Gluing]
tGluingsIrredundant tr = 
    let
        gluings = tGluings_ tr
        gluingsFirstTris = setFromList (fst <$> gluings) 

        isRedundant (tri,unpackOrderedFace -> (_,tri')) =
            (tri > tri') && member tri' gluingsFirstTris 
    in
        filter (not . isRedundant) gluings


prop_tGluingsIrredundant :: Triangulation -> Bool
prop_tGluingsIrredundant tr = 
    tGlueMap_ tr == tGlueMap_ (fromRight $ mkTriangulation (tTetrahedra_ tr) (tGluingsIrredundant tr)) 


-- Note: All fields except the first two ('triangTets_', 'triangGluings_') are semantically redundant, 
-- but kept in this data structure for memoization purposes
data Triangulation = Triangulation { 
    tTetrahedra_ :: [TIndex], 
    tGluings_ :: [Gluing],

    -- | INVARIANT: 
    --
    -- @lookup (ITriangle x y) tGlueMap_ = Just (OITriangle x' y' g')@ 
    -- implies 
    -- @lookup (ITriangle x' y') tGlueMap_ = Just (OITriangle x y (inv g'))@
    --
    -- (for all @f0@, @f1@, @g@)
    tGlueMap_ :: Map ITriangle OITriangle,


    edgeEqv :: Equivalence IEdge, 
    oEdgeEqv :: Equivalence OIEdge, 
    vertexEqv :: Equivalence IVertex
--    normalArcEqv :: Equivalence INormalArc
}


        



instance Pretty Triangulation where
    pretty Triangulation{..} =
        prettyRecord "Triangulation" fields

          where
            fields = [ ("Tetrahedron indices", pretty (fmap (dullcyan . pretty) tTetrahedra_)) 
                     , ("Triangle gluings", pretty tGluings_)
--                     , ("Edges", pretty edgeEqv)
                     , ("Ordered edges", pretty oEdgeEqv)
                     , ("Vertices", pretty vertexEqv)
                     ]



newtype TriangulationBuilder a = TriangulationBuilder (Writer [(ITriangle,OITriangle)] a) 
    deriving(Monad)

-- instance TIndex => Show Triangulation where
--     show (Triangulation count gluings _) = 
--         unlines (("Triangulation with "++show count++" tetrahedr"++(if count==1 then "on" else "a")++" {")
--                 : gluingsStr
--                 ++ ["}"])
--             where
--                 gluingsStr = [ $(printf "\tindex %H ~ %H") 
--                                     two
--                                     otwo
-- 
--                         | (two,otwo) <- assocs gluings ] 
--



runTriangulationBuilder :: [TIndex] -> TriangulationBuilder a -> Triangulation
runTriangulationBuilder tTetrahedra_ (TriangulationBuilder x) 
    = either error id (mkTriangulation tTetrahedra_ (execWriter x)) 


mkTriangulation :: [TIndex] -> [Gluing] -> Either String Triangulation
mkTriangulation tTetrahedra_ tGluings_
        =   
            assert (not . hasDuplicates $ tTetrahedra_) (do

    let allIEdges :: [IEdge]
        allIEdges = concatMap edgeList tTetrahedra_
        _allOIEdges = concatMap (asList . allOIEdges) tTetrahedra_

        allIVertices :: [IVertex]
        allIVertices = concatMap vertexList allIEdges

--         allINormalArcs :: [INormalArc]
--         allINormalArcs = [ i ./ na | i <- tTetrahedra_ , na <- allNormalArcs ]

    let addGluing mrec (t,f1) = mrec >>= (\r -> case lookup t r of
                                                        Nothing -> return (mapInsert t f1 r)
                                                        Just f2 
                                                            | f2==f1 -> return r
                                                            | otherwise -> err t f1 f2)

        err t f1 f2 = Left 
                        (if t == forgetVertexOrder f1
                            then 
                                ("Triangle "++show t++" is glued to itself (in order "++show f1++")")
                            else    
                                ("Triangle "++show t++" is glued to both triangle "++show f1++" and "++show f2))
        
    tGlueMap_ <- List.foldl' addGluing (return mempty) 
                        (tGluings_ ++ fmap flipGluing tGluings_)


    let vertexEqv :: Equivalence IVertex 
        vertexEqv = mkEquivalence
            [ pair |
                (two,otwo') <- tGluings_,
                pair <- zip (vertexList two) (vertexList otwo')
                ]

                allIVertices

        edgeEqv :: Equivalence IEdge
        edgeEqv = mkEquivalence
            [ pair | 
                
                  (two,otwo') <- tGluings_
                , pair <- zip (edgeList two)
                              (forgetVertexOrder <$> edgeList otwo')
            ]

            allIEdges


        oEdgeEqv = mkEquivalence pairs _allOIEdges
            where
                pairs =
                    [ pair | 
                        
                        (two,otwo') <- tGluings_
                        , pair0 <- zip (edgeList (packOrderedFace S3abc two))
                                       (edgeList otwo')

                                    :: [(OIEdge, OIEdge)]

                        , pair <- [ pair0,  map2 (Flip .*) pair0 ]
                    ]

--         normalArcEqv = mkEquivalence
--             
--             allINormalArcs



                            
    return Triangulation{ tTetrahedra_, tGlueMap_, edgeEqv, oEdgeEqv, vertexEqv, tGluings_ })

glue :: TIndex -> Triangle -> TIndex -> (S3, Triangle) -> TriangulationBuilder ()
glue i0 f0 i1 (g,f1) = TriangulationBuilder (tell [(i0 ./ f0, i1 ./ (packOrderedFace g f1))])




-- | Convenience function for the special case that there are no isolated tetrahedra
triang ::  [Gluing] -> Triangulation
triang gluings = fromRight $ mkTriangulation (nub' $ tets) gluings
    where
        tets = concatMap (\(t,o) -> [ getTIndex t, getTIndex o ]) gluings

-- tt3a = ti tt3 1 A
-- tt3d = ti tt3 1 D
-- tt3ac = ti tt3 1 AC


-- ti ::  Triangulation -> TIndex -> a -> T (I a)
-- ti t i a = T t (I i a)



instance Show Triangulation where
    showsPrec = prettyShowsPrec 



instance Arbitrary Triangulation where
    arbitrary = {-# SCC "arbitrary/Triangulation" #-} 

        sized (\n -> do
            nTets <- choose (1::Int,max 1 (n`div`5))
            nGluings <- choose (0,2*nTets)
            randT nTets nGluings
            )



    shrink t = concatMap removeTet (tTetrahedra_ t) ++ fmap removeGluing (tGluings_ t)
        where
            -- we can remove a tetrahedron iff it is isolated
            removeTet tet =
                if $(allTuple 4) (isBoundaryITriangle t) (triangles tet)
                then [ fromRight $ mkTriangulation (List.delete tet (tTetrahedra_ t)) (tGluings_ t) ]
                else []

            removeGluing g = fromRight $ mkTriangulation (tTetrahedra_ t) (List.delete g (tGluings_ t))

generateUntilRight :: Show a => Gen (Either a b) -> Gen b
generateUntilRight g = fromRight <$> (g `suchThat` isRight)

randT :: Int -> Int -> Gen Triangulation
randT nTets nGluings = assert (nGluings <= 2*nTets) generateUntilRight go
    where
        go = do
            let tets = (tindex . fromIntegral) <$> [1..nTets]


            let loop :: [Gluing] -> Int -> StateT (Set ITriangle) Gen [Gluing] 
                loop acc 0 = return acc
                loop acc j = do
                    t1 <- takeTriangle
                    t2 <- takeTriangle
                    g <- lift arbitrary
                    loop ((t1,packOrderedFace g t2):acc) (j-1)

                takeTriangle = do
                    trianglesLeft <- get
                    ix <- lift (choose (0, setSize trianglesLeft-1))
                    let res = elemOfSetAt ix trianglesLeft
                    put (deleteAt ix trianglesLeft)
                    return res

--            ngluings <- choose (0,2*nTets)

            pairs <- evalStateT (loop [] nGluings) (setFromList ((./) <$> tets <*> allTriangles))
            
            return $ mkTriangulation tets pairs

        
        




isBoundaryITriangle ::  Triangulation -> ITriangle -> Bool
isBoundaryITriangle t x = not (x `memberOfMap` tGlueMap_ t)



randomTriangulation :: Int -> Int -> IO Triangulation
randomTriangulation nTets nGluings = do
    g <- newStdGen 
    return $ unGen (randT nTets nGluings) g 0 

triangTetCount :: Triangulation -> Int
triangTetCount = length `liftM` tTetrahedra_


qc_Triangulation ::  IO Bool
qc_Triangulation = $quickCheckAll




genTet ::  Triangulation -> Gen TIndex
genTet = elements . tTetrahedra_ 


mkTriangulationG :: 
    (Show t, Ord t, t ~ TetIndex g, GluingSpec g) => 
    [t] -> [g] -> Either String Triangulation
mkTriangulationG tets gluings =
    let
        tetIxs = [tindex 0..]
        tetIxMap = fromListWithKey (\k _ _ -> error ("Duplicate tetrahedron: "++show k)) (zip tets tetIxs)
    in
        mkTriangulation (elems tetIxMap) (toGluing (tetIxMap!) <$> gluings) 



embedsEdges :: Triangulation -> Either String ()
embedsEdges = mapM_ checkClass . eqvClasses . oEdgeEqv
    where
        checkClass :: EquivalenceClass OIEdge -> Either String ()
        checkClass ec = mapM_ checkEdge (equivalents ec)
            where
                checkEdge e = when (ecMember (Flip .* e) ec) (Left ("Edge "++show e++" is glued to itself in reverse"))



instance Quote Triangulation where
    quotePrec prec t = quoteParen (prec >= 11) ("mkTriangulation "
                                                    ++ quotePrec 11 (tTetrahedra_ t)
                                                    ++ " "
                                                    ++ quotePrec 11 (tGluings_ t))




lookupGluingOfITriangle :: Triangulation -> ITriangle -> Maybe OITriangle
lookupGluingOfITriangle t tri =
    lookup tri (tGlueMap_ t)

lookupGluingOfOITriangle :: Triangulation -> OITriangle -> Maybe OITriangle
lookupGluingOfOITriangle t (unpackOrderedFace -> (g,tri)) =
    (g .*) <$> lookupGluingOfITriangle t tri




tOIEdgeEquivalents :: Triangulation -> OIEdge -> [OIEdge]
tOIEdgeEquivalents tr oiedge = eqv_equivalents (oEdgeEqv tr) oiedge

tOIEdgeDegree :: Triangulation -> OIEdge -> Int
tOIEdgeDegree tr = length . tOIEdgeEquivalents tr


tITriangles :: Triangulation -> [ITriangle]
tITriangles = concatMap triangleList . tTetrahedra_

tOITriangles :: Triangulation -> [OITriangle]
tOITriangles tr = liftM2 packOrderedFace allS3 (tITriangles tr)

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
        else if eq (Flip .* oedge1) oedge2 
        then Just Flip
        else Nothing


prop_getOIEdgeGluingSense_same :: Triangulation -> Property
prop_getOIEdgeGluingSense_same tr = forAllElements (tOIEdges tr)
    (\e -> getOIEdgeGluingSense tr e e == Just NoFlip )
    
prop_getOIEdgeGluingSense_sym :: Triangulation -> Property
prop_getOIEdgeGluingSense_sym tr = 
    join forAllElements2 (tOIEdges tr) (\(e1, e2) -> f e1 e2 == f e2 e1)
  where
        f = getOIEdgeGluingSense tr

canonicalizeIVertex
  :: Triangulation -> IVertex -> IVertex
canonicalizeIVertex t x = eqvRep (vertexEqv t) x

canonicalizeIEdge
  :: Triangulation -> IEdge -> IEdge
canonicalizeIEdge t x = (eqvRep (edgeEqv t) x)

canonicalizeOIEdge
  :: Triangulation -> OIEdge -> OIEdge
canonicalizeOIEdge t x = (eqvRep (oEdgeEqv t) x)

canonicalizeITriangle :: Triangulation -> ITriangle -> ITriangle
canonicalizeITriangle t rep = 
    case lookup rep (tGlueMap_ t) of
                            Just (forgetVertexOrder -> rep') | rep' < rep -> rep'
                            _ -> rep

canonicalizeINormalArc :: Triangulation -> INormalArc -> INormalArc
canonicalizeINormalArc t ina =
    let
        tri = iNormalArcGetTriangle ina
    in
          case lookup tri (tGlueMap_ t) of
                            Just otri | _tri' <- forgetVertexOrder otri, 
                                        _tri' < tri -> 
                                
                                    
                                 (gluingMap (tri,otri) ina)
                                    

                            _ -> ina

canonicalizeINormalCorner :: Triangulation -> INormalCorner -> INormalCorner
canonicalizeINormalCorner t (viewI -> I i (edge -> e)) = 
      iNormalCorner $ canonicalizeIEdge t (i ./ e)

prop_forgetVertexOrder_natural_for_canonicalization :: Triangulation -> Property
prop_forgetVertexOrder_natural_for_canonicalization t =
    forAllElements (tOIEdges t)
        (\e -> canonicalizeIEdge t (forgetVertexOrder e) == forgetVertexOrder (canonicalizeOIEdge t e)) 


{-# LANGUAGE TupleSections, BangPatterns, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, Rank2Types, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module TriangulationCxtObject where

import AbstractTetrahedron
import Collections
import Control.Arrow
import Control.Monad.Reader
import Data.Function
import Data.Functor
import Data.Maybe
import Equivalence
import HomogenousTuples
import IndexedSimplices
import Prelude hiding(catch,lookup)
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import Triangulation
import TupleTH
import UPair


class TriangulationCxtObject a where
    data T a
    viewT :: T a -> (Triangulation,a)

getT ::  TriangulationCxtObject a => T a -> Triangulation
getT (viewT -> (t,_)) = t

unT ::  TriangulationCxtObject a => T a -> a
unT (viewT -> (_,a)) = a





instance TriangulationCxtObject IVertex where
    data T IVertex = TVertex {
                        tv_tlation :: Triangulation 
                      , tv_anyrep :: IVertex 
                      , tv_canonicalRep :: IVertex 
                    }
    viewT = tv_tlation &&& tv_anyrep

-- | Vertex of a triangulation
type TVertex = T IVertex

instance TriangulationCxtObject IEdge where
    data T IEdge = TEdge {
                        te_tlation :: Triangulation 
                      , te_anyrep :: IEdge 
                      , te_canonicalRep :: IEdge 
                    }
    viewT = te_tlation &&& te_anyrep

-- | Edge of a triangulation
type TEdge = T IEdge


instance TriangulationCxtObject ITriangle where
    data T ITriangle = TTriangle { 
                    tt_tlation :: Triangulation 
                  , tt_anyrep :: ITriangle -- arbitrary rep 
                  }
    viewT = tt_tlation &&& tt_anyrep

-- | Triangle of a triangulation
type TTriangle = T ITriangle


instance TriangulationCxtObject INormalArc where
    data T INormalArc = TNormalArc 
                    Triangulation 
                    INormalArc -- arbitrary rep 
    viewT (TNormalArc t x) = (t, x)

type TNormalArc = T INormalArc

instance TriangulationCxtObject INormalCorner where
    data T INormalCorner = TNormalCorner 
                    Triangulation 
                    INormalCorner -- arbitrary rep 
    viewT (TNormalCorner t x) = (t, x)

type TNormalCorner = T INormalCorner


tvertex :: Triangulation -> IVertex -> TVertex
tvertex t x = TVertex t x (eqvRep (vertexEqv t) x)

tedge :: Triangulation -> IEdge -> TEdge
tedge t x = TEdge t x (eqvRep (edgeEqv t) x)
ttriangle :: Triangulation -> ITriangle -> TTriangle

ttriangle = TTriangle


tVertices ::  MonadReader Triangulation m => m [TVertex]
tVertices = asks eqvClasses 

tEdges ::  MonadReader Triangulation m => m [TEdge]
tEdges = asks eqvClasses 

tTriangles ::  MonadReader Triangulation m => m [TTriangle]
tTriangles = asks eqvClasses

instance HasEquivalence Triangulation TVertex IVertex where
    eqvClasses t = fmap (\ec -> let rep = canonicalRep ec in TVertex t rep rep) (eqvClasses (vertexEqv t))
    eqvClassOf t x = tvertex t x


instance HasEquivalence Triangulation TEdge IEdge where
    eqvClasses t = fmap (\ec -> let rep = canonicalRep ec in TEdge t rep rep) (eqvClasses (edgeEqv t))
    eqvClassOf t x = tedge t x


instance HasEquivalence Triangulation TTriangle ITriangle where
    eqvClasses t =
        let gluings = tGluings_ t
        in (tBoundaryTriangles t ++ (ttriangle t . fst <$> gluings ))

    eqvClassOf t x = ttriangle t x

instance IsEquivalenceClass TVertex IVertex where 
    canonicalRep = tv_canonicalRep
    equivalents (viewT -> (t,x)) = eqvEquivalents (vertexEqv t) $ x
              
    ecSize (viewT -> (t,x)) = ecSize (eqvClassOf (vertexEqv t) x) 
    ecMember x (viewT -> (t,y)) = ecMember x (eqvClassOf (vertexEqv t) y)


instance IsEquivalenceClass TEdge IEdge where 
    canonicalRep = te_canonicalRep
    equivalents (viewT -> (t,x)) = eqvEquivalents (edgeEqv t) $ x

    ecSize (viewT -> (t,x)) = ecSize (eqvClassOf (edgeEqv t) x) 
    ecMember x (viewT -> (t,y)) = ecMember x (eqvClassOf (edgeEqv t) y)

instance IsEquivalenceClass TTriangle ITriangle where 
    canonicalRep x0@(viewT -> (_, !rep)) = {-# SCC "canonicalRep/TTriangle" #-} 
                                 case lookupGluing x0 of
                                    Just (forgetVertexOrder -> rep') | rep' < rep -> rep'
                                    _ -> rep

    equivalents t@(viewT -> (_,rep)) = rep : maybeToList (fmap forgetVertexOrder $ lookupGluing t)

    ecSize x0 = case lookupGluing x0 of
                                    Just _ -> 2
                                    _ -> 1


    ecMember x ty@(viewT -> (_,y)) = x == y || case lookupGluing ty of
                                          Just y' -> x == forgetVertexOrder y'
                                          _ -> False
                                  
    


equivalentIVertices :: TVertex -> [IVertex]
equivalentIVertices = equivalents 


-- | This does not take into account orientation.
equivalentIEdges :: TEdge -> [IEdge]
equivalentIEdges = equivalents

-- | This does not take into account orientation.
equivalentITriangles :: TTriangle -> [ITriangle]
equivalentITriangles = equivalents 







instance Show TVertex where show = prettyString . pretty
instance Show TEdge where show = prettyString . pretty
instance Show TTriangle where show = prettyString . pretty

instance  Pretty TVertex where
    pretty x = encloseSep lbrace rbrace comma (fmap pretty (equivalentIVertices x))

instance  Pretty TEdge where
    pretty x = encloseSep lbrace rbrace comma (fmap pretty (equivalentIEdges x))

instance  Pretty TTriangle where
    pretty t@(viewT -> (_,rep)) = encloseSep lbrace rbrace comma elts
        where
            elts =  case lookupGluing t of
                        Nothing -> [pretty rep]
                        Just rep' -> [pretty rep,pretty rep']



lookupGluing :: TTriangle -> Maybe OITriangle
lookupGluing (viewT -> (t,rep)) = lookup rep (tGlueMap_ t)





prop_VerticesOfEdge_welldefined :: Triangulation -> Property
prop_VerticesOfEdge_welldefined t = forAllElements (unT <$> tEdges t)
            (mkWellDefinednessProp (eqvEquivalents t)
               (uncurry uPair . vertices_E t)) 
                
                


tBoundaryTriangles ::  Triangulation -> [TTriangle]
tBoundaryTriangles t =
    Prelude.filter isBoundaryTriangle 
        (concatMap (\tet -> ttriangle t . (tet ./) <$> allTriangles) (tTetrahedra_ t))



isBoundaryTriangle :: TTriangle -> Bool
isBoundaryTriangle (viewT -> (t,rep)) = not (rep `memberOfMap` tGlueMap_ t)




                


-- -- | Things that may be identified by the map /p/ from the disjoint union of tetrahedra to the pseudomanifold 
-- class HasEquivClass a where
--     equivClass ::  a -> T t (EquivalenceClass a)
-- 
-- instance HasEquivClass IEdge where
--     equivClass x = flip eqv_class x <$> askEdgeEqv




qc_TriangulationCxtObject ::  IO Bool
qc_TriangulationCxtObject = $quickCheckAll





instance (Eq a, IsEquivalenceClass (T a) a) => Eq (T a) where 
    (==) !x !y = canonicalRep x == canonicalRep y

-- onPar ::  (t1 -> t1 -> a) -> (t -> t1) -> t -> t -> a
-- onPar g f x y = runEval $ do
--     fx <- rpar (f x)
--     fy <- rseq (f y)
--     return (g fx fy)

instance (Ord a, IsEquivalenceClass (T a) a) => Ord (T a) where 
    compare = compare `on` canonicalRep




#ifdef USE_TRIEMAP
instance (Repr a, IsEquivalenceClass (T a) a) => Repr (T a) where
    {-# SPECIALIZE instance (Repr TVertex) #-}
    {-# SPECIALIZE instance (Repr TTriangle) #-}

    type Rep (T a) = Rep a
    type RepList (T a) = DRepList a

    toRep = toRep . canonicalRep
    toRepList = dToRepList
#endif

-- instance Repr (TEdge) where
-- 
--     type Rep (TEdge) = Rep IEdge
--     type RepList (TEdge) = DRepList IEdge
-- 
--     toRep = toRep . canonicalRep
--     toRepList = dToRepList

-- instance Eq TEdge where (==) = (==) `on` canonicalRep
-- instance Ord TEdge where compare = compare `on` canonicalRep

isSubface_VE :: TVertex -> IEdge -> Bool
isSubface_VE x y = any2 (\x' -> x == tvertex (getT x) x') (vertices y)

isSubface_ET ::  TEdge -> ITriangle -> Bool
isSubface_ET x y = $(anyTuple 3) (\x' -> x == tedge (getT x) x') (edges y)

isSubface_TTet :: Triangulation -> ITriangle -> TIndex -> Bool
isSubface_TTet t x i  = any (`isSubface` i) (eqvEquivalents t x :: [ITriangle]) 

instance  IsSubface TVertex TEdge where
    isSubface x y = isSubface_VE x (unT y)

instance  IsSubface TEdge TTriangle where
    isSubface x y = isSubface_ET x (unT y)

instance IsSubface TTriangle TIndex where
    isSubface x i = isSubface_TTet (getT x) (unT x) i

deriving instance Show TNormalArc
deriving instance Show TNormalCorner



prop_IsSubface_TTet_welldefined :: Triangulation -> Property
prop_IsSubface_TTet_welldefined t = 
    forAllElements2 (unT <$> tTriangles t) (tTetrahedra_ t)
        (mkWellDefinednessProp2 (eqvEquivalents t) ((:[]))
            (isSubface_TTet t))  

prop_IsSubface_VE_welldefined :: Triangulation -> Property
prop_IsSubface_VE_welldefined t = 
    forAllElements2 (unT <$> tVertices t) (unT <$> tEdges t)
        (mkWellDefinednessProp2 (eqvEquivalents t) (eqvEquivalents t)
            (\x y -> isSubface_VE (tvertex t x) y))  



prop_IsSubface_ET_welldefined :: Triangulation -> Property
prop_IsSubface_ET_welldefined t = 
    forAllElements2 (unT <$> tEdges t) (unT <$> tTriangles t)
        (mkWellDefinednessProp2 (eqvEquivalents t) (eqvEquivalents t)
            (\x y -> isSubface_ET (tedge t x) y))  






-- -- | This instance is trivial since tetrahedra are never glued together
-- instance CanonicalRep TIndex TIndex where canonicalRep = getTIndex



vertices_E :: Triangulation -> IEdge -> Pair TVertex
vertices_E t rep = map2 (tvertex t) (vertices rep)

instance Vertices (TEdge) (Pair TVertex) where 
    vertices (viewT -> (t,x)) = vertices_E t x

instance Edges (TTriangle) (Triple (TEdge)) where 
    edges (viewT -> (t,rep)) = map3 (tedge t) (edges rep)

instance Triangles (Triangulation, TIndex) (Quadruple (TTriangle)) where
    triangles (t,i) = map4 (ttriangle t) (triangles i)


prop_TIndexToTTriangles_surjective ::  Triangulation -> Property
prop_TIndexToTTriangles_surjective t = setEq (tTriangles t) (concatMap (triangleList . (t,)) (tTetrahedra_ t)) 

prop_TTrianglesToTEdges_surjective ::  Triangulation -> Property
prop_TTrianglesToTEdges_surjective t = setEq (tEdges t) (concatMap edgeList (tTriangles t)) 


prop_TEdgesToTVertices_surjective ::  Triangulation -> Property
prop_TEdgesToTVertices_surjective t = setEq (tVertices t) (concatMap vertexList (tEdges t)) 

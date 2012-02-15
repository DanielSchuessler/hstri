{-# LANGUAGE ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, ViewPatterns, RecordWildCards, TemplateHaskell, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module PreRenderable(
    -- * Reex
    module Math.Groups.S3,
    module Numeric.AD,
    Vec2(..),Vec3(..),(&+),(&-),(&*),(*&),
    TriangleLabel(..),
    defaultTriangleLabelUpDisplacement,

    -- * Edge\/triangle immersions
    GeneralTriangleImmersion(..),
    EdgeImmersion(..),
    TriangleImmersion(..),
    GeneralEdgeImmersion(..),
    evalEdgeImmersion,
    GeneralTetImmersion(..),
    Resolution,

    -- * Main
    EdgeDeco(..),
    Visibility(..),
    FaceName,mkFaceName,unFaceName,
    PreRenderable(..),
    Coords(..),
--     rotateAngleAxis,
    mkPreRenderable,
    mkPreRenderableWithTriangleLabels,
    mkPreRenderableFromTetImmersions,
    tet3d,
    SimplicialTriangleLabelAssoc(..),
    sTriangleLabelAssoc,
    triangleLabelsForSimplicial,


    pr_mapDs,
    pr_popDimension,
    pr_faceName,
    pr_hide,
    pr_hideEds,
    pr_hideTris,
    pr_quad,
    pr_makeImmersionsGeneral,

    -- * Lenses\/getters\/setters
    pr_generalTriangleImmersionL,
    pr_generalEdgeImmersionL,
    pr_setTriangleImmersion,
    pr_setGeneralTriangleImmersion,
    pr_edgeDecoL,
    pr_setEdgeDecoAssocs,
    pr_triangleLabel,
    pr_triangleLabelL,

    -- ** Immersions
    pr_coords,
    pr_triangleImmersion,
    pr_edgeImmersion,

    -- ** Visibility
    visibleIf,
    pr_setVisibility,
    pr_setEdVisibility,
    pr_setTriVisibility,
    pr_visibility,
    pr_vertVisibility,
    pr_edVisibility,
    pr_triVisibility,


    ) where

import Control.Applicative
import Control.Arrow
import Control.Exception(assert)
import Control.Monad
import Data.AdditiveGroup
import Data.AscTuples
import Data.Lens.Common
import Data.Lens.Template
import Data.String
import Data.Vect.Double.Base hiding((*.),(.*),(.*.),Vector)
import Data.VectorSpace
import DisjointUnion
import GHC.Generics
import Language.Haskell.TH.Lift
import Math.Groups.S3
import Numeric.AD(FF,UF,lowerFF,lowerUF,AD)
import Numeric.AD.Vector
import OrphanInstances.Lift()
import PreRenderable.TriangleLabel
import PrettyUtil(Pretty(..),prettyRecord,prettyFunction,prettyPrecFromShow)
import ShortShow
import Simplicial.DeltaSet3
import Simplicial.SimplicialComplex
import THUtil
import Tetrahedron.Vertex
import Util
import qualified Data.Map as M
import Data.Maybe

data EdgeDeco = EdgeDeco { edgeDecoConeCount :: Int, edgeDecoDir :: S2 } 
    deriving (Show)

type Resolution = Int

data GeneralTriangleImmersion = 
    -- | SEMANTICS: 
    --
    -- Let (e0,e1,e2) = edges t
    --
    -- The arc from zero to vec2X corresponds to e0 
    --
    -- The arc from zero to vec2Y corresponds to e1 
    --
    -- The arc from vec2X to vec2Y corresponds to e2 
    GTE Resolution (FF Tup2 Tup3 Double)

instance Coords GeneralTriangleImmersion where
    transformCoords f (GTE res g) = GTE res (f . g)

data TriangleImmersion = 
        FlatTriangle Vec3 Vec3 Vec3
    |   GeneralTriangle GeneralTriangleImmersion

data GeneralEdgeImmersion = 
    -- | SEMANTICS: 
    --
    -- Let (v0,v1) = vertices e
    --
    -- 0 corresponds to v0
    --
    -- 1 corresponds to v1
    --
    GEE Resolution (UF Tup3 Double)

instance Coords GeneralEdgeImmersion where
    transformCoords f (GEE res g) = GEE res (f . g)

data EdgeImmersion =
        FlatEdge Vec3 Vec3
    |   GeneralEdge GeneralEdgeImmersion


class Coords t where
    transformCoords :: (FF Tup3 Tup3 Double) -> t -> t

-- rotateAngleAxis :: Coords t => Double -> Vec3 -> t -> t
-- rotateAngleAxis = (.) transformCoords  . rotate3


data Visibility = Invisible | OnlyLabels | Visible
    deriving (Eq,Ord,Show)

instance Pretty Visibility where prettyPrec = prettyPrecFromShow

newtype FaceName = FaceName { unFaceName :: String }
    deriving (Show,IsString,Pretty)

mkFaceName :: String -> FaceName
mkFaceName = FaceName

data PreRenderable s = PreRenderable {
    pr_ds :: s,
    -- | Overriden ('pr_coords') by the GeneralTriangleImmersion in pr_triangleInfo, if it exists for at least one triangle containing the vertex
    pr_coords0 :: Vert s -> Vec3,
    pr_faceInfo :: AnySimplex2Of s -> (Visibility,FaceName), 
    pr_triangleInfo :: Tri s -> (Maybe TriangleLabel, Maybe GeneralTriangleImmersion),
    pr_edgeInfo :: Ed s -> (Maybe EdgeDeco, Maybe GeneralEdgeImmersion)
}
    deriving(Generic)

nameMakeLens ''PreRenderable (Just . (++"L"))


pr_quad :: Resolution-> FF Tup2 Tup3 Double -> PreRenderable (SC2 (Bool, Bool))
pr_quad reso (f :: FF Tup2 Tup3 Double) = 
    PreRenderable {
        pr_ds = ds,
        pr_coords0 = const (assert False undefined),
        pr_faceInfo = \asi -> ( if asi == edToAnySimplex2 _diag then Invisible else Visible
                              , mkFaceName $ shortShow asi),

        pr_triangleInfo = 
            \t -> 
                (Nothing,
                 
                    let pretransform :: FF Tup2 Tup2 Double 
                        pretransform =
                            if elemAsc3 ff t 
                                then id
                                else \(Tup2 (u, v)) -> Tup2 (u+v, 1-v)

                    in (Just . GTE reso) (f . pretransform)
                    )

            
            
                ,

        pr_edgeInfo = const (assert False undefined)

    }

  where
    ff = (False,False)
    ft = (False,True)
    tf = (True,False)
    tt = (True,True)

    ds = fromTris [(ff,ft,tf),(ft,tf,tt)] 
    _diag = asc2 (ft,tf)



instance Coords (PreRenderable a) where
    transformCoords f =
            modL pr_coords0L (\coords0 -> tup3toVec3 . lowerFF f . vec3toTup3 . coords0)
        .   modL pr_generalEdgeImmersionL (fmap (transformCoords f) .)
        .   modL (pr_triangleInfoL >>> secondLens) (fmap (transformCoords f) .)

instance 
    (   DisjointUnionable s1 s2 s inj1 inj2 ei
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (PreRenderable s1) (PreRenderable s2) (PreRenderable s) inj1 inj2 ei where

    disjointUnionWithInjs = fmap djZapUnits . defaultDisjointUnion




mkPreRenderable
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>
     (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderable = mkPreRenderableWithTriangleLabels (const Nothing)

mkPreRenderableWithTriangleLabels
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>

     (Tri s -> Maybe TriangleLabel)
     -> (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderableWithTriangleLabels triangleLabels pr_coords_ pr_ds_ = 
    PreRenderable {
        pr_ds = pr_ds_,
        pr_coords0 = pr_coords_, 
        pr_triangleInfo = triangleLabels &&& const Nothing,
        pr_faceInfo = const Visible &&& (mkFaceName . shortShow),
        pr_edgeInfo = const (Nothing,Nothing)
    }

instance OneSkeletonable s => OneSkeletonable (PreRenderable s) where
    oneSkeleton _pr = _pr { pr_ds = oneSkeleton (pr_ds _pr) }


--type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

--tet3d :: PreRenderableSimplicialComplex Vertex
tet3d :: PreRenderable (SC3 Vertex)
tet3d = mkPreRenderable vertexDefaultCoords abstractTet 


pr_mapDs
  :: (Vert t ~ Vert s,
      Ed t ~ Ed s,
      Tri t ~ Tri s) =>
     (t -> s) -> PreRenderable t -> PreRenderable s
pr_mapDs f (PreRenderable a b c d e) = PreRenderable (f a) b c d e

pr_popDimension :: PreRenderable (SC3 v) -> PreRenderable (SC2 v)
pr_popDimension = pr_mapDs sccons_skeleton

pr_faceName :: PreRenderable s -> AnySimplex2Of s -> FaceName
pr_faceName = (fmap . fmap) snd pr_faceInfo

pr_visibilityL :: Lens (PreRenderable s) (AnySimplex2Of s -> Visibility)
pr_visibilityL = pr_faceInfoL >>> firstLens

pr_setVisibility :: (AnySimplex2Of s -> Visibility) -> PreRenderable s -> PreRenderable s
pr_setVisibility = setL pr_visibilityL

pr_triVisibilityL :: Lens (PreRenderable s) (Element (Tris s) -> Visibility)
pr_triVisibilityL = pr_visibilityL >>> rightLens

pr_edVisibilityL :: Lens (PreRenderable s) (Ed s -> Visibility)
pr_edVisibilityL = pr_visibilityL >>> leftLens >>> rightLens

pr_vertVisibilityL :: Lens (PreRenderable s) (Element (Verts s) -> Visibility)
pr_vertVisibilityL = pr_visibilityL >>> leftLens >>> leftLens

pr_setEdVisibility
  :: (Ed s -> Visibility) -> PreRenderable s -> PreRenderable s
pr_setEdVisibility = setL pr_edVisibilityL

pr_setTriVisibility :: (Element (Tris s) -> Visibility)-> PreRenderable s -> PreRenderable s
pr_setTriVisibility = setL pr_triVisibilityL

pr_vertVisibility :: PreRenderable s -> Element (Verts s) -> Visibility
pr_vertVisibility = getL pr_vertVisibilityL 
pr_edVisibility ::  PreRenderable s -> Ed s -> Visibility
pr_edVisibility = getL pr_edVisibilityL
pr_triVisibility :: PreRenderable s -> Element (Tris s) -> Visibility
pr_triVisibility = getL pr_triVisibilityL


pr_triangleLabelL
  :: Lens (PreRenderable s) (Tri s -> Maybe TriangleLabel)
pr_triangleLabelL = pr_triangleInfoL >>> firstLens

pr_triangleLabel :: PreRenderable s -> Tri s -> Maybe TriangleLabel
pr_triangleLabel = getL pr_triangleLabelL


pr_generalTriangleImmersionL
  :: Lens (PreRenderable s) (Tri s -> Maybe GeneralTriangleImmersion)
pr_generalTriangleImmersionL = pr_triangleInfoL >>> secondLens

{-# DEPRECATED pr_setTriangleImmersion "use pr_setGeneralTriangleImmersion" #-}
pr_setTriangleImmersion
  :: (Tri s -> Maybe GeneralTriangleImmersion)
     -> PreRenderable s -> PreRenderable s
pr_setTriangleImmersion = pr_setGeneralTriangleImmersion

pr_setGeneralTriangleImmersion :: (Tri s -> Maybe GeneralTriangleImmersion)-> PreRenderable s -> PreRenderable s
pr_setGeneralTriangleImmersion = setL pr_generalTriangleImmersionL

pr_edgeDecoL ::  Lens (PreRenderable s) (Ed s -> Maybe EdgeDeco)
pr_edgeDecoL = pr_edgeInfoL >>> firstLens
pr_generalEdgeImmersionL :: Lens (PreRenderable s) (Ed s -> Maybe GeneralEdgeImmersion)
pr_generalEdgeImmersionL = pr_edgeInfoL >>> secondLens

pr_setEdgeDecoAssocs :: Ord (Element (Eds s)) =>[(Ed s, EdgeDeco)] -> PreRenderable s -> PreRenderable s
pr_setEdgeDecoAssocs = setL pr_edgeDecoL . flip M.lookup . M.fromList 

triResolutionToEdgeResolution ::  Num a => a -> a
triResolutionToEdgeResolution = (*10)

pr_edgeImmersion
  :: (PreDeltaSet2 s,
      Show (Ed s),
      Show (Tri s)) =>
     PreRenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> Ed s
     -> EdgeImmersion
pr_edgeImmersion pr tcec e =
            case findJust (\(triangle,i) -> do 
                        emb <- getL pr_generalTriangleImmersionL pr triangle
                        return (triangle,i,emb))
                 . lookupTrianglesContainingEdge tcec
                 $ e of 

                 Nothing -> 
                    case getL pr_generalEdgeImmersionL pr e of
                        Just gee -> GeneralEdge gee 
                        Nothing -> flatFace pr (uncurry FlatEdge) map2 vertices e 

                 -- edge is contained in some curved triangle
                 Just (_, i, GTE res emb) -> 
--                     trace ("Edge "++show e++" has index "++show i++" in "++show t) $

                    GeneralEdge
                        (GEE
                            (triResolutionToEdgeResolution res)
                            (emb . pretransform))

                  where
                    pretransform :: UF Tup2 Double
                    pretransform = 
                        let (v0,v1) = 
                                case i of
                                     I3_0 -> (pure 0,tup2X)
                                     I3_1 -> (pure 0,tup2Y)
                                     I3_2 -> (tup2X,tup2Y)

                        in \x -> interpol x v0 v1

flatFace :: PreRenderable s-> (b -> c) -> ((Vert s -> Vec3) -> b1 -> b) -> (a -> b1) -> a -> c
flatFace pr ctor mapN verticesAscending face =    
                                ctor
                            .   mapN (pr_coords0 pr)
                            .   verticesAscending
                            $   face
                            

pr_triangleImmersion :: PreDeltaSet2 s => PreRenderable s -> Tri s -> TriangleImmersion
pr_triangleImmersion pr t = 
    case getL pr_generalTriangleImmersionL pr t of
         Nothing -> flatFace pr (uncurry3 FlatTriangle) map3 vertices t

         Just emb -> GeneralTriangle emb

-- | See also: 'pr_coords0'
pr_coords
  :: (PreDeltaSet2 s,
      Show (Vert s),
      Show (Ed s),
      Show (Tri s)) =>
     PreRenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> EdgesContainingVertex_Cache (Vert s) (Ed s)
     -> Vert s
     -> Vec3
pr_coords pr tcec ecvc v = 

    (
            findJust (\(e,i) -> 
                    case pr_edgeImmersion pr tcec e of
                         FlatEdge {} -> Nothing
                         GeneralEdge (GEE _ emb) ->
--                             trace ("Vertex "++show v++" has index "++show i++" in "++show e) $
                            Just (tup3toVec3 $ lowerUF emb (case i of
                                            I2_0 -> 0
                                            I2_1 -> 1)))

                 . lookupEdgesContainingVertex ecvc
                 $ v 
    )
    `orElse`
    pr_coords0 pr v



instance (Pretty s, Pretty (Vert s), Pretty (Ed s), Pretty (Tri s)
            , Vertices s, Edges s, Triangles s) => 
    Pretty (PreRenderable s) where


    pretty PreRenderable{..} = 
        prettyRecord "PreRenderable"
            [ ("pr_ds",pretty pr_ds)
            , ("pr_coords0",prettyFunction pr_coords0 (vertices pr_ds))
            , ("pr_faceInfo",prettyFunction pr_faceInfo (anySimplex2s pr_ds))
            , ("pr_triangleInfo",prettyFunction 
                    (second (fmap (const "<<function>>")) . pr_triangleInfo) (triangles pr_ds))
            ]


pr_visibility :: PreRenderable s -> AnySimplex2Of s -> Visibility
pr_visibility = getL pr_visibilityL



-- instance Lift TriangleImmersion where
--     lift (FlatTriangle a b c) = [| FlatTriangle a b c |]
-- 
-- instance Lift EdgeImmersion where
--     lift (FlatEdge a b) = [| FlatEdge a b |]

-- | error instance
instance Lift GeneralTriangleImmersion where
    lift _ = [| error "'lift' not supported for GeneralTriangleImmersion" |]

-- | error instance
instance Lift GeneralEdgeImmersion where
    lift _ = [| error "'lift' not supported for GeneralEdgeImmersion" |]

instance Lift FaceName where
    lift (unFaceName -> x) = [| mkFaceName x |] 

deriveLiftMany [''Visibility,''TriangleImmersion,''EdgeImmersion,''EdgeDeco]

instance (PreDeltaSet2 s, Lift s, Ord (Vert s), Ord (Ed s), Ord (Tri s)
            , Lift (Vert s), Lift (Ed s), Lift (Tri s)) => 

    Lift (PreRenderable s) where

    lift PreRenderable{..} = 
        [| PreRenderable {
                pr_ds = pr_ds,
                pr_coords0 = $(liftFunction (vertexList pr_ds) pr_coords0),
                pr_faceInfo = $(liftFunction (anySimplex2s pr_ds) pr_faceInfo),
                pr_triangleInfo = $(liftFunction (triangleList pr_ds) pr_triangleInfo),
                pr_edgeInfo = $(liftFunction (edgeList pr_ds) pr_edgeInfo)
        } |]


-- pr_ghide
--   :: ((t1 -> Visibility) -> PreRenderable s -> t)
--      -> (t1 -> AnySimplex2Of s) -> (t1 -> Bool) -> PreRenderable s -> t
-- pr_ghide _setVisibility f p pr =
--     _setVisibility (\asi -> visibleIf (not (p asi)) * pr_visibility pr (f asi)) pr

pr_ghide :: Lens a (t -> Visibility) -> (t -> Bool) -> a -> a
pr_ghide _lens p = modL _lens (\oldVi -> \x -> visibleIf (not (p x)) `min` oldVi x) 

pr_hide
  :: (AnySimplex2Of s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hide = pr_ghide pr_visibilityL


--pr_hideVerts = pr_ghide pr_setVertVisibility vertToAnySimplex2

pr_hideEds :: (Ed s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hideEds = pr_ghide pr_edVisibilityL

pr_hideTris
  :: (Tri s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hideTris = pr_ghide pr_triVisibilityL

visibleIf ::  Bool -> Visibility
visibleIf b = if b then Visible else Invisible



isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t| (Bool,Bool) |]

-- | Doesn't work on edges yet
pr_makeImmersionsGeneral
  :: PreDeltaSet2 s =>
     (Tri s -> Resolution) -> PreRenderable s -> PreRenderable s
pr_makeImmersionsGeneral triRes pr_ =
    setL pr_generalTriangleImmersionL
        (\t -> Just $ case pr_triangleImmersion pr_ t of
                    GeneralTriangle emb -> emb
                    FlatTriangle a au av ->
                        GTE 
                            (triRes t)
                            (evalFlatTriangleImmersion a au av))

 $       pr_


evalFlatTriangleImmersion
  :: Vec3 -> Vec3 -> Vec3 -> FF Tup2 Tup3 Double
evalFlatTriangleImmersion a au av =
                            (\(Tup2 (u, v) :: Tup2 (AD s Double)) -> 
                                ((1-u-v) *^ liftVec3 a) 
                                ^+^ 
                                (u *^ liftVec3 au) 
                                ^+^ 
                                (v *^ liftVec3 av)

                                :: Tup3 (AD s Double)
                                )

evalEdgeImmersion :: EdgeImmersion -> UF Tup3 Double
evalEdgeImmersion (FlatEdge a0 a1) = 
    \u -> 
        let 
            res = interpol u (liftVec3 a0) (liftVec3 a1) 
        in $(assrt [| allReal res |] ['u, 'a0, 'a1]) res

evalEdgeImmersion (GeneralEdge (GEE _ f)) = f

data GeneralTetImmersion = GTetE Resolution (FF Tup3 Tup3 Double)

mkPreRenderableFromTetImmersions
  :: forall s. (Ord (Tri s), ShortShow (Vert s), ShortShow (Tri s), ShortShow (Ed s), PreDeltaSet3 s) =>
     (Tet s -> Maybe GeneralTetImmersion)
     -> (Vert s -> Vec3) -- ^ Vertex coordinates to use for vertices which are in a tetrahedron for which the first argument returns 'Nothing' (or vertices which aren't in any tetrahedron). 
     -> s 
     -> PreRenderable s
mkPreRenderableFromTetImmersions (getTetImm :: Tet s -> Maybe GeneralTetImmersion) coords s =
    setL pr_generalTriangleImmersionL 
        (\t -> do
            (GTetE res tetImm,i) <- 
                listToMaybe 
                    (do
                        (tet,i) <- lookupTetsContainingTri tctc t
                        tetImm <- maybeToList (getTetImm tet)
                        [ (tetImm,i) ])

            let pretransform :: FF Tup3 Tup4 Double 
                pretransform (Tup3 (u,v,w)) =
                    case i of
                         I4_0 -> Tup4 (u,v,w,0)
                         I4_1 -> Tup4 (u,v,0,w)
                         I4_2 -> Tup4 (u,0,v,w)
                         I4_3 -> Tup4 (0,u,v,w)

            Just (GTE res (tetImm . stdToUnit3 . pretransform . unitToStd2))
        )
    
        (mkPreRenderable coords s)


    where
        tctc = mkTCTC s



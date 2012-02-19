{-# LANGUAGE TemplateHaskell #-}
import ConcreteNormal.PreRenderable
import R3Immersions
import CheckAdmissibility
import Blender
import Triangulation
import ConcreteNormal
import NormalSurfaceBasic
import PreRenderable
import Numeric.AD.Vector
import DisjointUnion
import FileLocation
import Simplicial.DeltaSet2
import TriangulationCxtObject
import PrettyUtil
import VerboseDD
import qualified Data.Vector.Generic as VG


ba = mkBlenderable normalSurfaceStyle pr_ 
     `disjointUnion`
     pr_tr

pr_tr =
     mkBlenderable 
        pseudomanifoldStyle 
        (pr_mapDs gds2 $ 
            mkPreRenderableFromTetImmersions getTetImm tr)


tr = mkTriangulation 1 [gluing (0./tACD) (0./oBCD)] 


sols = fundEdgeSolutions tr

cns :: ConcreteNSurface
cns = toConcrete (sols VG.! 3)

getTetImm = const (GTetE (text"std") 60 (standardSnapped3Ball . unitToStd3))

pr_ = prnsFromTetImmersions getTetImm (gds2 cns)

scene
  :: Scene
       (GenericDeltaSet2
          (DJSimp DIM2 TNmTriOrQuadHalf TTriangle))
scene = setCam cam . defaultScene $ disableHelpLines ba

cam = readCam "(Vector((1.573462963104248, 2.0371408462524414, 0.5036145448684692)), Euler((1.404733419418335, 1.1177638043591287e-05, 2.4831230640411377), 'XYZ'), 0.8575560591178853)"


main = testBlender scene

{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
import HsTri
import qualified Data.Map as M
import Data.Map(Map)
import Control.Monad
import Control.Category((>>>))
import qualified Data.List as L

tr_before = mkTriangulation 3
        [ gluing (0./tACD) (1./oACD)
        , gluing (1./tABD) (2./oABC)
        ]

tr_after = mkTriangulation 4 
    (tGluingsIrredundant tr_before ++ 
        [ gluing (3./tABC) (1./oABC)
        , gluing (3./tABD) (2./oABD)
        ])


p_before = pMap tr_before
p_after  = pMap tr_after

coords = flip $indxShow . M.fromList $ [
            (p_before (0./vD), co Bottom),
            (p_before (0./vB), co (EquatorVertex 0)),
            (p_before (0./vA), co (EquatorVertex 1)),
            (p_before (2./vD), co (EquatorVertex 2)),
            (p_before (1./vB), co (EquatorVertex 3)),
            (p_before (0./vC), co (EquatorVertex 4))

            ]

co = edgeNeighborhoodVertexCoords 1 5

-- | Must map 0 to 0
heightFunction = 
    (1.5 *) 
    --(0.8*) . sqrt

reso = 100

imm3 = GTetE reso (unitToStd3 >>>
    (\(Tup4 (a,b,c,d)) ->

        let
            r0 = sumV [ p_before *^ liftVec3 (coords v)
                            
                            | (p_before,v) <- zip 
                                        [a,b,c,d] 
                                        [ p_before (1./vA), p_before (1./vB)
                                        , p_before (1./vC), p_before (2./vD) ] ] 


        in

            r0 ^+^ tup3Z ^* heightFunction(c*d) 

        

        ))
    
before = mkPreRenderable coords tr_before
after = 
    mkPreRenderableFromTetImmersions 
    (\i -> guard (i==p_after 3) >> return imm3)
    (\v -> case L.find ((<3) . getTIndex) (preimageListOfVertex v) of 
                Nothing -> error ("Tried to get coords of "++show v)
                Just vsnake -> coords . p_before $ vsnake)
    tr_after 

go fn = blenderMain DoRender 
     . setCam cam
     . setRenderRes 1920 1200
     . setRenderFilepath fn
     . defaultScene
     . disableHelpLines
     . mkBlenderable pseudomanifoldStyle

cam =  readCam "(Vector((-2.308177947998047, -0.1556624323129654, 0.8290101885795593)), Euler((1.1477700471878052, -5.8432365221960936e-06, -1.5118657350540161), 'XYZ'), 0.8575560591178853)"

main = do
    go "/h/dipl/pictures/layeringBefore.png" before 
    go "/h/dipl/pictures/layeringAfter.png" after



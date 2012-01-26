module Triangulation.Random where

import {-# SOURCE #-} Triangulation
import Test.QuickCheck
    
arbitraryTriangulation :: Gen Triangulation
shrinkTriangulation :: Triangulation -> [ Triangulation ]

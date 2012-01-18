{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Triangulation.Class
    (module Triangulation,
     ToTriangulation(..)
    )

    where

import Triangulation

class ToTriangulation t where
    toTriangulation :: t -> Triangulation 

instance ToTriangulation Triangulation where
    toTriangulation = id

instance ToTriangulation LabelledTriangulation where
    toTriangulation = snd

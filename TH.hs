{-# LANGUAGE TemplateHaskell #-}
module TH where
import Language.Haskell.TH

atType ::  TypeQ -> ExpQ
atType t = [| \f -> f (undefined :: $(t)) |]

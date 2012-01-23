{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
import ZapUnits

f :: ((((),Int),((),())),()) -> Int
f = zapUnits


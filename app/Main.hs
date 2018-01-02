{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Diagrams.RubiksCube
import qualified Diagrams.RubiksCube.Move as RubiksCube.Move
import qualified Diagrams.RubiksCube.Model
import qualified Data.Colour.RGBSpace
import qualified Data.Colour.SRGB

import Control.Lens hiding ((#))
import qualified Diagrams.Prelude

import qualified Diagrams.Backend.SVG


standardRubiksCube :: Diagrams.RubiksCube.Model.RubiksCube (Data.Colour.RGBSpace.Colour Double)
standardRubiksCube = Diagrams.RubiksCube.Model.RubiksCube (Diagrams.RubiksCube.Model.Cube f b l r u d)
  where
    -- colours from http://clrs.cc/
    f = pure (Data.Colour.SRGB.sRGB24 61 153 112)  -- olive
    b = pure (Data.Colour.SRGB.sRGB24 0 116 217)   -- blue
    l = pure (Data.Colour.SRGB.sRGB24 255 133 27)  -- orange
    r = pure (Data.Colour.SRGB.sRGB24 255 65 54)   -- red
    u = pure (Data.Colour.SRGB.sRGB24 255 255 255) -- white
    d = pure (Data.Colour.SRGB.sRGB24 255 220 0)   -- yellow


main :: IO ()
main = do
  let cube :: Diagrams.RubiksCube.Model.RubiksCube (Data.Colour.RGBSpace.Colour Double)
      cube    = standardRubiksCube ^. Diagrams.RubiksCube.Model.undoMoves [RubiksCube.Move.R, RubiksCube.Move.U, RubiksCube.Move.R', RubiksCube.Move.U']
      tPermMoves :: [RubiksCube.Move.Move]
      tPermMoves =
        let
          r  = RubiksCube.Move.R
          u  = RubiksCube.Move.U
          r' = RubiksCube.Move.R'
          u' = RubiksCube.Move.U'
          f  = RubiksCube.Move.F
          f' = RubiksCube.Move.F'
        in  [r, u, r', u', r', f, r, r, u', r', u', r, u, r', f']
      settings :: Diagrams.RubiksCube.MovesSettings Double
      settings = (Diagrams.Prelude.with & Diagrams.RubiksCube.showStart .~ True)
      d :: Diagrams.Prelude.Diagram (Diagrams.Backend.SVG.B)
      d   = Diagrams.RubiksCube.drawRubiksCube Diagrams.Prelude.def standardRubiksCube
      tPermedD = Diagrams.RubiksCube.drawMoves Diagrams.Prelude.def standardRubiksCube tPermMoves
  Diagrams.Backend.SVG.renderSVG "demo_images/demo1.svg" (Diagrams.Prelude.mkWidth 100) d
  Diagrams.Backend.SVG.renderSVG "demo_images/t_perm.svg" (Diagrams.Prelude.mkWidth 900) tPermedD
  return ()
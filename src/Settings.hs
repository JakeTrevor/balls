{-# LANGUAGE LambdaCase #-}

module Settings
  ( Boundaries (..),
    box,
    threeSides,
    Setting (..),
    idealGas,
    ballistics,
    settingsParser,
  )
where

import Control.Applicative ((<**>))
import GHC.Base (divInt)
import Graphics.Gloss (Point)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper, info, long, option, progDesc, short, showDefault, value)

_fps :: Int
_fps = 120

data Boundaries = MkBoundaries {top :: Maybe Float, right :: Maybe Float, bottom :: Maybe Float, left :: Maybe Float}

data Setting = MkSetting {boundaries :: Boundaries, g :: Float, decay :: Float, fps :: Int}

getLimits :: IO Point
getLimits = do
  (w, h) <- getScreenSize
  let hLimits = fromIntegral (divInt h 2) - 10
  let wLimits = fromIntegral (divInt w 2) - 10
  return (wLimits, hLimits)

-- boundary pre-sets
box :: IO Boundaries
box = do
  (w, h) <- getLimits
  return $ MkBoundaries (Just h) (Just w) (Just (-h)) (Just (-w))

threeSides :: IO Boundaries
threeSides = do
  (w, h) <- getLimits
  return $ MkBoundaries Nothing (Just w) (Just (-h)) (Just (-w))

-- overall presets
idealGas :: IO Setting
idealGas = do
  bounds <- box
  return $ MkSetting {boundaries = bounds, g = 0, decay = 1, fps = _fps}

ballistics :: IO Setting
ballistics = do
  bounds <- threeSides
  return $ MkSetting {boundaries = bounds, g = 9.81 / fromIntegral _fps, decay = 0.8, fps = _fps}

-- parsing
data Preset = IdealGas | Ballistics
  deriving (Read, Show)

presetParser :: Parser (IO Setting)
presetParser =
  ( \case
      Ballistics -> ballistics
      IdealGas -> idealGas
  )
    <$> option
      auto
      ( long "Preset"
          <> short 'p'
          <> help "Use a preset (either Ballistics or IdealGas)"
          <> value Ballistics
          <> showDefault
      )

settingsParser :: ParserInfo (IO Setting)
settingsParser = info (presetParser <**> helper) (fullDesc <> progDesc "Simulate Your Balls")
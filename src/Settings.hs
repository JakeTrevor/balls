{-# LANGUAGE LambdaCase #-}

module Settings
  ( Boundaries (..),
    Setting (..),
    idealGas,
    ballistics,
    settingsParser,
  )
where

import Control.Applicative (Alternative ((<|>)), (<**>))
import Data.Functor ((<&>))
import GHC.Base (divInt)
import Graphics.Gloss (Point)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper, info, long, option, progDesc, short, showDefault, switch, value)

_fps :: Int
_fps = 120

data Boundaries = MkBoundaries {top :: Maybe Float, right :: Maybe Float, bottom :: Maybe Float, left :: Maybe Float}

data Setting = MkSetting {boundaries :: Boundaries, g :: Float, decay :: Float, fps :: Int, colours :: Bool, i :: Maybe Float}

mkIOSetting :: IO Boundaries -> Float -> Float -> Int -> Bool -> Maybe Float -> IO Setting
mkIOSetting ioBounds g decay fps cols i = do
  bounds <- ioBounds
  return $ MkSetting bounds g decay fps cols i

getLimits :: IO Point
getLimits = do
  (w, h) <- getScreenSize
  let hLimits = fromIntegral (divInt h 2)
  let wLimits = fromIntegral (divInt w 2)
  return (wLimits, hLimits)

mkBounds :: Boundary -> IO Boundaries
mkBounds Box = mkBounds $ Custom (True, True, True, True)
mkBounds ThreeSides = mkBounds $ Custom (False, True, True, True)
mkBounds None = mkBounds $ Custom (False, False, False, False)
mkBounds (Custom (t, r, b, l)) = do
  (w, h) <- getLimits
  return $ MkBoundaries {top = check t h, right = check r w, bottom = check b (-h), left = check l (-w)}
  where
    check cond v = if cond then Just v else Nothing

-- overall presets
idealGas :: IO Setting
idealGas = do
  bounds <- mkBounds Box
  return $ MkSetting {boundaries = bounds, g = 0, decay = 1, fps = _fps, colours = True, i = Nothing}

ballistics :: IO Setting
ballistics = do
  bounds <- mkBounds ThreeSides
  return $ MkSetting {boundaries = bounds, g = 9.81 / fromIntegral _fps, decay = 0.8, fps = _fps, colours = False, i = Nothing}

orbital :: IO Setting
orbital = do
  bounds <- mkBounds None
  return $ MkSetting {boundaries = bounds, g = 0 / fromIntegral _fps, decay = 0, fps = _fps, colours = False, i = Just (6.67 * (10 ** (-2)))}

-- parsing

data Boundary = Box | ThreeSides | None | Custom (Bool, Bool, Bool, Bool)
  deriving (Show)

instance Read Boundary where
  readsPrec _ "Box" = [(Box, "")]
  readsPrec _ "ThreeSides" = [(ThreeSides, "")]
  readsPrec _ "None" = [(None, "")]
  readsPrec _ l@[_, _, _, _] =
    let a = map ('x' ==) l
        (w, x, y, z) = case a of
          [w', x', y', z'] -> (w', x', y', z')
          _ -> error "impossible"
     in [(Custom (w, x, y, z), "")]
  readsPrec _ _ = []

boundaryParser :: Parser (IO Boundaries)
boundaryParser =
  mkBounds
    <$> option
      auto
      ( long "boundary"
          <> short 'b'
          <> help "which sides should be boundaries; use a preset (Box, ThreeSides) or specify a pattern (see readme for details)"
          <> value ThreeSides
          <> showDefault
      )

gParser :: Parser Float
gParser =
  (/ fromIntegral _fps)
    <$> option
      auto
      ( long "gravity"
          <> short 'g'
          <> help "force due to gravity"
          <> value 9.81
          <> showDefault
      )

decayParser :: Parser Float
decayParser =
  option
    auto
    ( long "decay"
        <> short 'd'
        <> help "Energy loss; velocity after collision with the floor will be multiplied by this"
        <> value 1
        <> showDefault
    )

coloursParser :: Parser Bool
coloursParser =
  switch
    ( long "Colours"
        <> short 'c'
        <> help "Map velocity to a colour"
    )

iParser :: Parser (Maybe Float)
iParser =
  (<&> (/ fromIntegral _fps))
    <$> option
      auto
      ( long "Inter-particle force"
          <> short 'g'
          <> help "Attraction between particles; G in GMm/r^2"
          <> value Nothing
          <> showDefault
      )

customSettingParser :: Parser (IO Setting)
customSettingParser =
  mkIOSetting
    <$> boundaryParser
    <*> gParser
    <*> decayParser
    <*> pure _fps
    <*> coloursParser
    <*> iParser

data Preset = IdealGas | Ballistics | Orbital
  deriving (Read, Show)

presetParser :: Parser (IO Setting)
presetParser =
  ( \case
      Ballistics -> ballistics
      IdealGas -> idealGas
      Orbital -> orbital
  )
    <$> option
      auto
      ( long "Preset"
          <> short 'p'
          <> help "Use a preset (either Ballistics or IdealGas)"
          <> value Ballistics
          <> showDefault
      )

sParser :: Parser (IO Setting)
sParser = presetParser <|> customSettingParser

settingsParser :: ParserInfo (IO Setting)
settingsParser = info (sParser <**> helper) (fullDesc <> progDesc "Simulate Balls with Physics")
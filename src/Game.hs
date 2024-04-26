module Game
  ( playBall,
  )
where

import Ball
import Control.Monad.Reader (runReader)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Options.Applicative (execParser)
import Settings (Setting (..), fps, settingsParser)
import Vectors

data World = MkWorld {slingshot :: Maybe Point, mouse :: Point, worldBalls :: [Ball]}

handle :: Event -> World -> IO World
handle (EventKey (MouseButton LeftButton) Down _ coords) world = do
  let balls = MkBall coords (0, 0) : worldBalls world
  return $ world {worldBalls = balls}
handle (EventMotion coords) world = return $ world {mouse = coords}
handle (EventKey (MouseButton RightButton) Down _ coords) world = return $ world {slingshot = Just coords, mouse = coords}
handle (EventKey (MouseButton RightButton) Up _ _) world@(MkWorld {slingshot = Nothing}) = return world
handle (EventKey (MouseButton RightButton) Up _ p1) world@(MkWorld {slingshot = Just p2}) = do
  let v = scalarMul (vectorSub p2 p1) 0.1
  let balls = MkBall p1 v : worldBalls world
  return $ world {slingshot = Nothing, worldBalls = balls}
handle _ x = return x

render :: Setting -> World -> IO Picture
render (MkSetting {colours = cols}) (MkWorld {worldBalls = balls, slingshot = s, mouse = m}) =
  let ballPics = mconcat $ map (renderBall cols) balls
      arrow = case s of
        Just x -> Color red $ line [x, m]
        _ -> blank
   in return $ ballPics <> arrow

renderBall :: Bool -> Ball -> Picture
renderBall cols (MkBall (x, y) v _) = translate x y $ Color c $ circleSolid 10
  where
    c = if cols then vecAsColour v else black

vecAsColour :: Point -> Color
vecAsColour v =
  let value = magnitude v / 25
   in mixColors value (1 - value) red blue

-- step

step :: Setting -> Float -> World -> IO World
step setting _t world = do
  let balls' = runReader (updateBalls (worldBalls world)) setting
  return $ world {worldBalls = balls'}

startingWorld :: World
startingWorld = (MkWorld {slingshot = Nothing, mouse = (0, 0), worldBalls = []})

playBall :: IO ()
playBall = do
  factory <- execParser settingsParser
  setting <- factory
  playIO FullScreen white (fps setting) startingWorld (render setting) handle (step setting)
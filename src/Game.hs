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

data World = MkWorld {slingshot :: Maybe Point, mouse :: Point, worldBalls :: [Ball], newSize :: Float, pause :: Bool, pictureScale :: Float}

scaleConst :: Float
scaleConst = 1.05

handle :: Event -> World -> IO World
-- zoom
handle (EventKey (MouseButton WheelUp) Down (Modifiers {ctrl = Down}) _) world@(MkWorld {pictureScale = worldScale, mouse = m}) = return $ world {pictureScale = worldScale / scaleConst, mouse = scalarMul m scaleConst}
handle (EventKey (MouseButton WheelDown) Down (Modifiers {ctrl = Down}) _) world@(MkWorld {pictureScale = worldScale, mouse = m}) = return $ world {pictureScale = worldScale * scaleConst, mouse = scalarMul m (1 / scaleConst)}
-- pause
handle (EventKey (Char 'p') Down _ _) world@(MkWorld {pause = p}) = return $ world {pause = not p}
-- change ball size
handle (EventKey (Char 'e') Down _ _) world@(MkWorld {newSize = sz}) = return $ world {newSize = sz + 1}
handle (EventKey (Char 'q') Down _ _) world@(MkWorld {newSize = sz}) = return $ world {newSize = max (sz - 1) 1}
handle (EventKey (MouseButton WheelUp) Down _ _) world@(MkWorld {newSize = sz}) = return $ world {newSize = sz + 1}
handle (EventKey (MouseButton WheelDown) Down _ _) world@(MkWorld {newSize = sz}) = return $ world {newSize = max (sz - 1) 1}
-- place ball
handle (EventKey (MouseButton LeftButton) Down _ _) world@(MkWorld {mouse = coords}) = do
  let balls = MkBall coords (0, 0) (newSize world) : worldBalls world
  return $ world {worldBalls = balls}
-- move mouse
handle (EventMotion coords) world@(MkWorld {pictureScale = worldScale}) = return $ world {mouse = scalarMul coords (1 / worldScale)}
-- slingshot
handle (EventKey (MouseButton RightButton) Down _ _) world@(MkWorld {mouse = coords}) = return $ world {slingshot = Just coords}
handle (EventKey (MouseButton RightButton) Up _ _) world@(MkWorld {slingshot = Nothing}) = return world
handle (EventKey (MouseButton RightButton) Up _ _) world@(MkWorld {slingshot = Just p2, mouse = p1}) = do
  let v = scalarMul (vectorSub p2 p1) 0.1
  let balls = MkBall p1 v (newSize world) : worldBalls world
  return $ world {slingshot = Nothing, worldBalls = balls}
handle _ x = return x

render :: Setting -> World -> IO Picture
render (MkSetting {colours = cols}) (MkWorld {worldBalls = balls, slingshot = s, mouse = m@(x, y), newSize = sz, pictureScale = worldScale}) = do
  let ballPics = mconcat $ map (renderBall cols) balls
      arrow = case s of
        Just p -> Color red $ line [p, m]
        _ -> blank
      mouseBall = translate x y $ circle sz
   in return $ scale worldScale worldScale $ ballPics <> arrow <> mouseBall

renderBall :: Bool -> Ball -> Picture
renderBall cols (MkBall (x, y) v sz) = translate x y $ Color c $ circleSolid sz
  where
    c = if cols then vecAsColour v else black

vecAsColour :: Point -> Color
vecAsColour v =
  let value = magnitude v / 25
   in mixColors value (1 - value) red blue

-- step

step :: Setting -> Float -> World -> IO World
step _ _t world@(MkWorld {pause = True}) = return world
step setting _t world = do
  let balls' = runReader (updateBalls (worldBalls world)) setting
  return $ world {worldBalls = balls'}

startingWorld :: World
startingWorld = (MkWorld {slingshot = Nothing, mouse = (0, 0), worldBalls = [], newSize = 10, pause = False, pictureScale = 1})

playBall :: IO ()
playBall = do
  factory <- execParser settingsParser
  setting <- factory
  playIO FullScreen white (fps setting) startingWorld (render setting) handle (step setting)

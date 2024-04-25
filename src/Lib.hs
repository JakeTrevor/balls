module Lib
  ( game,
  )
where

import GHC.Base
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game
import Vectors
import Prelude hiding (foldr)

-- Constants of Nature
fps :: Int
fps = 120

g :: Float
g = 9.81 / fromIntegral fps

decay :: Float
decay = 0.8

--

data Ball = MkBall {pos :: Point, vel :: Point} -- size::float

data World = MkWorld {slingshot :: Maybe Point, mouse :: Point, worldBalls :: [Ball]}

handle :: Event -> World -> IO World
handle (EventKey (MouseButton LeftButton) Down _ coords) world = do
  let balls = MkBall coords (0, 0) : worldBalls world
  return $ world {worldBalls = balls}

-- slingshot
handle (EventMotion coords) world@(MkWorld {slingshot = Just _}) = return $ world {mouse = coords}
handle (EventKey (MouseButton RightButton) Down _ coords) world = return $ world {slingshot = Just coords, mouse = coords}
handle (EventKey (MouseButton RightButton) Up _ _) world@(MkWorld {slingshot = Nothing}) = return world
handle (EventKey (MouseButton RightButton) Up _ p1) world@(MkWorld {slingshot = Just p2}) = do
  let v = scalarMul (vectorSub p2 p1) 0.1
  let balls = MkBall p1 v : worldBalls world
  return $ world {slingshot = Nothing, worldBalls = balls}
-- any other case
handle _ x = return x

step :: Float -> World -> IO World
step _t world = do
  limits <- getLimits
  let balls = map (updateBall limits) (worldBalls world)
  let balls' = collideBalls [] balls
  return $ world {worldBalls = balls'}

getLimits :: IO Point
getLimits = do
  (w, h) <- getScreenSize
  let hLimits = fromIntegral (divInt h 2) - 10
  let wLimits = fromIntegral (divInt w 2) - 10
  return (wLimits, hLimits)

updateBall :: Point -> Ball -> Ball
updateBall (w, h) ball = updateVelocity $ collideRight w $ collideLeft w $ collideBottom h $ moveBall ball

moveBall :: Ball -> Ball
moveBall ball@(MkBall {pos = p, vel = v}) = ball {pos = vectorAdd p v}

collideBottom :: Float -> Ball -> Ball
collideBottom height ball@(MkBall {pos = (px, py), vel = (x, y)})
  | py <= limit = MkBall {pos = pos', vel = vel'}
  | otherwise = ball
  where
    limit = -height
    pos' = (px, limit - (py - limit))
    vel' = scalarMul (x, -y) decay

-- I dont use this, but might be useful
-- TODO consider using abs for general form;
collideTop :: Float -> Ball -> Ball
collideTop limit ball@(MkBall {pos = (px, py), vel = (x, y)})
  | py >= limit = MkBall {pos = pos', vel = vel'}
  | otherwise = ball
  where
    pos' = (px, limit - (py - limit))
    vel' = (x, -y)

collideLeft :: Float -> Ball -> Ball
collideLeft width ball@(MkBall {pos = (px, py), vel = (x, y)})
  | px <= limit = MkBall {pos = pos', vel = vel'}
  | otherwise = ball
  where
    limit = -width
    pos' = (limit - (px - limit), py)
    vel' = (-x, y)

collideRight :: Float -> Ball -> Ball
collideRight limit ball@(MkBall {pos = (px, py), vel = (x, y)})
  | px >= limit = MkBall {pos = pos', vel = vel'}
  | otherwise = ball
  where
    pos' = (limit - (px - limit), py)
    vel' = (-x, y)

updateVelocity :: Ball -> Ball
updateVelocity ball@(MkBall {vel = v}) = ball {vel = vectorAdd v (0, -g)}

-- ball collisions
didCollide :: Ball -> Ball -> Bool
didCollide b1 b2 = dist (pos b1) (pos b2) <= 20

collideBalls :: [Ball] -> [Ball] -> [Ball]
collideBalls oldBalls (b : bs) = updatedBall : rest
  where
    halfUpdated = foldl collide b oldBalls
    updatedBall = foldl collide halfUpdated bs
    rest = collideBalls (b : oldBalls) bs
collideBalls _ [] = []

collide :: Ball -> Ball -> Ball
collide b1 b2
  | didCollide b1 b2 = resolveCollision b1 b2
  | otherwise = b1

resolveCollision :: Ball -> Ball -> Ball
resolveCollision b1@(MkBall {vel = v1}) b2@(MkBall {vel = v2}) = b1 {pos = newPos, vel = finalVelocity}
  where
    normal' = vectorSub (pos b1) (pos b2)
    deltaP = (10 - magnitude normal') / 2
    deltaPVec = scalarMul normal deltaP
    newPos = vectorSub (pos b1) deltaPVec

    normal = unit normal'
    tangent = perpendicular normal
    (v1n, v1t) = project normal tangent v1
    (v2n, v2t) = project normal tangent v2
    v1t' = v1t
    _v2t' = v2t
    v1n' = v2n
    _v2n' = v1n
    finalVelocity = vectorAdd (scalarMul normal v1n') (scalarMul tangent v1t')

render :: World -> IO Picture
render (MkWorld {worldBalls = balls, slingshot = s, mouse = m}) =
  let ballPics = mconcat $ map renderBall balls
      arrow = case s of
        Just x -> Color red $ line [x, m]
        _ -> blank
   in return $ ballPics <> arrow

renderBall :: Ball -> Picture
renderBall (MkBall (x, y) _) = translate x y $ Color black $ circleSolid 10

game :: IO ()
game = playIO FullScreen white fps (MkWorld {slingshot = Nothing, mouse = (0, 0), worldBalls = []}) render handle step
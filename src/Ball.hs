module Ball (Ball (..), updateBalls) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Settings (Boundaries (..), Setting (..))
import Vectors

data Ball = MkBall {pos :: Point, vel :: Point, size :: Float}

mass :: Ball -> Float
mass = (** 2) . size

accelerate :: Point -> Ball -> Ball
accelerate force ball@(MkBall {vel = v}) = ball {vel = vectorAdd v force}

move :: Ball -> Ball
move ball@(MkBall {pos = p, vel = v}) = ball {pos = vectorAdd p v}

collideBoundaries :: Ball -> Reader Setting Ball
collideBoundaries ball =
  try collideBottom ball
    >>= try collideTop
    >>= try collideLeft
    >>= try collideRight
  where
    try f x = fromMaybeT x (f x)

type BoundaryCollider = Ball -> MaybeT (Reader Setting) Ball

fromMaybeT :: (Monad m) => a -> MaybeT m a -> m a
fromMaybeT value comp = runMaybeT comp <&> fromMaybe value

collideTop :: BoundaryCollider
collideTop ball@(MkBall {pos = (px, py), vel = (x, y)}) = do
  MkSetting {boundaries = (MkBoundaries {top = maybeLimit})} <- ask
  limit <- hoistMaybe maybeLimit
  let pos' = (px, limit - (py - limit))
  let vel' = (x, -y)

  return $
    if py >= limit
      then ball {pos = pos', vel = vel'}
      else ball

collideBottom :: BoundaryCollider
collideBottom ball@(MkBall {pos = (px, py), vel = (x, y)}) = do
  MkSetting {boundaries = (MkBoundaries {bottom = maybeLimit}), decay = d} <- ask
  limit <- hoistMaybe maybeLimit
  let pos' = (px, limit - (py - limit))
  let vel' = scalarMul (x, -y) d

  return $
    if py <= limit
      then ball {pos = pos', vel = vel'}
      else ball

collideLeft :: BoundaryCollider
collideLeft ball@(MkBall {pos = (px, py), vel = (x, y)}) = do
  MkSetting {boundaries = (MkBoundaries {left = maybeLimit})} <- ask
  limit <- hoistMaybe maybeLimit
  let pos' = (limit - (px - limit), py)
  let vel' = (-x, y)

  return $
    if px <= limit
      then ball {pos = pos', vel = vel'}
      else ball

collideRight :: BoundaryCollider
collideRight ball@(MkBall {pos = (px, py), vel = (x, y)}) = do
  MkSetting {boundaries = (MkBoundaries {right = maybeLimit})} <- ask
  limit <- hoistMaybe maybeLimit
  let pos' = (limit - (px - limit), py)
  let vel' = (-x, y)
  return $
    if px >= limit
      then ball {pos = pos', vel = vel'}
      else ball

-- ball collisions
didCollide :: Ball -> Ball -> Bool
didCollide b1 b2 = dist (pos b1) (pos b2) <= (size b1 + size b2)

collideBalls :: [Ball] -> [Ball] -> [Ball]
collideBalls oldBalls (b : bs) = updatedBall : rest
  where
    updatedBall = foldl collide b (oldBalls ++ bs)
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
    normal = unit normal'

    minDist = size b1 + size b2
    deltaP = (minDist - magnitude normal') / 2
    deltaPVec = scalarMul normal deltaP
    newPos = vectorSub (pos b1) deltaPVec

    m1 = mass b1
    m2 = mass b2

    tangent = perpendicular normal
    (v1n, v1t) = project normal tangent v1
    (v2n, _v2t) = project normal tangent v2
    v1t' = v1t
    v1n' = ((v1n * (m1 - m2)) + (2 * m2 * v2n)) / (m1 + m2)
    finalVelocity = vectorAdd (scalarMul normal v1n') (scalarMul tangent v1t')

updateBall :: Ball -> Reader Setting Ball
updateBall ball = do
  (MkSetting {g = grav}) <- ask
  ball' <- collideBoundaries $ move ball
  return $ accelerate (0, -grav) ball'

updateBalls :: [Ball] -> Reader Setting [Ball]
updateBalls balls = do
  balls' <- mapM updateBall balls
  return $ collideBalls [] balls'
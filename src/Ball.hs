module Ball (Ball (..), updateBalls) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Settings (Boundaries (..), Setting (..))
import Vectors

data Ball = MkBall {pos :: Point, vel :: Point, size :: Float}
  deriving (Show)

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

fromMaybeT :: (Monad m) => a -> MaybeT m a -> m a
fromMaybeT value comp = runMaybeT comp <&> fromMaybe value

data Axis = X | Y

transform :: Axis -> (Float -> Float) -> Point -> Point
transform X f (x, y) = (f x, y)
transform Y f (x, y) = (x, f y)

getAxis :: Axis -> Point -> Float
getAxis X = fst
getAxis Y = snd

boundaryCollider :: Axis -> Maybe Float -> Ball -> Maybe Ball
boundaryCollider _ Nothing _ = Nothing
boundaryCollider _ (Just 0) _ = Nothing
boundaryCollider ax limit'' ball@(MkBall {pos = p, vel = v, size = sz}) = do
  limit' <- limit''
  let limit = if limit' < 0 then limit' + sz else limit' - sz

  let vel' = transform ax negate v
  let pos' = transform ax ((2 * limit) -) p
  let coord = getAxis ax p
  if (coord * limit > 0) && (abs coord >= abs limit)
    then Just ball {pos = pos', vel = vel'}
    else Nothing

collideTop :: Ball -> MaybeT (Reader Setting) Ball
collideTop ball = do
  MkSetting {boundaries = (MkBoundaries {top = limit})} <- ask
  hoistMaybe $ boundaryCollider Y limit ball

collideBottom :: Ball -> MaybeT (Reader Setting) Ball
collideBottom ball = do
  MkSetting {boundaries = (MkBoundaries {bottom = limit}), decay = d} <- ask
  ball'@(MkBall {vel = v}) <- hoistMaybe $ boundaryCollider Y limit ball
  let vel' = scalarMul v d
  return ball' {vel = vel'}

collideLeft :: Ball -> MaybeT (Reader Setting) Ball
collideLeft ball = do
  MkSetting {boundaries = (MkBoundaries {left = limit})} <- ask
  hoistMaybe $ boundaryCollider X limit ball

collideRight :: Ball -> MaybeT (Reader Setting) Ball
collideRight ball = do
  MkSetting {boundaries = (MkBoundaries {right = limit})} <- ask
  hoistMaybe $ boundaryCollider X limit ball

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
    normal' = vectorSub (pos b2) (pos b1)
    normal = unit normal'

    minDist = size b1 + size b2
    speedAdjustment = magnitude v1 / (magnitude v1 + magnitude v2)
    deltaP = (minDist - magnitude normal') * ((size b1 + 1) / minDist) * speedAdjustment
    newPos = vectorSub (pos b1) $ scalarMul normal deltaP

    m1 = mass b1
    m2 = mass b2

    tangent = perpendicular normal
    (v1n, v1t) = unitProject normal tangent v1
    (v2n, _v2t) = unitProject normal tangent v2
    v1t' = v1t
    v1n' = ((v1n * (m1 - m2)) + (2 * m2 * v2n)) / (m1 + m2)
    finalVelocity = vectorAdd (scalarMul normal v1n') (scalarMul tangent v1t')

applyForce :: Float -> Ball -> Ball -> Ball
applyForce gravConst b1 b2 = do
  let normal = vectorSub (pos b2) (pos b1)

  let m2 = mass b2

  let acceleration = (gravConst * m2) / (magnitude normal ** 2)
  let accelerationVector = scalarMul (unit normal) acceleration

  accelerate accelerationVector b1

attractParticles :: Float -> [Ball] -> [Ball] -> [Ball]
attractParticles _ _ [] = []
attractParticles iGrav oldBalls (b : bs) =
  foldl (applyForce iGrav) b (oldBalls ++ bs)
    : attractParticles iGrav (b : oldBalls) bs

updateBall :: Ball -> Reader Setting Ball
updateBall ball = do
  (MkSetting {g = grav}) <- ask
  ball' <- collideBoundaries $ move ball
  return $ accelerate (0, -grav) ball'

updateBalls :: [Ball] -> Reader Setting [Ball]
updateBalls balls = do
  (MkSetting {i = iGrav}) <- ask
  balls' <- mapM updateBall balls

  let balls'' =
        ( case iGrav of
            Nothing -> balls'
            Just iGrav' -> attractParticles iGrav' [] balls'
        )
  return $ collideBalls [] balls''
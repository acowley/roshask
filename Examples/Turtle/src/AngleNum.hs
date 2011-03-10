{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
-- |Define a lightweight wrapper, 'Angle', for floating point numeric
-- types that comes with a 'Num' instance that wraps around at
-- 0 and 2&#x03C0, and properly handles angle differencing.
module AngleNum (Angle, angle, fromAngle, toDegrees) where
import Data.Function
import Data.VectorSpace

wrapAngle :: (Floating a, Ord a) => a -> a
wrapAngle theta 
  | theta < 0    = theta + 2 * pi
  | theta > 2*pi = theta - 2 * pi
  | otherwise    = theta

angleDiff :: (Floating a, Ord a) => a -> a -> a
angleDiff x y = wrapAngle (x' + pi - y') - pi
  where x' = if x < 0 then x + 2*pi else x
        y' = if y < 0 then y + 2*pi else y

-- |Representation of an angle in radians.
newtype Angle a = Angle { fromAngle :: a } 
  deriving (Eq, Ord, Show, Fractional, Floating)

-- |Produce an 'Angle' value within the range [0,2pi].
angle :: (Floating a, Ord a) => a -> Angle a
angle = Angle . wrapAngle

-- |Convert an 'Angle' into a floating point number of degrees.
toDegrees :: Floating a => Angle a -> a
toDegrees = (* (180 / pi)) . fromAngle

instance (Floating a, Ord a) => Num (Angle a) where
  Angle x + Angle y = Angle . wrapAngle $ x + y
  (*) = ((Angle . wrapAngle) .) . (*) `on` fromAngle
  Angle x - Angle y = Angle $ angleDiff x y
  negate (Angle x) = Angle (negate x)
  abs (Angle x) = Angle (abs x)
  signum (Angle x) = Angle (signum x)
  fromInteger = Angle . wrapAngle . fromInteger

instance (Floating a, Ord a, AdditiveGroup a) => AdditiveGroup (Angle a) where
  zeroV = Angle 0
  (^+^) = ((Angle . wrapAngle) . ) . (^+^) `on` fromAngle
  negateV = negate

instance (Floating a, Ord a, AdditiveGroup a) => VectorSpace (Angle a) where
  type Scalar (Angle a) = a
  s *^ Angle x = Angle . wrapAngle $ s * x

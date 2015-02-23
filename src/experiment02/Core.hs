
{-# LANGUAGE MultiParamTypeClasses #-}

module Core
(
  NonUnitVector
, UnitVector
, Point(..)
, Ray(..)
, Transform(..)
, RayPosition
, VectorUnaryOps(..)
, VectorBinaryOps(..)
, vector
, normal
, unsafeForceUnitVector
, origin
, to
, normalize
, at
, toRayPosition
, magnitude
)
where


data Vector = Vector !Double !Double !Double

newtype NonUnitVector = NonUnitVector Vector

newtype UnitVector = UnitVector Vector

data Point = Point !Double !Double !Double

data Ray = Ray
    { rayOrigin    :: !Point
    , rayDirection :: !UnitVector
    }

newtype RayPosition = RayPosition Double
    deriving (Eq, Ord)

class Transform t where
    translate :: NonUnitVector -> t -> t

class VectorUnaryOps v where
    neg          :: v -> v
    vectorValues :: v -> (Double, Double, Double)
    (|*|)        :: v -> Double -> NonUnitVector

class VectorBinaryOps v1 v2 where
    (|.|) :: v1 -> v2 -> Double
    (|+|) :: v1 -> v2 -> NonUnitVector
    cross :: v1 -> v2 -> NonUnitVector

instance Transform Point where
    translate (NonUnitVector (Vector !vx !vy !vz)) (Point !px !py !pz) =
        Point (px + vx)
              (py + vy)
              (pz + vz)

instance Transform Ray where
    translate !v (Ray !ro !rd) =
        Ray { rayOrigin    = translate v ro
            , rayDirection = rd
            }

instance VectorUnaryOps NonUnitVector where
    neg (NonUnitVector (Vector !xv !yv !zv)) =
        vector (-xv)
               (-yv)
               (-zv)
    vectorValues (NonUnitVector (Vector !x !y !z)) =
        (x, y, z)
    (|*|) (NonUnitVector (Vector !vx !vy !vz)) !s =
        vector (vx * s)
               (vy * s)
               (vz * s)

instance VectorUnaryOps UnitVector where
    neg (UnitVector (Vector !xv !yv !zv)) =
        UnitVector (Vector (-xv)
                           (-yv)
                           (-zv))
    vectorValues (UnitVector (Vector !x !y !z)) =
        (x, y, z)
    (|*|) (UnitVector (Vector !vx !vy !vz)) !s =
        vector (vx * s)
               (vy * s)
               (vz * s)

instance VectorBinaryOps NonUnitVector NonUnitVector where
    (|.|) (NonUnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vx * wx
      + vy * wy
      + vz * wz
    (|+|) (NonUnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vector (vx + wx)
               (vy + wy)
               (vz + wz)
    cross (NonUnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vector (vy * wz - vz * wy)
               (vz * wx - vx * wz)
               (vx * wy - vy * wx)

instance VectorBinaryOps NonUnitVector UnitVector where
    (|.|) (NonUnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vx * wx
      + vy * wy
      + vz * wz
    (|+|) (NonUnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vector (vx + wx)
               (vy + wy)
               (vz + wz)
    cross (NonUnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vector (vy * wz - vz * wy)
               (vz * wx - vx * wz)
               (vx * wy - vy * wx)

instance VectorBinaryOps UnitVector UnitVector where
    (|.|) (UnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vx * wx
      + vy * wy
      + vz * wz
    (|+|) (UnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vector (vx + wx)
               (vy + wy)
               (vz + wz)
    cross (UnitVector (Vector !vx !vy !vz)) (UnitVector (Vector !wx !wy !wz)) =
        vector (vy * wz - vz * wy)
               (vz * wx - vx * wz)
               (vx * wy - vy * wx)

vector :: Double -> Double -> Double -> NonUnitVector
vector !x !y !z =
    NonUnitVector (Vector x y z)

normal :: Double -> Double -> Double -> UnitVector
normal !x !y !z =
    normalize $ vector x y z

unsafeForceUnitVector :: NonUnitVector -> UnitVector
unsafeForceUnitVector (NonUnitVector v) =
    UnitVector v

origin :: Point
origin = Point 0.0 0.0 0.0

to :: Point -> Point -> NonUnitVector
to (Point !px !py !pz) (Point !qx !qy !qz)
    = vector (qx - px)
             (qy - py)
             (qz - pz)

normalize :: NonUnitVector -> UnitVector
normalize !v =
    UnitVector (Vector nx ny nz)
  where
    !m = 1.0 / magnitude v
    (NonUnitVector (Vector !nx !ny !nz)) = v |*| m

magnitude :: NonUnitVector -> Double
magnitude =
    sqrt . magnitudeSquared


magnitudeSquared :: NonUnitVector -> Double
magnitudeSquared (NonUnitVector (Vector !vx !vy !vz)) =
    vx * vx
  + vy * vy
  + vz * vz

at :: Ray -> RayPosition -> Point
at (Ray ro rd) (RayPosition t) =
    translate (rd |*| t) ro

toRayPosition :: Double -> RayPosition
toRayPosition =
    RayPosition

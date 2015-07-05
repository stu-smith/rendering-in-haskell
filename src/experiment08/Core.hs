
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
, RefractiveIndex
, RayWithMedium(..)
, vector
, normal
, unsafeForceUnitVector
, origin
, to
, normalize
, normalizeWithLength
, at
, toRayPosition
, magnitude
, magnitudeSquared
, unitX
, unitY
, unitZ
, perpendiculars
, calculateReflection
, calculateRefraction
, refractiveIndexAir
, refractiveIndexGlass
)
where

import Numeric.FastMath  ( )
import Control.DeepSeq   ( NFData(..) )


data Vector = Vector !Double !Double !Double

instance NFData Vector where
    rnf !(Vector !_ !_ !_) = ()

newtype NonUnitVector = NonUnitVector Vector

instance NFData NonUnitVector where
    rnf !(NonUnitVector !(Vector !_ !_ !_)) = ()

newtype UnitVector = UnitVector Vector

instance NFData UnitVector where
    rnf !(UnitVector !(Vector !_ !_ !_)) = ()

data Point = Point !Double !Double !Double

instance NFData Point where
    rnf !(Point !_ !_ !_) = ()

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
    rotateAround :: v -> UnitVector -> Double -> v

class (VectorUnaryOps v1, VectorUnaryOps v2) => VectorBinaryOps v1 v2 where
    (|.|) :: v1 -> v2 -> Double
    (|+|) :: v1 -> v2 -> NonUnitVector
    (|-|) :: v1 -> v2 -> NonUnitVector
    (|-|) v1 v2 = v1 |+| neg v2
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
    rotateAround v k theta =
        (v |*| cosTheta ) |+| ((k `cross` v) |*| sinTheta) |+| (k |*| ((k |.| v) * (1.0 - cosTheta)))
      where
        cosTheta = cos theta
        sinTheta = sin theta

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
    rotateAround v k theta =
        normalize ((v |*| cosTheta ) |+|
                   ((k `cross` v) |*| sinTheta) |+|
                   (k |*| ((k |.| v) * (1.0 - cosTheta))))
      where
        cosTheta = cos theta
        sinTheta = sin theta

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

instance VectorBinaryOps UnitVector NonUnitVector where
    (|.|) (UnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vx * wx
      + vy * wy
      + vz * wz
    (|+|) (UnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vector (vx + wx)
               (vy + wy)
               (vz + wz)
    cross (UnitVector (Vector !vx !vy !vz)) (NonUnitVector (Vector !wx !wy !wz)) =
        vector (vy * wz - vz * wy)
               (vz * wx - vx * wz)
               (vx * wy - vy * wx)

vector :: Double -> Double -> Double -> NonUnitVector
vector !x !y !z =
    NonUnitVector (Vector x y z)

unitX :: UnitVector
unitX = UnitVector (Vector 1.0 0.0 0.0)

unitY :: UnitVector
unitY = UnitVector (Vector 0.0 1.0 0.0)

unitZ :: UnitVector
unitZ = UnitVector (Vector 0.0 0.0 1.0)

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

normalizeWithLength :: NonUnitVector -> (UnitVector, RayPosition)
normalizeWithLength !v =
    (UnitVector (Vector nx ny nz), RayPosition mag)
  where
    !mag = magnitude v
    !m   = 1.0 / mag
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
at (Ray !ro !rd) (RayPosition !t) =
    translate (rd |*| t) ro

toRayPosition :: Double -> RayPosition
toRayPosition =
    RayPosition

perpendiculars :: UnitVector -> (NonUnitVector, NonUnitVector)
perpendiculars n =
    (vb1, vb2)
  where
    (!nx, !ny, _) = vectorValues n
    !vb1pre       = if abs nx > abs ny then unitY else unitX
    vb1           = vb1pre |-| (n |*| (n |.| vb1pre))
    vb2           = n `cross` vb1

calculateReflection :: UnitVector -> UnitVector -> UnitVector
calculateReflection !incoming !surfaceNormal =
    normalize (incoming |+| (surfaceNormal |*| (2 * c1)))
  where
    !c1 = - (surfaceNormal |.| incoming)

newtype RefractiveIndex = RefractiveIndex Double

data RayWithMedium = RayWithMedium Ray RefractiveIndex

refractiveIndexAir :: RefractiveIndex
refractiveIndexAir = RefractiveIndex 1.0

refractiveIndexGlass :: RefractiveIndex
refractiveIndexGlass = RefractiveIndex 1.5

calculateRefraction :: UnitVector -> UnitVector -> RefractiveIndex -> RefractiveIndex -> (UnitVector, RefractiveIndex)
calculateRefraction !incoming !surfaceNormal (RefractiveIndex !ri1) (RefractiveIndex !ri2) =
    if sin2ThetaT > 1.0
       then (calculateReflection incoming surfaceNormal, (RefractiveIndex ri1))
       else (normalize ((incoming |*| ri1ri2) |+| (surfaceNormal |*| factor)), (RefractiveIndex ri2))
  where
    !sin2ThetaT = (ri1ri2 * ri1ri2) * (1.0 - cosThetaI * cosThetaI)
    !ri1ri2     = ri1 / ri2
    !cosThetaI  = acos (incoming |.| surfaceNormal)
    factor      = ri1ri2 * cosThetaI - sqrt (1.0 - sin2ThetaT)

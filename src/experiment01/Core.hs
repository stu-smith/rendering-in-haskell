module Core
(
  Vector(..)
, Point(..)
, Ray(..)
, Transform(..)
, origin
, to
, neg
, normalize
, cross
, at
, (|*|)
, (|.|)
, (|+|)
)
where


data Vector = Vector !Double !Double !Double

data Point = Point !Double !Double !Double

data Ray = Ray
    { rayOrigin    :: !Point
    , rayDirection :: !Vector
    }


class Transform t where
    translate :: Vector -> t -> t


instance Transform Point where
    translate (Vector !vx !vy !vz) (Point !px !py !pz) =
        Point (px + vx)
              (py + vy)
              (pz + vz)

instance Transform Ray where
    translate !v (Ray !ro !rd) =
        Ray { rayOrigin    = translate v ro
            , rayDirection = rd
            }


origin :: Point
origin = Point 0.0 0.0 0.0

to :: Point -> Point -> Vector
to (Point !px !py !pz) (Point !qx !qy !qz)
    = Vector (qx - px)
             (qy - py)
             (qz - pz)

(|*|) :: Vector -> Double -> Vector
(|*|) (Vector !vx !vy !vz) !s =
    Vector (vx * s)
           (vy * s)
           (vz * s)

(|.|) :: Vector -> Vector -> Double
(|.|) (Vector !vx !vy !vz) (Vector !wx !wy !wz) =
    vx * wx
  + vy * wy
  + vz * wz

(|+|) :: Vector -> Vector -> Vector
(|+|) (Vector !vx !vy !vz) (Vector !wx !wy !wz) =
    Vector (vx + wx)
           (vy + wy)
           (vz + wz)

neg :: Vector -> Vector
neg (Vector !xv !yv !zv) =
    Vector (-xv)
           (-yv)
           (-zv)

normalize :: Vector -> Vector
normalize !v =
    v |*| m
  where !m = 1.0 / magnitude v

magnitude :: Vector -> Double
magnitude =
    sqrt . magnitudeSquared


magnitudeSquared :: Vector -> Double
magnitudeSquared (Vector !vx !vy !vz) =
    vx * vx
  + vy * vy
  + vz * vz

cross :: Vector -> Vector -> Vector
cross (Vector !vx !vy !vz) (Vector !wx !wy !wz) =
    Vector (vy * wz - vz * wy)
           (vz * wx - vx * wz)
           (vx * wy - vy * wx)


at :: Ray -> Double -> Point
at (Ray ro rd) t =
    translate (rd |*| t) ro

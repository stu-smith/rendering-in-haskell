module Surface
(
  Surface(..)
, mkSphere
, mkPlane
)
where

import Core   ( Vector(..), Point(..), Ray(..), Transform(..)
              , origin, to, neg, (|*|), (|.|) )
import Color  ( Color(..) )

data Surface = Surface
    { intersection  :: (Ray   -> Maybe Double)
    , normalAtPoint :: (Point -> Vector)
    , flatColor     :: !Color
    }


instance Transform Surface where
    translate !v (Surface sfcIntersection sfcNormal sfcColor) =
        Surface { intersection = newIntersection
                , normalAtPoint       = newNormal
                , flatColor           = sfcColor
                }
      where newIntersection !ray = sfcIntersection $ translate nv ray
            newNormal       !pos = sfcNormal       $ translate nv pos
            nv                   = neg v



mkSphere :: Double -> Color -> Surface
mkSphere !radius !color = Surface
    { intersection  = sphereIntersection radius
    , normalAtPoint = sphereNormal (1.0 / radius)
    , flatColor     = color
    }

mkPlane :: Point -> Vector -> Color -> Surface
mkPlane !point !normal !color = Surface
    { intersection  = planeIntersection point normal
    , normalAtPoint = const normal
    , flatColor     = color
    }



sphereIntersection :: Double -> Ray -> Maybe Double
sphereIntersection !r (Ray !ro !rd)
        | det    < 0   = Nothing
        | b - sd > eps = Just (b - sd)
        | b + sd > eps = Just (b + sd)
        | otherwise    = Nothing
      where !op  = ro `to` origin
            !eps = 1e-4
            !b   = op |.| rd
            !det = (b * b) - (op |.| op) + (r * r)
            sd   = sqrt det

sphereNormal :: Double -> Point -> Vector
sphereNormal invr p =
    (origin `to` p) |*| invr

planeIntersection :: Point -> Vector -> Ray -> Maybe Double
planeIntersection point normal (Ray ro rd)
    | ln == 0.0  = Nothing
    | otherwise = Just d
  where d  = ((ro `to` point) |.| normal) / ln
        ln = rd |.| normal

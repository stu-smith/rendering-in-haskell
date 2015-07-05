module Volume
(
  Volume(..)
, mkSphere
, mkInfinite
, mkInfinitePlane
)
where

import Numeric.FastMath  ( )

import Core              ( Transform(..), Ray(..), Point(..), UnitVector, RayPosition
                         , neg, to, toRayPosition, origin, unsafeForceUnitVector, (|.|), (|*|) )
import Material          ( Material )


data Volume = Volume
    { volumeSurfaceIntersection  :: Ray   -> Maybe RayPosition
    , volumeSurfaceNormalAtPoint :: Point -> UnitVector
    , volumePointInVolume        :: Point -> Bool
    , volumeMaterial             :: !Material
    }

instance Transform Volume where
    translate !v (Volume !intersection !normal !pointIn !material) =
        Volume { volumeSurfaceIntersection  = newIntersection
               , volumeSurfaceNormalAtPoint = newNormal
               , volumePointInVolume        = newPointIn
               , volumeMaterial             = material
               }
      where
        newIntersection !ray = intersection $ translate nv ray
        newNormal       !pos = normal       $ translate nv pos
        newPointIn      !pos = pointIn      $ translate nv pos
        !nv                  = neg v

mkSphere :: Double -> Material -> Volume
mkSphere !radius !material = Volume
    { volumeSurfaceIntersection  = sphereIntersection radius
    , volumeSurfaceNormalAtPoint = sphereNormal (1.0 / radius)
    , volumePointInVolume        = sphereInside radius
    , volumeMaterial             = material
    }

mkInfinite :: Material -> Volume
mkInfinite !material = Volume
    { volumeSurfaceIntersection  = const Nothing
    , volumeSurfaceNormalAtPoint = undefined
    , volumePointInVolume        = const True
    , volumeMaterial             = material
    }

mkInfinitePlane :: Point -> UnitVector -> Material -> Volume
mkInfinitePlane !point !normal !material = Volume
    { volumeSurfaceIntersection  = planeIntersection point normal
    , volumeSurfaceNormalAtPoint = const normal
    , volumePointInVolume        = planeCategorise point normal
    , volumeMaterial             = material
    }

sphereIntersection :: Double -> Ray -> Maybe RayPosition
sphereIntersection !r (Ray !ro !rd)
        | det    < 0   = Nothing
        | b - sd > eps = Just $ toRayPosition (b - sd)
        | b + sd > eps = Just $ toRayPosition (b + sd)
        | otherwise    = Nothing
      where !op  = ro `to` origin
            !eps = 1e-4
            !b   = op |.| rd
            !det = (b * b) - (op |.| op) + (r * r)
            sd   = sqrt det

sphereNormal :: Double -> Point -> UnitVector
sphereNormal !invr !p =
    unsafeForceUnitVector $ (origin `to` p) |*| invr

sphereInside :: Double -> Point -> Bool
sphereInside !radius (Point !px !py !pz) =
    ds <= radius * radius
  where
    ds = px * px + py * py + pz * pz

planeIntersection :: Point -> UnitVector -> Ray -> Maybe RayPosition
planeIntersection !point !normal (Ray !ro !rd)
    | ln == 0.0 = Nothing
    | d   < 0.0 = Nothing
    | otherwise = Just $ toRayPosition d
  where !d  = ((ro `to` point) |.| normal) / ln
        !ln = rd |.| normal

planeCategorise :: Point -> UnitVector -> Point -> Bool
planeCategorise !point !normal !test =
    (normal |.| (point `to` test)) <= 0.0

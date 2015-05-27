module Surface
(
  Surface(..)
, mkSphere
, mkPlane
, mkDisc
)
where

import Numeric.FastMath  ( )

import Core              ( UnitVector, Point(..), Ray(..), Transform(..), RayPosition, VectorUnaryOps(..)
                         , origin, to, at, neg, toRayPosition, unsafeForceUnitVector, magnitudeSquared
                         , (|.|) )
import Material          ( Material )


data Surface = Surface
    { surfaceIntersection  :: Ray   -> Maybe RayPosition
    , surfaceNormalAtPoint :: Point -> UnitVector
    , surfaceMaterial      :: !Material
    }


instance Transform Surface where
    translate !v (Surface intersection normal material) =
        Surface { surfaceIntersection  = newIntersection
                , surfaceNormalAtPoint = newNormal
                , surfaceMaterial      = material
                }
      where
        newIntersection !ray = intersection $ translate nv ray
        newNormal       !pos = normal       $ translate nv pos
        !nv                  = neg v

mkSphere :: Double -> Material -> Surface
mkSphere !radius !material = Surface
    { surfaceIntersection  = sphereIntersection radius
    , surfaceNormalAtPoint = sphereNormal (1.0 / radius)
    , surfaceMaterial      = material
    }

mkPlane :: Point -> UnitVector -> Material -> Surface
mkPlane !point !normal !material = Surface
    { surfaceIntersection  = planeIntersection point normal
    , surfaceNormalAtPoint = const normal
    , surfaceMaterial      = material
    }

mkDisc :: Point -> UnitVector -> Double -> Material -> Surface
mkDisc !point !normal !radius !material = Surface
    { surfaceIntersection  = discIntersection point normal radius
    , surfaceNormalAtPoint = const normal
    , surfaceMaterial      = material
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

planeIntersection :: Point -> UnitVector -> Ray -> Maybe RayPosition
planeIntersection !point !normal (Ray !ro !rd)
    | ln == 0.0 = Nothing
    | d   < 0.0 = Nothing
    | otherwise = Just $ toRayPosition d
  where !d  = ((ro `to` point) |.| normal) / ln
        !ln = rd |.| normal

discIntersection :: Point -> UnitVector -> Double -> Ray -> Maybe RayPosition
discIntersection !point !normal !radius !ray = do
    rp <- planeIntersection point normal ray
    let !pos    = ray `at` rp
    let !distSq = magnitudeSquared (point `to` pos)
    if distSq <= radius * radius
        then Just rp
        else Nothing

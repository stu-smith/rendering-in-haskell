module Surface
(
  Surface(..)
, mkSphere
, mkPlane
)
where

import Core      ( UnitVector, Point(..), Ray(..), Transform(..), RayPosition
                 , origin, to, neg, toRayPosition, normalize, (|.|) )
import Material  ( Material )


data Surface = Surface
    { intersection  :: Ray   -> Maybe RayPosition
    , normalAtPoint :: Point -> UnitVector
    , material      :: Material
    }


instance Transform Surface where
    translate !v (Surface sfcIntersection sfcNormal sfcMaterial) =
        Surface { intersection  = newIntersection
                , normalAtPoint = newNormal
                , material      = sfcMaterial
                }
      where newIntersection !ray = sfcIntersection $ translate nv ray
            newNormal       !pos = sfcNormal       $ translate nv pos
            nv                   = neg v



mkSphere :: Double -> Material -> Surface
mkSphere !radius !mat = Surface
    { intersection  = sphereIntersection radius
    , normalAtPoint = sphereNormal
    , material      = mat
    }

mkPlane :: Point -> UnitVector -> Material -> Surface
mkPlane !point !normal !mat = Surface
    { intersection  = planeIntersection point normal
    , normalAtPoint = const normal
    , material      = mat
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

sphereNormal :: Point -> UnitVector
sphereNormal p =
    normalize (origin `to` p)

planeIntersection :: Point -> UnitVector -> Ray -> Maybe RayPosition
planeIntersection point normal (Ray ro rd)
    | ln == 0.0 = Nothing
    | d   < 0.0 = Nothing
    | otherwise = Just $ toRayPosition d
  where d  = ((ro `to` point) |.| normal) / ln
        ln = rd |.| normal

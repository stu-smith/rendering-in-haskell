module Material
(
  Material
, ColorProbability(..)
, BRDF
, mkMaterial
, mkEmmissive
, probabilityDiffuseReflection
, probabilitySpecularReflection
, brdf
, diffuseLight
, specularLight
, hasSpecularComponent
, specularProbabilities
, mul
, emmissive
)
where

import Numeric.FastMath  ( )
import Core              ( UnitVector, Point, (|.|), (|*|), (|-|) )
import Light             ( Light(..), scaled, sumLights, black )


type BRDF = Light -> UnitVector -> UnitVector -> UnitVector -> Point -> Light

data ColorProbability = ColorProbability !Double !Double !Double

data Material = Material
  { diffuseProbabilities          :: !ColorProbability
  , specularProbabilities         :: !ColorProbability
  , probabilityDiffuseReflection  :: !Double
  , probabilitySpecularReflection :: !Double
  , brdf                          :: !BRDF
  , emmissive                     :: !Light
  }

mkMaterial :: ColorProbability -> ColorProbability -> Material
mkMaterial diffuse@(ColorProbability !dr !dg !db) specular@(ColorProbability !sr !sg !sb) =
    Material { diffuseProbabilities          = diffuse
             , specularProbabilities         = specular
             , probabilityDiffuseReflection  = pd
             , probabilitySpecularReflection = pr - pd
             , brdf                          = phongBrdf diffuse specular
             , emmissive                     = black
             }
  where
    !drdgdb = dr + dg + db
    !srsgsb = sr + sg + sb
    !pr     = max (dr + sr) $ max (dg + sg) (db + sb)
    !pd     = pr * drdgdb / (drdgdb + srsgsb)

mkEmmissive :: Light -> Material
mkEmmissive light =
    Material { diffuseProbabilities          = ColorProbability 0.0 0.0 0.0
             , specularProbabilities         = ColorProbability 0.0 0.0 0.0
             , probabilityDiffuseReflection  = 0.0
             , probabilitySpecularReflection = 0.0
             , brdf                          = emmissiveBrdf light
             , emmissive                     = light
             }

diffuseLight :: Material -> Light -> Light
diffuseLight (Material (ColorProbability !dr !dg !db) _ !pd _ _ _) (Light !r !g !b) =
    Light (r * dr * ipd)
          (g * dg * ipd)
          (b * db * ipd)
  where
    !ipd = 1.0 / pd

specularLight :: Material -> Light -> Light
specularLight (Material _ (ColorProbability !sr !sg !sb) _ !ps _ _) (Light !r !g !b) =
    Light (r * sr * ips)
          (g * sg * ips)
          (b * sb * ips)
  where
    !ips = 1.0 / ps

diffuseBrdf :: ColorProbability -> BRDF
diffuseBrdf !pd !incomingLight !incomingVector _ !surfaceNormal _ =
    incomingLight `mul` pd `scaled` (incomingVector |.| surfaceNormal)

specularBrdf :: ColorProbability -> BRDF
specularBrdf !ps !incomingLight !incomingVector !outgoingVector !surfaceNormal _ =
    incomingLight `mul` ps `scaled` (outgoingVector |.| reflectionVector)
  where
    reflectionVector = surfaceNormal |*| ((surfaceNormal |*| 2) |.| incomingVector) |-| incomingVector

sumBrdfs :: [BRDF] -> BRDF
sumBrdfs !brdfs incomingLight incomingVector outgoingVector surfaceNormal wp =
    sumLights $ map apply brdfs
  where
    apply f = f incomingLight incomingVector outgoingVector surfaceNormal wp

phongBrdf :: ColorProbability -> ColorProbability -> BRDF
phongBrdf !pd !ps =
    sumBrdfs [diffuseBrdf pd, specularBrdf ps]

emmissiveBrdf :: Light -> BRDF
emmissiveBrdf light =
    (const . const . const . const . const) light


mul :: Light -> ColorProbability -> Light
mul (Light !lr !lg !lb) (ColorProbability !pr !pg !pb) =
    Light (lr * pr) (lg * pg) (lb * pb)

hasSpecularComponent :: Material -> Bool
hasSpecularComponent material =
    pr > 0.0 || pg > 0.0 || pb > 0.0
  where
    (ColorProbability pr pg pb) = specularProbabilities material

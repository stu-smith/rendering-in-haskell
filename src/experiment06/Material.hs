module Material
(
  Material
, ColorProbability(..)
, mkMaterial
, probabilityDiffuseReflection
, probabilitySpecularReflection
, diffuseLight
, specularLight
)
where

import Light  ( Light(..) )

data ColorProbability = ColorProbability !Double !Double !Double

data Material = Material
  { diffuseProbabilities          :: ColorProbability
  , specularProbabilities         :: ColorProbability
  , probabilityDiffuseReflection  :: Double
  , probabilitySpecularReflection :: Double
  }

mkMaterial :: ColorProbability -> ColorProbability -> Material
mkMaterial diffuse@(ColorProbability dr dg db) specular@(ColorProbability sr sg sb) =
    Material { diffuseProbabilities          = diffuse
             , specularProbabilities         = specular
             , probabilityDiffuseReflection  = pd
             , probabilitySpecularReflection = pr - pd
             }
  where
    drdgdb = dr + dg + db
    srsgsb = sr + sg + sb
    pr     = max (dr + sr) $ max (dg + sg) (db + sb)
    pd     = pr * drdgdb / (drdgdb + srsgsb)

diffuseLight :: Material -> Light -> Light
diffuseLight (Material (ColorProbability dr dg db) _ pr _) (Light r g b) =
    Light (r * dr / pr)
          (g * dg / pr)
          (b * db / pr)

specularLight :: Material -> Light -> Light
specularLight (Material _ (ColorProbability sr sg sb) _ ps) (Light r g b) =
    Light (r * sr / ps)
          (g * sg / ps)
          (b * sb / ps)

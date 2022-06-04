module PublicRoutes where

import           Control.Monad.State
import           Data.Maybe
import           Data.Set


type Stop = String
type Route = [Stop]
type Rutas = [Route]

accion :: Rutas -> Stop -> Stop
accion rutas st = snd $ runState (mapM aux rutas) st where
    aux route = do
        stop <- get
        put $ case dropWhile (\s -> s /= stop)route of
                (_:stop':_) -> stop'
                _           -> stop

unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f []          = []
unzipWith f ((k, v):xs) = f k v : unzipWith f xs

movimientos :: [(Route, Int)] -> Stop -> Stop
movimientos = accion . concat . unzipWith (flip replicate)

-- [l1, l2, l3]
-- DelGolfo
-- FelixGomez

transbord :: Route -> Route -> Maybe Stop
transbord xr yr = do
    let xr' = fromList xr
        yr' = fromList yr
    case toList $ intersection xr' yr' of
      []    -> Nothing
      (x:_) -> Just x

joinRoutes :: Route -> Route -> Maybe Route
joinRoutes xr yr =
    case transbord xr yr of
      Nothing -> Nothing
      Just t -> Just $ takeWhile travel xr ++ dropWhile travel yr
          where travel = (\s -> s /= t)

linea1 :: Route
linea1 =
    [ "Hospital"
    , "Central"
    , "Cuahutemoc"
    , "Del Golfo"
    , "Felix Gomez"
    ]

linea2 :: Route
linea2 =
    [ "Regina"
    , "General Anaya"
    , "Cuahutemoc"
    , "Alameda"
    , "Zaragoza"
    ]

linea3 :: Route
linea3 =
    [ "Zaragoza"
    , "Santa Lucia"
    , "Colonia Obrera"
    , "Felix Gomez"]

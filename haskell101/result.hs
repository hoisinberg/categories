module Result where
import qualified GHC.Err as GHC

type Result v = Either v [String]

value :: v -> Result v
value = Left

errors :: [String] -> Result v
errors = Right

error :: String -> Result v
error s = errors [s]

get :: Result v -> v
get (Left v) = v
get (Right e) = GHC.error (show e)

orDefault :: Result v -> v -> v
orDefault (Left v) _ = v
orDefault (Right e) d = d

fmap :: (v -> Result w) -> Result v -> Result w
fmap f (Left v) = f v
fmap _ (Right e) = Right e

map :: (v -> w) -> Result v -> Result w
map f (Left v) = (value . f) v
map _ (Right e) = Right e

orTry :: Result v -> Result v -> Result v
orTry (Left v) _ = Left v
orTry (Right e) x = x

onFailure :: Result v -> String -> Result v
onFailure (Left v) _ = Left v
onFailure (Right e) s = Right (s:e)

import Debug.Trace
import PathfinderFirst.Duel (duel, findWinner)
import PathfinderFirst.Internal
import PathfinderFirst.SmallLightningElemental
import PathfinderFirst.Wolf
import Roll

main :: IO()
main = print $! flatten $! fmap (\(a,b) -> traceShow (hp$getEnt a, hp$getEnt b) $!findWinner 100 $ duel (constant a) (constant b) ) (mix wolf smallLightningElemental)

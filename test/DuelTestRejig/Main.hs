import PathfinderFirst.Duel ( duel, findWinner )
import Debug.Trace
import Roll
import PathfinderFirst.Internal
import PathfinderFirst.Wolf
import PathfinderFirst.SmallLightningElemental

main :: IO()
main = print $! flatten $! fmap (\(a,b) -> traceShow (hp$getEnt a, hp$getEnt b) $!findWinner 100 $ duel (constant a) (constant b) ) (mix wolf smallLightningElemental)

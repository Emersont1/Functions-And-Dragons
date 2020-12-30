import PathfinderFirst.Duel (duel, findWinner)
import PathfinderFirst.SmallLightningElemental
import PathfinderFirst.Wolf

main :: IO()
main = putStrLn $ show $! findWinner 100 $ duel wolf smallLightningElemental

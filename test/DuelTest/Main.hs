import PathfinderFirst.Duel ( duel, findWinner )
import PathfinderFirst.Wolf
import PathfinderFirst.SmallLightningElemental

main :: IO()
main = putStrLn $ show $! findWinner 100 $ duel wolf smallLightningElemental

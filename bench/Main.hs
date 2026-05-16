module Main where

import Test.Tasty.Bench
import BatchBench (benchmarks)

main :: IO ()
main = defaultMain benchmarks

module Main where

import Tarefa1_Spec
import Tarefa2_Spec
import Tarefa3_Spec
import Tarefa4_Spec
import Tarefa5_Spec
import Test.HUnit



main :: IO ()
main = runTestTTAndExit $ TestList [test_suite_01, test_suite_02,testesTarefa3,testesTarefa4]

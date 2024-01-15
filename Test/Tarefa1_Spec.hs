module Tarefa1_Spec where

import LI12324
import Tarefa1
import Test.HUnit

mapaTeste01 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
           [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Alcapao, Alcapao, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ]


-- Não coincide com uma plataforma/alçapão
jogadorTeste01 = 
    Personagem 
      (4,2)
      Jogador
      (300,500)     -- posição
      Norte
      (90,90)     -- tamanho
      False
      True          --resalta
      3
      20
      (True,10.0)  

-- Coincide com um alçapão
jogadorTeste02 = 
    Personagem 
      (800,300)
      Jogador
      (800,300)     -- posição
      Norte
      (90,90)     -- tamanho
      False 
      False         -- ressalta
      1
      20
      (True,10.0)
  
-- Coincide com uma plataforma 
jogadorTeste03 = 
    Personagem 
      (800,300)
      Jogador
      (200,300)    -- posição
      Norte
      (90,90)    -- tamanho
      False 
      False        -- ressalta
      1
      20
      (True,10.0)

inimigoTeste01 = 
    Personagem 
      (420,200)
      Jogador
      (320,500)    -- posição  
      Norte
      (90,90)    -- tamanho
      False 
      True         --ressalta
      1
      20
      (True,10.0)

test_suite_01  :: Test
test_suite_01  = TestLabel "Testes da Tarefa1" $ test (tcolisoesParede ++ tcolisoesPersonagens)


tcolisoesParede = [
    TestCase (assertBool "Teste um personagem que coincide com um alçapão " (colisoesParede mapaTeste01 jogadorTeste02)),
    TestCase (assertBool "Teste um personagem que coincide com uma plataforma " (colisoesParede mapaTeste01 jogadorTeste03)),
    TestCase (assertBool "Teste um personagem que não coincide com uma plataforma/alçapão " (colisoesParede mapaTeste01 jogadorTeste01))
   ]

tcolisoesPersonagens = [
    TestCase (assertBool "Testa um inimigo que coincide com um personagem" (colisoesPersonagens inimigoTeste01 jogadorTeste01)),
    TestCase (assertBool "Testa um inimigo que não coincide com um personagem" (colisoesPersonagens inimigoTeste01 jogadorTeste02))
    ]
    
module Tarefa2_Spec where 

import LI12324
import Tarefa2
import Test.HUnit

mapaTeste01 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
           [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ] 

mapaTeste02 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
           [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Vazio]
           ] 

mapaTeste03 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
           [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Plataforma, Plataforma, Vazio, Escada, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma]
           ]

jogadorTeste01 = 
    Personagem 
      (400,200)
      Jogador
      (400,200)
      Norte
      (150,150)
      False
      True
      3
      20
      (True,10.0)  

jogadorTeste02 = 
    Personagem 
      (800,300)
      Jogador
      (800,300)
      Norte
      (300,300)
      False 
      False  --ressalta
      1
      20
      (True,10.0)


inimigoTeste01 = 
    Personagem 
      (420,200)
      MacacoMalvado
      (420,200)
      Norte
      (150,150)
      False 
      True  --ressalta
      1
      20
      (True,10.0)

fantasmaTeste01 = 
    Personagem 
      (400,300)
      Fantasma
      (400,300)
      Norte
      (150,150)
      False 
      False  --ressalta
      1
      20
      (True,10.0)

fantasmaTeste02 = 
    Personagem 
      (600,300)
      Fantasma
      (600,300)
      Norte
      (150,150)
      False 
      False  --ressalta
      3
      20
      (True,10.0)

colecionavelTeste01 = ((Martelo),(300.0,150.0))

colecionavelTeste02 = ((Moeda),(300.0,500.0))

test_suite_02 :: Test
test_suite_02 = TestLabel "Testes da Tarefa2" $ test (tvressalta++tvinimigos++tvposicao++tvchao++tvfantasma++tvalcapao++tvplataforma++tvescada)

tvinimigos = [
    TestCase (assertBool "Teste para uma lista com 3 inimigos" (vinimigos [inimigoTeste01,fantasmaTeste01,fantasmaTeste02])),
    TestCase (assertBool "Teste para uma lista com apenas 1 inimigo " (vinimigos [fantasmaTeste01])),
    TestCase (assertBool "Teste para uma lista com apenas 0 inimigos " (vinimigos []))
    ]

tvressalta = [
    TestCase (assertBool "Teste para um inimigo e jogador com Ressalta True" (vressalta [inimigoTeste01] jogadorTeste01 )),
    TestCase (assertBool "Teste para um inimigo com Ressalta False" (vressalta [fantasmaTeste02,fantasmaTeste01] jogadorTeste01)),
    TestCase (assertBool "Teste para um jogador com Ressalta False" (vressalta [inimigoTeste01] jogadorTeste02))
    ] 

tvposicao = [
    TestCase (assertBool "Testa um inimigo cuja posição coincide com o jogador" (vposicao [fantasmaTeste02, inimigoTeste01] jogadorTeste01)),
    TestCase (assertBool "Testa um inimigo cuja poisção não coincide com o jogador" (vposicao [fantasmaTeste01] jogadorTeste02))
    ]

tvchao = [
    TestCase (assertBool "Testa se um mapa com chão" (vchao mapaTeste01)),
    TestCase (assertBool "Testa um mapa sem chão" (vchao mapaTeste02))
    ]

tvfantasma = [
    TestCase (assertBool "Testa um fantasma com 1 vida" (vfantasma [inimigoTeste01,fantasmaTeste01])),
    TestCase (assertBool "Testa um fantasma com 3 vidas" (vfantasma [fantasmaTeste01,fantasmaTeste02]))
    ]

tvalcapao = [
    TestCase (assertBool "Testa um jogador menor do que o tamanho de um alçapão" (valcapao mapaTeste01 jogadorTeste01)),
    TestCase (assertBool "Testa um jogador maior do que o tamanho de um alçapão" (valcapao mapaTeste01 jogadorTeste02))
    ]

tvplataforma = [
    TestCase (assertBool "Testa um fantasma, um colecionável e um jogador que não se encontram dentro de nenhuma plataforma" (vplataforma mapaTeste01 [fantasmaTeste01] [colecionavelTeste02] jogadorTeste01)),
    TestCase (assertBool "Testa um colecionável que se encontram dentro de uma plataforma" (vplataforma mapaTeste01 [fantasmaTeste01] [colecionavelTeste01] jogadorTeste01)),
    TestCase (assertBool "Testa um jogador que se encontram dentro de uma plataforma" (vplataforma mapaTeste01 [fantasmaTeste01] [colecionavelTeste02] jogadorTeste02)),
    TestCase (assertBool "Testa um inimigo que se encontra dentro de uma plataforma" (vplataforma mapaTeste01 [fantasmaTeste01,fantasmaTeste02] [colecionavelTeste02] jogadorTeste01))
    ] 

tvescada = [
    TestCase (assertBool "Testa um mapa com uma escada com uma extremidade plataforma e nenhuma alçapão" (vescada mapaTeste02)),
    TestCase (assertBool "Testa uma escada com  uma extremidade alçapão" (vescada mapaTeste01)),
    TestCase (assertBool "Testa uma escada cujas extremidades são o vazio" (vescada mapaTeste03))
    ] 


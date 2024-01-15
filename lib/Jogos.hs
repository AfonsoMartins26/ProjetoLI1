module Jogos where

import LI12324



-- comp : 16 por  altura : 20 

mapa1 :: Mapa 
mapa1 = Mapa  ((25,922),Este)
              (325,75)
              [
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma]
              ,[Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ]


mapa2:: Mapa 
mapa2 = Mapa ((400,900),Oeste)
             (325,25)
             [
              [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Plataforma]
             ,[Plataforma,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Vazio,Vazio,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ]

mapa3 :: Mapa
mapa3 = Mapa ((750,922),Oeste)
             (175,25)
             [
              [Plataforma,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
             ,[Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma,Plataforma,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Plataforma]
             ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ]


mapa4 :: Mapa
mapa4 = Mapa ((750,922),Este)
             (725,25)
             [ [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Alcapao,Alcapao,Alcapao,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Vazio,Vazio,Plataforma,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Vazio,Vazio,Plataforma,Plataforma,Vazio,Vazio,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Vazio,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
              ,[Plataforma,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Plataforma]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
             ]




jogo1 :: Jogo 
jogo1 = Jogo 
                mapa1 
                [fantasma5',fantasma4',fantasma3']
                [(Martelo,(375,925))]
                botquefazmapa1 

jogo2 :: Jogo 
jogo2 = Jogo 
                mapa2 
                [fantasma1',fantasma2',fantasma3']
                [(Martelo,(470,520))]
                botquefazmapa2


jogo3 :: Jogo 
jogo3 = Jogo 
                mapa3 
                [fantasma6',fantasma7',fantasma9']
                [(Martelo,(400,775))]
                botquefazmapa3


jogo4 :: Jogo 
jogo4 = Jogo 
            mapa4
            [fantasma10',fantasma11',fantasma12']
            [(Martelo,(125,775))]
            botquefazmapa4 



-- fantasmas para os jogos 
fantasma1' :: Personagem
fantasma1' = (Personagem (1,0) Fantasma (200, 375) Este (4,4) False  True 1 0 (False,0)) 

fantasma2' :: Personagem
fantasma2' = (Personagem (2,0) Fantasma (700, 525) Oeste (4,4) False  True 1 0 (False,0))

fantasma3' :: Personagem
fantasma3' = (Personagem (1,0) Fantasma (400, 725) Este (4,4) False  True 1 0 (False,0))

fantasma4' :: Personagem
fantasma4' = (Personagem (3,0) Fantasma (550, 425) Este (50,50) False  True 1 0 (False,0))

fantasma5' :: Personagem
fantasma5' = (Personagem (3,0) Fantasma (100, 575) Este (50,50) False  True 1 0 (False,0))

fantasma6' :: Personagem
fantasma6' = (Personagem (1,0) Fantasma (100,925) Este (4,4) False  True 1 0 (False,0))  

fantasma7' :: Personagem
fantasma7' = (Personagem (1,0) Fantasma (675, 325) Este (4,4) False  True 1 0 (False,0)) 

fantasma8' :: Personagem
fantasma8' = (Personagem (1,0) Fantasma (100, 475) Este (4,4) False  True 1 0 (False,0)) 

fantasma9' :: Personagem
fantasma9' = (Personagem (1,0) Fantasma (200, 775) Oeste (4,4) False  True 1 0 (False,0))

fantasma10' :: Personagem
fantasma10' = (Personagem (1,0) Fantasma (575, 325) Este (4,4) False  True 1 0 (False,0))

fantasma11' :: Personagem
fantasma11' = (Personagem (1,0) Fantasma (400, 775) Este (4,4) False  True 1 0 (False,0))

fantasma12' :: Personagem
fantasma12' = (Personagem (1,0) Fantasma (100, 475) Oeste (4,4) False  True 1 0 (False,0))

fantasma13' :: Personagem
fantasma13' = (Personagem (1,0) Fantasma (100,725) Oeste (4,4) False  True 1 0 (False,0))

botquefazmapa1 :: Personagem 
botquefazmapa1 = (Personagem (10,0) Jogador (25,915) Este (40,53) False False  3 0 (False,0.0)) 

botquefazmapa2 :: Personagem 
botquefazmapa2 = (Personagem (10,0) Jogador (400,922) Oeste (40,53) False False  3 0 (False,0.0))

botquefazmapa3 :: Personagem 
botquefazmapa3  = (Personagem (10,0) Jogador (700,922) Oeste (40,53) False False 3 0 (False,0.0))

botquefazmapa4 :: Personagem 
botquefazmapa4  = (Personagem (10,0) Jogador (575,922) Oeste (40,53) False False 3 0 (False,0.0))
module Tarefa6 where
        
import LI12324

import Tarefa1

import Tarefa3

import Jogos

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


-- bot que resolve os mapas 

botquefazmapa1 :: Personagem 
botquefazmapa1 = (Personagem (1,0) Jogador (50,1000) Este (80,40) False False  20 0 (False,0.0)) 



fazmapa1 :: Personagem -> Mapa -> [Acao] -> Personagem 
fazmapa1 botquefazmapa1 mapa1 [] = botquefazmapa1
fazmapa1 botquefazmapa1 mapa1 listademovimentosmapa1 = fazacao' botquefazmapa1 mapa1  listademovimentosmapa1
 
fazacao' :: Personagem -> Mapa -> [Acao] -> Personagem  
fazacao' p1 mapa1 (h:t) = fazacao' (movimentaconsoanteacoes p1 h) mapa1 t  

listademovimentosmapa1 =  [AndarDireita,AndarDireita,AndarDireita,Subir,Subir,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Subir,Subir,AndarEsquerda,
                            AndarEsquerda,AndarEsquerda,Subir,Subir,AndarDireita,AndarDireita,
                            AndarDireita,Saltar,Subir,Subir,AndarDireita ]

fazmapa2 :: Personagem -> Mapa -> [Acao] -> Personagem 
fazmapa2 botquefazmapa2 mapa2 [] = botquefazmapa2
fazmapa2 botquefazmapa2 mapa2 listademovimentosmapa2 = fazacao' botquefazmapa2 mapa2  listademovimentosmapa2



listademovimentosmapa2 = [AndarDireita,AndarDireita,AndarDireita,Subir,Subir,AndarEsquerda,
                        AndarEsquerda,AndarEsquerda,AndarEsquerda,Saltar,Subir,Subir,
                        AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,
                        Subir,Subir,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,
                        Subir,Subir]

botquefazmapa2 :: Personagem 
botquefazmapa2 = (Personagem (1,0) Jogador (400,1000) Oeste (80,40) False False  20 0 (False,0.0)) 

listademovimentosmapa3 = [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,Subir,Subir,
                        AndarDireita,AndarDireita,Subir,Subir ,AndarEsquerda,AndarEsquerda,AndarEsquerda,Saltar,AndarEsquerda,AndarEsquerda,AndarEsquerda,
                        Subir,Subir,AndarEsquerda,Saltar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Subir,Subir,
                        Saltar,AndarDireita,AndarDireita,AndarDireita,Subir,Subir,AndarDireita,Saltar,AndarDireita,AndarDireita,AndarDireita,AndarDireita,
                        AndarDireita,AndarDireita]


fazmapa3 :: Personagem -> Mapa -> [Acao] -> Personagem 
fazmapa3 botquefazmapa3 mapa3 [] = botquefazmapa3
fazmapa3 botquefazmapa3 mapa3 listademovimentosmapa3 = fazacao' botquefazmapa3 mapa3  listademovimentosmapa3



botquefazmapa3 :: Personagem 
botquefazmapa3  = (Personagem (1,0) Jogador (750,950) Este (80,40) False False 20 0 (False,0.0)) 

movimentaconsoanteacoes :: Personagem -> Acao -> Personagem 
movimentaconsoanteacoes p acao 
        |acao == Saltar = p  {posicao = (x,y+50)}
        |acao == Subir = p  {posicao = (x,y+50)}
        |acao == Descer = p {posicao = (x,y-50)}
        |acao == AndarDireita = p {posicao = (x+50,y)}
        |otherwise = p {posicao = (x-50,y)}
                where (x,y) = posicao p 


-- Adicionar novos tipos de inimigos ou blocos de mapa

mapaportal :: Mapa 
mapaportal = Mapa  ((50,1000),Este)
              (400,300)
              [
               [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Portal,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
              ,[Portal,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ,[Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio]
              ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
              ]

passaporPortal :: Bloco -> Personagem -> Personagem 
passaporPortal bloco p1       
                |bloco  == Portal =  if coincideHitboxes (hitboxp (posicao p1) (tamanho p1)) (hitboxbloco bloco (50,750) (50,50) ) == True 
                                     then p1 {posicao =(50,250) }
                                     else p1 
                |otherwise = p1 
                 
hitboxbloco :: Bloco -> Posicao -> Tamanho -> Hitbox
hitboxbloco bloco (x,y) (c,l) = ((x,y+l),(x+c,y))


coordenadasportal1mapa1 :: Posicao
coordenadasportal1mapa1 = (50,750)

coordenadasportal2mapa1 :: Posicao 
coordenadasportal2mapa1 = (50,250)

-- Cheatcodes
{-
vidainfinita:: Personagem -> Event -> Personagem
vidainfinita p1 (EventKey (Char 'v') Down _ _) = p1 {vida =1000}

marteloinfinito :: Personagem -> Event -> Personagem
marteloinfinito p1 (EventKey (Char 'm') Down _ _) = p1 {aplicaDano = (True,300000)}

pontosinfinitos :: Personagem -> Event -> Personagem 
pontosinfinitos p1 (EventKey (Char 'p') Down _ _) = p1 {pontos =9999999999}

autowinjogo1 :: Personagem -> Event -> Personagem 
autowinjogo1 p1 (EventKey (Char '1') Down _ _) = p1 { posicao = (400,300)}

autowinjogo2 :: Personagem -> Event -> Personagem 
autowinjogo2 p2 (EventKey (Char '2') Down _ _) = p2 { posicao =(400,300) }

autowinjogo3 :: Personagem -> Event -> Personagem 
autowinjogo3 p3 (EventKey (Char '3') Down _ _)  = p3 {posicao =(750,200) }
-}
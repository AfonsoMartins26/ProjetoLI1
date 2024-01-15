{-
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Afonso Paulo Martins <a106931@alunos.uminho.pt>
              Salomé Pereira Faria <a108487@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game 
import LI12324
import Graphics.Gloss
-- import Tarefa3 


-- | atualiza o jogo consoante as acoes de todos os personagens 
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesinimigos acao (Jogo m lista colecionaveis jogador) = Jogo m (aplicarAcao lista acoesinimigos ) colecionaveis (acaofeita jogador acao) 

-- | funcao que ao receber uma tecla e um personagem devolve a acao que vai corresponder a cada tecla   
teclapressionada :: Personagem  -> Event -> Maybe Acao 
teclapressionada (Personagem velo Jogador (x, y) direc (x1,y1) False False vida pontos1 (False,0.0)) (EventKey (SpecialKey KeySpace)Down _ _ )   = Just Saltar  
teclapressionada (Personagem velo Jogador (x, y) direc (x1,y1) True False vida pontos1 (False,0.0)) (EventKey (Char 'w') Down _ _) = Just Subir  
teclapressionada (Personagem velo Jogador (x, y) direc (x1,y1) True False vida pontos1 (False,0.0))  (EventKey (Char 's') Down _ _) = Just  Descer
teclapressionada (Personagem velo Jogador (x, y) direc (x1,y1) False False vida pontos1 (False,0.0))  (EventKey (Char 'a') Down _ _) = Just AndarEsquerda 
teclapressionada (Personagem velo Jogador (x, y) direc (x1,y1) False False vida pontos1  (False,0.0))  (EventKey (Char 'd') Down _ _) = Just AndarDireita 

 

-- |movimento dos fantasmas 
movinimigovel :: Personagem  ->  Maybe Acao -> Personagem
movinimigovel (Personagem velo1 Fantasma (x, y) direc (x1,y1) False True vida pontos1 (False,0.0))  (Just AndarDireita) = (Personagem (3,0) Fantasma (x, y) Este (x1,y1) False True vida 0 (False,0.0))
movinimigovel (Personagem velo1 Fantasma (x, y) direc (x1,y1) False True vida pontos1 (False,0.0))  (Just AndarEsquerda) = (Personagem (-3,0) Fantasma (x, y) Oeste (x1,y1) False True vida 0 (False,0.0))
movinimigovel (Personagem velo1 Fantasma (x, y) direc (x1,y1) False True vida pontos1 (False,0.0))  (Just Subir) = (Personagem (0,-3) Fantasma (x, y) direc (x1,y1) False True vida 0 (False,0.0))
movinimigovel (Personagem velo1 Fantasma (x, y) direc (x1,y1) False True vida pontos1 (False,0.0))  (Just Descer) = (Personagem (0,3) Fantasma (x, y) direc (x1,y1) False True vida 0 (False,0.0))

 

atualizaJogador :: Maybe Acao -> Jogo -> Jogo
atualizaJogador mo (Jogo m e c j) = (Jogo m e c (acaofeita j mo))


-- |funcao  que altera a velocidade e direcao de um personagem ao  fazer uma acao 
acaofeita :: Personagem -> Maybe Acao -> Personagem 
acaofeita p (Just Subir) = p { velocidade = (0,-10), direcao = Norte }
acaofeita p (Just Descer) = p { velocidade = (0,10), direcao = Sul }
acaofeita p (Just AndarDireita) = p { velocidade = (10,0), direcao = Este }
acaofeita p (Just AndarEsquerda) = p { velocidade = (-10,0), direcao = Oeste }
acaofeita p@Personagem {velocidade = (vx,vy)} (Just Saltar) = p { velocidade = (vx,-60)}
acaofeita p (Just Parar) = p { velocidade = (0,0)}
acaofeita p Nothing = p
                             

-- | aplica as acoes dos inimigos a todos os inimigos 
aplicarAcao :: [Personagem] -> [Maybe Acao] -> [Personagem]
aplicarAcao [] _ = []
aplicarAcao l [] = l
aplicarAcao (h1:t1) (h:t)    
     |(tipo h1) == Fantasma = movinimigovel h1 h : aplicarAcao t1 t  
     |otherwise = aplicarAcao t1 t 


{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Afonso Paulo Martins <a106931@alunos.uminho.pt>
              Salomé Pereira Faria <a108487@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324 

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m (Personagem _ _ p d t _ _ _ _ _)  = coincideHitboxesblocos (hitboxp p t) (hitboxBlocos m (0,0) (tamanhoBloco (m)))

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = coincideHitboxes (hitboxp (posicao p1) (tamanho p1)) (hitboxp (posicao p2) (tamanho p2))

-- | Função que determina a hitbox de um personagem, dados a sua posição e tamanho
hitboxp :: Posicao -> (Double,Double) -> Hitbox  
hitboxp (x,y) (c,l) = ((x-(c/2),y+l/2), (x+c/2,y-l/2))


-- | Função Auxiliar que determina as hitboxes das Plataformas/Açapões do mapa
hitboxBlocos :: Mapa -> Posicao -> (Double,Double) -> [Hitbox]
hitboxBlocos (Mapa _ _ []) _ _ = []
hitboxBlocos m@(Mapa a b (h:t)) (x,y) (c,l) = (auxBlocos h  (x,y) (c,l)) ++ (hitboxBlocos (Mapa a b t) (x,y+l) (c,l))


-- | Função auxiliar que dado uma lista de blocos, a posição e o tamanho de um bloco dá as hitboxes das Plataformas/Alçapões dessa lista
auxBlocos :: [Bloco] -> Posicao -> (Double,Double) -> [Hitbox]
auxBlocos [] _ _ = []
auxBlocos (h:t) (x,y) (c,l) 
 | h == Portal ||h == Plataforma || h == Alcapao = ((x,y+l),(x+c,y)): auxBlocos t (x+c,y) (c,l)
 | otherwise = auxBlocos t (x+c,y) (c,l)
 
 
-- | Função que determina o tamanho de um bloco da matriz mapa por coordenadas, tendo em conta as dimensões da janela
tamanhoBloco :: Mapa -> (Double,Double)
tamanhoBloco (Mapa _ _ m@(h:t)) = (800/fromIntegral(length h), 1000/fromIntegral(length m)) -- !! MUDAR de acordo como tamanho da janela


-- | Função Auxiliar que testa se duas hitboxes se intersetam
coincideHitboxes :: Hitbox -> Hitbox -> Bool
coincideHitboxes ((x,y),(a,b)) ((p,s), (c,d))
 | x<(-400) || a>400 || b<(-500) || y> 500 || p<(-400) || c>400 || d<(-500) || s>500 = True -- ^ limites laterais !! MUDAR de acordo com dimensões da janela 
 | p<=x && x<=c && d<=y && y<=s = True
 | p<=a && a<=c && d<=b && b<=s = True
 | p<=a && a<=c && d<=y && y<=s = True
 | p<=x && x<=c && d<=b && b<=s = True 
 | x<=p && p<=a && b<=s && s<=y = True  -- ^ caso o personagem seja maior que o Inimigo/Bloco
 | otherwise = False 


-- | Função Auxiliar que testa se a hitbox do jogador interseta as hitboxes das plataformas/alçapões do mapa 
coincideHitboxesblocos :: Hitbox -> [Hitbox] -> Bool
coincideHitboxesblocos p [] = False
coincideHitboxesblocos ((x,y),(a,b)) (((p,s), (c,d)):t)
 | coincideHitboxes ((x,y),(a,b)) (((p,s), (c,d))) = True
 | otherwise = coincideHitboxesblocos ((x,y),(a,b)) t

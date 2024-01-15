{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Afonso Paulo Martins <a106931@alunos.uminho.pt>
              Salomé Pereira Faria <a108487@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where


import LI12324
import Tarefa1
import Jogos


valida :: Jogo -> Bool
valida (Jogo m i c j) =    vressalta i j 
                        && vposicao i j
                        && vinimigos i 
                        && vplataforma m i c j
                        && valcapao m j
                        && vfantasma i
                        && vchao m
                        && vescada m
                        

-- | verifica se um mapa tem chão                     
vchao :: Mapa -> Bool 
vchao (Mapa _ _ m) = auxchao (last m)

auxchao :: [Bloco] -> Bool
auxchao [] = True
auxchao (h:t)
 | h /= Plataforma = False
 | otherwise = auxchao t

-- | verifica se todos os inimigos têm a propriedade ressalta "True" e o jogador "False"
vressalta :: [Personagem] -> Personagem -> Bool
vressalta [] p = not (ressalta p)
vressalta (p@(Personagem _ i _ _ _ _ r _ _ _ ):t) p1@(Personagem _ Jogador _ _ _ _ re _ _ _ ) = ressalta p && vressalta t p1
 

-- | verifica se a posição inicial de um jogador coincide com a p.i. de um inimigo
vposicao :: [Personagem] -> Personagem -> Bool
vposicao [] p = True 
vposicao (p:t) j@(Personagem _ Jogador po _ _ _ _ _ _ _ ) 
 | colisoesPersonagens p j  = False
 | otherwise = vposicao t j


-- | verifica se há pelo menos dois inimigos
vinimigos :: [Personagem] -> Bool
vinimigos l = length l >= 2


-- | verifica se os fantasmas têm exatamente uma vida
vfantasma :: [Personagem] -> Bool
vfantasma [] = True
vfantasma (h:t) 
 | tipo h == Fantasma  &&  vida h /= 1 = False
 | otherwise = vfantasma t

-- | verifica se um personagem ou coloecinavel interseta uma plataforma/alçapão
vplataforma :: Mapa -> [Personagem] -> [(Colecionavel, Posicao)] -> Personagem -> Bool 
vplataforma m [] [] j = not (colisoesParede m j)
vplataforma m (h:t) [] j  
 |colisoesParede m h = False
 |otherwise = vplataforma m t [] j
vplataforma m [] ((c,p):r) j 
 | intersetaColeBloco m p (tamanhoBloco m) = False
 | otherwise = vplataforma m [] r j
vplataforma m (h:t) ((c,p):r) j
 |colisoesParede m h || intersetaColeBloco m p (tamanhoBloco m) = False       
 | otherwise = vplataforma m t r j


intersetaColeBloco :: Mapa -> Posicao -> (Double,Double) -> Bool
intersetaColeBloco m p t = coincideHitboxesblocos (hitboxp p t) (hitboxBlocos m (0,0) (tamanhoBloco (m)))   


-- | verifica se o jogador é menos largo do que um alçapão 
valcapao :: Mapa -> Personagem -> Bool 
valcapao m@(Mapa _ _ ((h:t):r)) p = tamanho p <= (2*fst(tamanhoBloco m),2*snd(tamanhoBloco m)) 


-- | verifica se pelo menos uma das extremidades da escada é uma plataforma e não podem terminar/começar em alçapões

vescada :: Mapa -> Bool 
vescada (Mapa _ _ m) = auxescada (transposta m)

transposta :: [[Bloco]] -> [[Bloco]]
transposta m@(h:t) = [ [(m !! j) !! i | j <- [0..l-1] ] | i <- [0..c-1]]
    where (l,c) = (length m, length h)

auxescada :: [[Bloco]] -> Bool
auxescada [] = True
auxescada (x:t)
 | (last a == Alcapao) || alAux b = False
 | (last a /= Plataforma) && plaAux b = False
 | otherwise = continuaLinha (dropWhile (==Escada) b) && auxescada t
 where (a,b) = (span (/=Escada) x) 
       plaAux b 
        | filter (/=Escada) b == [] = False
        | otherwise = head(filter (/=Escada) b ) /=Plataforma
       alAux b 
        | filter (/=Escada) b ==[] = False
        | otherwise = head(filter (/=Escada) b ) == Alcapao
       continuaLinha c 
        |snd (span (/=Escada) c) == [] = True
        | otherwise = auxescada (c:[])
{-
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Afonso Paulo Martins <a106931@alunos.uminho.pt>
              Salomé Pereira Faria <a108487@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where


import Tarefa1 
import LI12324 
import Data.Maybe
import Jogos






movimenta :: Semente -> Tempo ->  Jogo -> Jogo
movimenta seed t j@Jogo {mapa= m ,jogador= p }    = 
                              (movePersonagens
                                (limitesBlocos
                                    (efeitoGravidade   
                                      (mudaEmEscada 
                                          (retiraAlcapao 
                                             (perderVida 
                                                (colecionavelRecolhido j
                                                   )))))))


--(mudaEmEscada game))

-- | - - - - - - - COLECIONÁVEIS - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-- | Função que atualiza os colecionaveis e o jogador

colecionavelRecolhido :: Jogo -> Jogo 
colecionavelRecolhido (Jogo m i c j) = (Jogo m i (tiraColecionavel j c m) (recolhercolecionavel j c m)) 


-- | Função que dá a hitbox de um colecionável

hitboxcloecionavel ::  Posicao ->  Tamanho -> Hitbox
hitboxcloecionavel (x2,y2) (c,l) = ((x2-(c/2),y2+l/2), (x2+c/2,y2-l/2)) 



-- | funcao que verifica se o Jogador apanhou o colecionavel e o retira ao fim de 10 ticks

recolhercolecionavel :: Personagem -> [(Colecionavel,Posicao)] -> Mapa -> Personagem 
recolhercolecionavel p [] m = p
recolhercolecionavel p@Personagem{pontos=pontos, aplicaDano = (ativo,n)} ((c,(x,y)):t) m  
 | ativo =  aplicaDanoFalse p
 | coincideHitboxes (hitboxp (posicao p) (tamanho p)) (hitboxcloecionavel (x,y) (tamanhoBloco m)) = case c of Moeda -> p{pontos = (pontos +100)} 
                                                                                                              Martelo -> p{aplicaDano=(True,10)} 
 | otherwise = recolhercolecionavel p t m 

-- | Função que retira os colécionáveis do mapa caso estes sejam recolhidos 

tiraColecionavel ::  Personagem -> [(Colecionavel,Posicao)] -> Mapa -> [(Colecionavel,Posicao)]
tiraColecionavel p [] m = []
tiraColecionavel p ((c,(x,y)):t) m  
 | coincideHitboxes (hitboxp (posicao p) (tamanho p)) (hitboxcloecionavel (x,y) (tamanhoBloco m)) = tiraColecionavel p t m 
 | otherwise = (c,(x,y)) : tiraColecionavel p t m 

-- | Funçao que tira o colecionável após 10 segundos
aplicaDanoFalse :: Personagem -> Personagem
aplicaDanoFalse p@Personagem{aplicaDano= (ativo,n)} 
 | ativo = p{aplicaDano= (True,n-1)}
 | otherwise = p{aplicaDano= (False, 0)}


-- | - - - - - - - - - - - -  DANO - - - - - - - - - - - - - - - - - - - - - - - - - - 

-- | Função que atualiza o dano dos jogadores consoante o uso ou não de martelo

perderVida :: Jogo -> Jogo
perderVida  (Jogo m i c j) 
 | fst (aplicaDano j) = (Jogo m (inimigoAtingido j i) c j) 
 | otherwise = (Jogo m i c j) 


-- | Função que retira o tempo ao à função aplicaDano, consoante a passagem do mesmo

tempoDano :: Personagem -> Tempo -> Personagem
tempoDano p@(Personagem _ _ _ _ _ _ _ _ _ (True,t)) tempo
 | t == 0 = p{aplicaDano= (False,0)}
 | otherwise = p{aplicaDano=(True,t-tempo)}


{- |Função que retira vida de um inimigo, caso este coincida com o jogador armado 
      Se um inimigo ficar com 0 vidas, a função desenhaEstado deixará de o representar no mapa e de poder aplicar dano ao jogador -}

inimigoAtingido :: Personagem -> [Personagem] -> [Personagem]
inimigoAtingido p [] = []
inimigoAtingido p@(Personagem _ _ (x,y) _ (c,l) _ _ _ _ (True,n) ) (f@(Personagem _ _ _ _ _ _ _ v _ _):t) 
 | colisoesPersonagens p{posicao = (x-(c/2),y) ,tamanho=(2*c,l)} f = f{vida = v-1,aplicaDano=(False,0.0)} : (inimigoAtingido p t)
 | otherwise = f : (inimigoAtingido p t)


-- | funcao que verifica se o fanstasma seu dano ao personagem tendo em conta as Hitboxs 

jogadorantigidoinimigos :: Personagem -> [Personagem]-> Personagem
jogadorantigidoinimigos p1 [] = p1 
jogadorantigidoinimigos p1 (h:t) = jogadorantigidoinimigos (jogadorantigido p1 h) t 



jogadorantigido :: Personagem -> Personagem ->Personagem
jogadorantigido p@Personagem{vida=v} f
 | colisoesPersonagens p f  = p{vida = v-1}
 | otherwise = p









-- | - - - - - - - - - ALÇAPÃO - - - - - - - - - - - - - - - - - - - - - - - - - -

{- | Função que um Jogador ao passar por um alcapao, havendo colisao das hitboxes do bloco e do Jogador troca esse alcapao por vazio 
       Se não houver colisao das hitboxs do bloco e do Jogador não acontece nada ao alcapao -}

retiraAlcapao :: Jogo -> Jogo 
retiraAlcapao (Jogo m i c j) = (Jogo (selecionaAlcapao j m) i c j)


-- | Função que integra a lista de blocos mudade no mapa 

selecionaAlcapao :: Personagem -> Mapa -> Mapa
selecionaAlcapao p m@(Mapa a b ma)
 | coincide == Nothing = m
 | otherwise = (Mapa a b (desapareceAlcapao (fromJust coincide) ma (0,0) (tamanhoBloco m)))
   where coincide = coincideHitboxesAlcapao (hitboxp (posicao p) (tamanho p)) (hitboxAlcapao m (0,0) (tamanhoBloco m))


-- | Função que identifica a linha do alçapão pisado, atualizando a lista de blocos do mapa

desapareceAlcapao :: Posicao -> [[Bloco]] -> Posicao -> (Double,Double) -> [[Bloco]]
desapareceAlcapao (x,y) (h:t) (a,b) (c,l)
 | y == b = (mudaBloco h x 0 c) : t
 | otherwise =  h : desapareceAlcapao (x,y) t (a,b+l) (c,l)


-- | Função que muda um Alçapão pisado por um jogador para um bloc Vazio

mudaBloco :: [Bloco] -> Double -> Double -> Double -> [Bloco]
mudaBloco (h:t) x a c 
 | x == a = Vazio : t
 | otherwise = h : mudaBloco t x (a+c) c


-- | Função que se um a hitbox de um jogador coincidir com a hitbox de um alçapão dá a coordenada do canto superior esquerdo nesse alçapão 

coincideHitboxesAlcapao :: Hitbox -> [Hitbox] -> Maybe Posicao 
coincideHitboxesAlcapao p [] = Nothing
coincideHitboxesAlcapao ((x,y),(a,b)) (((p,s), (c,d)):t)
 | coincideHitboxes ((x,y),(a,b)) (((p,s), (c,d))) = Just (p,d)
 | otherwise = coincideHitboxesAlcapao ((x,y),(a,b)) t 


-- | Função Auxiliar que determina as hitboxes dos Alçapões do mapa

hitboxAlcapao :: Mapa -> Posicao -> (Double,Double) -> [Hitbox]
hitboxAlcapao (Mapa _ _ []) _ _ = []
hitboxAlcapao m@(Mapa a b (h:t)) (x,y) (c,l) = (auxAlcapao h  (x,y) (c,l)) ++ (hitboxAlcapao (Mapa a b t) (x,y+l) (c,l))


-- | Função auxiliar que dado uma lista de blocos, a posição e o tamanho de um bloco dá as hitboxes dos Alçapões dessa lista

auxAlcapao :: [Bloco] -> Posicao -> (Double,Double) -> [Hitbox]
auxAlcapao [] _ _ = []
auxAlcapao (h:t) (x,y) (c,l) 
 | h == Alcapao = ((x,y+l),(x+c,y)): auxAlcapao t (x+c,y) (c,l)
 | otherwise = auxAlcapao t (x+c,y) (c,l)

 
     

       




-- | - - - - - - - -  - - GRAVIDADE - - - - - - - - - - - - - - - - - - - - - -  

-- | funcao que aplica a gravidade ao Jogador quando se encontra no ar 


efeitoGravidade :: Jogo -> Jogo 
efeitoGravidade (Jogo m@(Mapa _ _ b) i c j) =(Jogo m ( map (gravidadedaPersonagem' b) i) c (gravidadedaPersonagem' b j))


chaoVazio :: Personagem -> [[Bloco]] -> Bool
chaoVazio Personagem {posicao = (x,y)} b = (b !! (floor ((y/50)+1) )) !! (floor (x/50)) == Vazio

movePersonagensvazio :: Personagem ->  [[Bloco]] -> Personagem
movePersonagensvazio p@(Personagem (v1,v2) _ (x,y) _ _ _ _ _ _ _ )  b 
                  | chaoVazio p b == True =  p{posicao = ((max 0 (min 800 ((v1+x)/500))),(max 0 (min 1000 ((v2+y)/2500))))}   
                  |otherwise = moveJogador p           

gravidadedaPersonagem' :: [[Bloco]] -> Personagem -> Personagem
gravidadedaPersonagem' b p@Personagem {velocidade = (vx,vy), posicao = (x,y)}
 | chaoVazio p b && vy /= snd gravidade = p{velocidade = (vx,(snd gravidade))}
 | otherwise                            = p



-- | - - - - - - - - - - COLISÕES - - - - - - - - - - - - - - - - - - - 

limitesBlocos :: Jogo -> Jogo 
limitesBlocos (Jogo m i c j) = (Jogo m (restricoesInimigos m i) c (restricoesMovimentos m j) )


restricoesInimigos :: Mapa -> [Personagem] -> [Personagem]
restricoesInimigos m [] = [] 
restricoesInimigos m (h:t) = restricoesMovimentos m h : restricoesInimigos m t
                          

restricoesMovimentos :: Mapa -> Personagem -> Personagem
restricoesMovimentos m@(Mapa _ _ b) p@Personagem{emEscada = e}
 | colisoesLaterais (hitboxp (posicao p) (tamanho p)) =  restringirCoordenadas  p
 | e = p
 | colisoesParede m p = coincideLimites p (hitboxp (posicao p) (tamanho p)) (hitboxBlocos m (0,0) (tamanhoBloco (m)))
 | otherwise = p



restringirCoordenadas ::   Personagem -> Personagem
restringirCoordenadas  p1 =  p1 {posicao =(max 0 (min 800 x), max 0 (min 980 y))}
  where
    (x,y) = posicao p1

coincideEsquerdaBaixo :: Hitbox -> Hitbox -> Bool
coincideEsquerdaBaixo ((x,y),(a,b)) ((p,s), (c,d)) = p<=x && x<=c && d<=y && y<=s 

coincideEsquerdaCima :: Hitbox -> Hitbox -> Bool
coincideEsquerdaCima((x,y),(a,b)) ((p,s), (c,d)) = p<=x && x<=c && d<=b && b<=s 

coincideDireitaBaixo :: Hitbox -> Hitbox -> Bool
coincideDireitaBaixo ((x,y),(a,b)) ((p,s), (c,d)) = p<=a && a<=c && d<=y && y<=s 


coincideDireitaCima :: Hitbox -> Hitbox -> Bool
coincideDireitaCima ((x,y),(a,b)) ((p,s), (c,d)) = p<=a && a<=c && d<=b && b<=s 




colisoesLaterais :: Hitbox -> Bool
colisoesLaterais ((x,y),(a,b)) = x<=0 || a>=800 || b<=10 || y>=990   



coincideLimites :: Personagem -> Hitbox -> [Hitbox] -> Personagem
coincideLimites pe _ [] = pe
coincideLimites pe ((x,y),(a,b)) (((p,s), (c,d)):t)
 | coincideDireitaCima ((x,y),(a,b)) (((p,s), (c,d))) = pe{velocidade=(-5,5)}
 | coincideEsquerdaCima ((x,y),(a,b)) (((p,s), (c,d))) = pe{velocidade=(5,5)}
 | coincideDireitaBaixo ((x,y),(a,b)) (((p,s), (c,d))) = pe{velocidade=(-5,-5)}
 | coincideEsquerdaBaixo ((x,y),(a,b)) (((p,s), (c,d))) =pe{velocidade=(5,-5)}
 | otherwise = coincideLimites pe ((x,y),(a,b)) t 

                                   




 
-- | - - - -  - CONSIDERAÇÕES ESCADAS - - - - - - - - - 

mudaEmEscada :: Jogo -> Jogo 
mudaEmEscada (Jogo m i c j) = (Jogo m i c (jogadorEmEscada m j))




-- | - - - - - - MOVIMENTOS INIMIGOS - - - - - - - - - - - - - - - - - - - - - - - 

-- | Verifica se um jogador está numa escada

jogadorEmEscada :: Mapa ->  Personagem -> Personagem
jogadorEmEscada m p
 | colisoesEscada m p = p{emEscada= True}
 | otherwise = p{emEscada= False}


colisoesEscada :: Mapa -> Personagem -> Bool
colisoesEscada m (Personagem _ _ p d t _ _ _ _ _)  = coincideHitboxesblocos (hitboxp p t) (hitboxBlocosEscada m (0,0) (tamanhoBloco (m)))

hitboxBlocosEscada :: Mapa -> Posicao -> (Double,Double) -> [Hitbox]
hitboxBlocosEscada (Mapa _ _ []) _ _ = []
hitboxBlocosEscada m@(Mapa a b (h:t)) (x,y) (c,l) = (auxBlocosEscada h  (x,y) (c,l)) ++ (hitboxBlocosEscada (Mapa a b t) (x,y+l) (c,l))


auxBlocosEscada :: [Bloco] -> Posicao -> (Double,Double) -> [Hitbox]
auxBlocosEscada [] _ _ = []
auxBlocosEscada (h:t) (x,y) (c,l) 
 | h == Escada = ((x,y+l),(x+c,y)): auxBlocosEscada t (x+c,y) (c,l)
 | otherwise = auxBlocosEscada t (x+c,y) (c,l)


emCimaEscada :: Mapa -> Personagem -> [Hitbox] -> Tamanho -> Bool
emCimaEscada _ _ [] _ = False
emCimaEscada m p1 (((x,y),(a,b)):t) (c,l) = coincideHitboxes (hitboxp (posicao p1) (tamanho p1)) ((x,y),(a,b-l)) && jogadorEmEscada m p1 == p1 {emEscada = False}   && not (chegouAoFimDaEscada' p1 m) || emCimaEscada m p1 t (c,l)

emBaixoEscada ::Mapa -> Personagem -> [Hitbox] -> Tamanho -> Bool
emBaixoEscada _ _ [] _ = False
emBaixoEscada m p1 (((x,y),(a,b)):t) (c,l) = coincideHitboxes (hitboxp (posicao p1) (tamanho p1)) ((x,y+l),(a,b)) && jogadorEmEscada m p1 == p1 {emEscada = False} && not (chegouAoFimDaEscada' p1 m) || emBaixoEscada m p1 t (c,l)




-- | - - - - - - MOVIMENTOS INIMIGOS - - - - - - - - - - - - - - - - - - - - - - - 


movePersonagens :: Jogo -> Jogo  
movePersonagens (Jogo m e c j) = (Jogo m (moveenemies e) c (moveJogador j))

moveJogador :: Personagem -> Personagem 
moveJogador p@(Personagem (v1,v2) _ (x,y) _ _ _ _ _ _ _ )= p{posicao = (max 75 (min 725 ((v1+x))),max 25 (min 975 ((v2+y))))}



moveenemies :: [Personagem] -> [Personagem]
moveenemies [] = []
moveenemies (p:t) = moveJogador p : moveenemies t


-- | Se um fantasma tiver em cima ou embaixo de uma escada sobe ou desce
estaNumaEscadaCima :: Mapa ->Posicao -> Tamanho -> Personagem -> Bool
estaNumaEscadaCima m (x,y) (c,l) p1 = or (map (naescadaCima  (hitboxp (posicao p1) (tamanho p1))) (hitboxBlocosEscada m (x,y) (c,l)  )) -- or faz com que ou tudo seja verdadeiro para dar verdadeiro 
                            where naescadaCima ((x1,x2),(y1,y2)) ((x3,x4),(y3,y4)) = x1 >= x3 && x2 <= x4 && y1 ==  y4-- se houver um ocorrencia de falso dá falso 


estaNumaEscadaBaixo :: Mapa -> Posicao -> Tamanho -> Personagem -> Bool
estaNumaEscadaBaixo m (x,y) (c,l) p1 = or (map (naEscadaBaixo (hitboxp (posicao p1) (tamanho p1))) (hitboxBlocosEscada m (x,y) (c,l)  )) 
                            where naEscadaBaixo ((x1,x2),(y1,yV2)) ((x3,x4),(y3,y4)) = x1 >= x3 && x2 <= x4 && y1 == y3 







velocidadeSobeEscada :: (Double, Double)
velocidadeSobeEscada = (0.0,-10.0)

velocidadeDesceEscada :: (Double, Double)
velocidadeDesceEscada = (0.0,10.0)



chegouAoFimDaEscada' :: Personagem -> Mapa -> Bool
chegouAoFimDaEscada' p1 mapa@(Mapa _ _ m) 
                | (emEscada p1) == True && (velocidade p1) == velocidadeSobeEscada && emCimaEscada mapa p1 (hitboxBlocosEscada mapa (0,0) (tamanhoBloco (mapa))) (tamanhoBloco mapa) = True
                | (emEscada p1) == True && (velocidade p1) == velocidadeDesceEscada && emBaixoEscada mapa p1 (hitboxBlocosEscada mapa (0,0) (tamanhoBloco (mapa))) (tamanhoBloco mapa)= True
                | otherwise = False





mudaposicao :: Posicao -> Personagem -> Personagem 
mudaposicao (x,y) p1 = p1 {posicao = (x,y)}

mudavelocidade :: Velocidade -> Personagem -> Personagem  
mudavelocidade (vx,vy) p1 = p1 { velocidade = (vx,vy)}

emsescada :: Bool -> Personagem -> Personagem 
emsescada bool p1 
        | bool == True = p1 {aplicaDano = (True, 10.0)}
        |otherwise = p1 {aplicaDano = (False, 0.0)}

-- | Muda as posições dos personagens num jogo atualizado






         

vaisairdaPlataforma :: Personagem -> Mapa -> Bool 
vaisairdaPlataforma p1 mapa 
            | (estanumbloco (hitboxpersonagem p1 ) (hitboxPlataforma mapa (0,0) (50,50))) = (estanumbloco proximoLimPersonagem ( hitboxVazio mapa (0,0) (50,50))) 
            | otherwise = False 
    where (vx,vy) = velocidade  p1 
          (x,y) = posicao p1 
          (x1,y1) = (x+vx,y+vy) 
          proximoLimPersonagem = hitboxpersonagem (mudaposicao (x1,y1) p1)  



estanomeiodaPlataforma :: Mapa -> Personagem -> Bool 
estanomeiodaPlataforma mapa p1 = any (checkeestanomeiodoBloco (hitboxpersonagem p1)) (hitboxPlataforma mapa (0,0) (50,50) )
                where (((e,f),(g,h)):t) = hitboxPlataforma mapa (0,0) (50,50)
                      ((a,b),(c,d)) = hitboxpersonagem p1 
                      checkeestanomeiodoBloco ((a,b),(c,d))((e,f),(g,h)) = a -10 == e && b + 10 == f && c == h 


estanumbloco ::  Hitbox -> [Hitbox] ->  Bool 
estanumbloco ((a,b),(c,d)) listadehitbox  = coincideHitboxesblocos ((a,b),(c,d)) listadehitbox

hitboxpersonagem :: Personagem -> Hitbox 
hitboxpersonagem p1 = hitboxp (posicao p1)(tamanho p1)

hitboxPlataforma :: Mapa -> Posicao -> (Double,Double) -> [Hitbox]
hitboxPlataforma (Mapa _ _ []) _ _ = []
hitboxPlataforma m@(Mapa a b (h:t)) (x,y) (c,l) = (auxBlocosPlataforma h  (x,y) (c,l)) ++ (hitboxPlataforma (Mapa a b t) (x,y+l) (c,l))


auxBlocosPlataforma :: [Bloco] -> Posicao -> (Double,Double) -> [Hitbox]
auxBlocosPlataforma [] _ _ = []
auxBlocosPlataforma (h:t) (x,y) (c,l) 
 | h == Plataforma = ((x,y+l),(x+c,y)): auxBlocosPlataforma t (x+c,y) (c,l)
 | otherwise = auxBlocosPlataforma t (x+c,y) (c,l)

hitboxVazio :: Mapa -> Posicao -> (Double,Double) -> [Hitbox]
hitboxVazio (Mapa _ _ []) _ _ = []
hitboxVazio m@(Mapa a b (h:t)) (x,y) (c,l) = (auxBlocosVazio h  (x,y) (c,l)) ++ (hitboxVazio (Mapa a b t) (x,y+l) (c,l))


auxBlocosVazio :: [Bloco] -> Posicao -> (Double,Double) -> [Hitbox]
auxBlocosVazio [] _ _ = []
auxBlocosVazio (h:t) (x,y) (c,l) 
 | h == Vazio = ((x,y+l),(x+c,y)): auxBlocosVazio t (x+c,y) (c,l)
 | otherwise = auxBlocosVazio t (x+c,y) (c,l)

inimigosmovemAleatorio :: Int -> Mapa -> Personagem -> [Maybe Acao]
inimigosmovemAleatorio seed mapa inimigo@(Personagem (vx,vy) tipo (x,y) direcao (c,l) escada _ vida _ _) 
            | tipo == MacacoMalvado = [Nothing]
            | mod numeroaleatorio 10 == 0 && estaNumaEscadaBaixo mapa (x,y) (c,l) inimigo && not escada && estanomeiodaPlataforma mapa inimigo = [Just Subir]
            | mod numeroaleatorio 10 == 0 && estaNumaEscadaCima mapa (x,y) (c,l) inimigo &&  not escada && estanomeiodaPlataforma mapa inimigo = [Just Descer] 
            | even numeroaleatorio && chegouAoFimDaEscada' inimigo mapa && estanomeiodaPlataforma mapa inimigo = [Just AndarDireita]
            | odd numeroaleatorio && chegouAoFimDaEscada' inimigo mapa && estanomeiodaPlataforma mapa inimigo = [Just AndarEsquerda]
            | direcao == Oeste && (colisoesParede mapa (mudaposicao (x',y') inimigo) || vaisairdaPlataforma inimigo mapa ) = [Just AndarDireita]
            | direcao == Este && (colisoesParede mapa (mudaposicao (x',y') inimigo) || vaisairdaPlataforma inimigo mapa ) = [Just AndarEsquerda]
            | direcao == Este && not escada = [Just AndarDireita]
            | direcao == Oeste && not escada = [Just AndarEsquerda]
            | escada && vy > 0 && not (chegouAoFimDaEscada' inimigo mapa) = [Just Descer] 
            | escada && vy < 0   = [Just Subir] 
        where numeroaleatorio = head (geraAleatorios seed 1)
              (x',y') = (x+vx,y+vy)  

inimigosmovemacao1' :: [Int] -> Mapa -> [Personagem]-> [Maybe Acao] -> [Maybe Acao]
inimigosmovemacao1' [] mapa [] [] = []
inimigosmovemacao1' (mv:movimentos) mapa e@((fantasma'):t) (ma:msa) = inimigosmovemAleatorio mv mapa fantasma' ++ inimigosmovemacao1'  movimentos mapa t msa 


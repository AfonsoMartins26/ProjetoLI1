module Tarefa5 where 

import LI12324
import Jogos
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import GHC.Float
import Data.Maybe
import Tarefa1
import Tarefa3
import Tarefa4 
import System.Random



-- | - - - - -  DATA TYPES - - - - - - - - -


data Menu = EmJogo Int | MenuInicial Opcoes | MenuVitoria Opcoes  | MenuMapas Int | MenuDerrota Opcoes  deriving (Show)
data Opcoes = Jogar | Sair | Mapas | Normal deriving (Show,Eq)

type ImagensMenu = [Picture]

type Imagens = [[Picture]] 



data PrimateKong = PrimateKong { jogo :: Jogo,                        -- ^ Jogo atual
                                 nivel :: Int,                        -- ^ Indicador a usar para selecionar o jogo na lista de jogos quando se anvança de nível
                                 modo :: Menu,                        -- ^ Modo atual de jogo (Ex: MenuInicial,MenuPausa, EmJogo, ...)
                                 imagens :: Imagens,                  -- ^ Lista de pictures impotadas pelo Gloss.Juicy
                                 movimentoJogador :: Maybe Acao,      -- ^ Acao atual do Jogador
                                 movimentoInimigos :: [Maybe Acao],   -- ^ Movimentos atuais dos inimigos
                                 seed :: Semente,                     -- ^ Seed para gerar numeros aleatorios
                                 movimentosAleatorios :: [Int],       -- ^ Lista de numeros aleatorios gerados com a seed, para definir movimentos aleatórios aos inimigos
                                 tempo :: Float,                      -- ^ Unidade de tempo 
                                 nextLevel :: Bool,                    -- ^ Indicador de que o jogador chegou à posição de mudença de nível de um mapa
                                 vidas :: Int                         -- ^ Indicador que retira a vida de um personagem, de acordo com o número de vezes que o mapa deu reset pela perda de uma vida
                               }


 


-- | - - - - - - - PLAY - - - - - - - -

inicioJogo :: IO ()
inicioJogo = do 
    pic <- getImages 
    seed <- geraSemente
    play  dm
          corFundo
          frameRate
          (estadoInicial seed pic)
          desenha 
          reage
          reageTempo     -- alterei o nome da funcao ? 
          
-- | - - - - - - SEMENTE - - - - - - - - 

geraSemente :: IO Semente
geraSemente = do seed <- randomRIO(1,100)
                 return seed

-- | - - - - - - IMAGENS - - - - - - - - -


getImages :: IO [[Picture]]
getImages = do       
    -- | MenuInicial 
        Just miJogar <- loadJuicyPNG ("imagens/menu/menuIJogar.png")
        Just miMapas <- loadJuicyPNG ("imagens/menu/menuIMapas.png")
        Just miSair <- loadJuicyPNG ("imagens/menu/menuISair.png")
    -- | MenuDerrota
        Just mdNormal <- loadJuicyPNG ("imagens/menu/menuDerrotaNormal.png")
        Just mdSair <- loadJuicyPNG ("imagens/menu/menuDerrotaSair.png")
    -- | MenuVitoria 
        Just mvNormal <- loadJuicyPNG ("imagens/menu/menuVitoriaNormal.png")
        Just mvSair <- loadJuicyPNG ("imagens/menu/menuVitoriaSair.png")
    -- | MenuPausa 
        Just mpRetomar <- loadJuicyPNG ("imagens/menu/MenuPausaRetomar.png")
        Just mpRestart <- loadJuicyPNG ("imagens/menu/MenuPausaRestart.png")
        Just mpSair <- loadJuicyPNG ("imagens/menu/MenuPausaSair.png")
    -- | MenuMapas
        Just menuMapas1 <- loadJuicyPNG ("imagens/menu/menuMapas1.png")
        Just menuMapas2 <- loadJuicyPNG ("imagens/menu/menuMapas2.png")
        Just menuMapas3 <- loadJuicyPNG ("imagens/menu/menuMapas3.png")
    -- | Jogador
        Just marioAndarD <- loadJuicyPNG ("imagens/jogador/marioAndarD.png")
        Just marioAndarE  <- loadJuicyPNG ("imagens/jogador/marioAndarE.png")
        Just marioCostasD <- loadJuicyPNG ("imagens/jogador/marioCostasD.png")
        Just marioCostasE  <- loadJuicyPNG ("imagens/jogador/marioCostasE.png")
        Just marioMarteloArD <- loadJuicyPNG ("imagens/jogador/marioMarteloArD.png")
        Just marioMarteloArE <- loadJuicyPNG ("imagens/jogador/marioMarteloArD.png")
        Just marioMarteloChaoD <- loadJuicyPNG ("imagens/jogador/marioMarteloChaoD.png")
        Just marioMarteloChaoE <- loadJuicyPNG ("imagens/jogador/marioMarteloChaoE.png")
        Just marioSaltarD <- loadJuicyPNG ("imagens/jogador/marioSaltarD.png")
        Just marioSaltarE <- loadJuicyPNG ("imagens/jogador/marioSaltarE.png")
        Just marioD <- loadJuicyPNG ("imagens/jogador/marioD.png")
        Just marioE <- loadJuicyPNG ("imagens/jogador/marioE.png")
    -- | Inimigos
        Just fantasmaRD <- loadJuicyPNG ("imagens/inimigos/fantasmaRD.png")
        Just fantasmaRE <- loadJuicyPNG ("imagens/inimigos/fantasmaRE.png")
        Just fantasmaFD <- loadJuicyPNG ("imagens/inimigos/fantasmaFD.png")
        Just fantasmaFE <- loadJuicyPNG ("imagens/inimigos/fantasmaFE.png")
        Just macaco <- loadJuicyPNG ("imagens/inimigos/macacoFrente.png")
    -- | Blocos 
        Just plataforma <- loadJuicyPNG ("imagens/blocos/plataforma.png")
        Just alcapao <- loadJuicyPNG ("imagens/blocos/alcapao.png")
        Just escada <- loadJuicyPNG ("imagens/blocos/escada.png")
        Just vazio <- loadJuicyPNG ("imagens/blocos/vazio.png")
        Just portal <- loadJuicyPNG ("imagens/blocos/portalfinal.png")
    -- | Colecionáveis
        Just martelo <- loadJuicyPNG ("imagens/colecionaveis/martelo.png")

        let pic = [[plataforma, alcapao, escada, vazio, portal ],
                   [marioD, marioE, marioAndarD, marioAndarE, marioSaltarD, marioSaltarE, marioCostasD, marioCostasE, marioMarteloArD, marioMarteloArE, marioMarteloChaoD, marioMarteloChaoE],
                   [fantasmaRD, fantasmaRE, fantasmaFD, fantasmaFE, macaco],
                   [miJogar,miMapas,miSair,mpRetomar,mpRestart,mpSair,mdNormal, mdSair, menuMapas1,menuMapas2,menuMapas3, mvNormal, mvSair],
                   [martelo]
                  ]
        return pic



-- | - - - - - DM - CORFUNDO - FRAMERATE - - - - - 


dm :: Display 
dm = InWindow "Donkey Kong" (1000, 1000) (500,0)

corFundo :: Color 
corFundo = black

frameRate :: Int 
frameRate = 40 




-- | - - - - - - -PrimateKong - - - - - - - 

-- | EstadoInincial de quaando se abre o jogo
estadoInicial :: Semente ->[[Picture]] -> PrimateKong 
estadoInicial seed pic = (PrimateKong jogo1 0 (MenuInicial Jogar) pic (Just Parar) [Just AndarDireita,Just AndarEsquerda,Just AndarDireita] seed (geraAleatorios seed (length(inimigos jogo2))) 0 False 1) -- não sei o que está errado aqui 
                                                                                 
jogos :: [Jogo]
jogos = [jogo1, jogo2, jogo3]


-- | - - - - - - DESENHA - - - - - - - - 

-- | Função que desenha o estado 

desenha :: PrimateKong -> Picture
desenha (PrimateKong j n  (MenuInicial o) i mj mi s ma t nl v) = desenhaMenuI (MenuInicial o) (i!!3)
desenha (PrimateKong j n  (MenuDerrota o) i mj mi s ma t nl v) = desenhaDerrota (MenuDerrota o) (i!!3)
desenha (PrimateKong j n  (MenuVitoria o) i mj mi s ma t nl v) = desenhaVitoria (MenuVitoria o) (i!!3)
desenha (PrimateKong j n (MenuMapas o) i mj mi s ma t nl v )= desenhaMenuMapas (MenuMapas o) (i!!3)
desenha (PrimateKong (Jogo m e c j) n (EmJogo ni) i mj mi s ma t nl v) = Pictures $ [desenhaMapa m (i!!0)] ++ [desenhaJogador j mj (i!!1)] ++ desenhaEnemies e (i!!2) ++ (desenhaColecionaveis c (i!!4)) ++ [desenhaVida j]
 

-- | Desenha o menu inicial

desenhaMenuI :: Menu -> [Picture] -> Picture
desenhaMenuI (MenuInicial o) menu
 | o == Jogar = Translate (0) (0)  $ menu!!0
 | o == Mapas = Translate (0) (0)  $ menu!!1
 | o == Sair = Translate (0) (0)  $ menu!!2



-- | Desenha o menu de escolha dos mapas

desenhaMenuMapas :: Menu -> [Picture] -> Picture
desenhaMenuMapas (MenuMapas o) menu 
 | o == 1 = menu!!8
 | o == 2 = menu!!9
 | o == 3 = menu!!10
 

-- | Desenha o menu de derrota 

desenhaDerrota :: Menu -> [Picture] -> Picture
desenhaDerrota (MenuDerrota o) menu
 | o == Normal =  menu!!6
 | o == Sair =  menu!!7

-- | Desenha o menu de derrota 

desenhaVitoria:: Menu -> [Picture] -> Picture
desenhaVitoria (MenuVitoria o) menu
 | o == Normal =  menu!!11
 | o == Sair =  menu!!12


-- | Desenha os colecionáveis no mapa 

desenhaColecionaveis :: [(Colecionavel,Posicao)] -> [Picture] -> [Picture]
desenhaColecionaveis [] _ = []
desenhaColecionaveis ((c,(x,y)):t) i 
 | c == Martelo = Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!0) : desenhaColecionaveis t i

-- | Desenha a vida de um jogador 
desenhaVida :: Personagem -> Picture
desenhaVida p@Personagem{vida =v } = Scale 0.4 0.2 $ Translate (-1225) 500 $ Color white $ Text ("Vida: "  ++ (show v)) 


-- | Desenha o jogador no mapa 

desenhaJogador :: Personagem -> Maybe Acao -> [Picture] -> Picture
desenhaJogador (Personagem _ Jogador (x,y) direcao _ emEscada _ _ _ (c,t) ) mo i
 | mo == (Just Parar) && direcao == Este && c == True =         Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!8)
 | mo == (Just Parar) && direcao == Oeste && c == True =        Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!9)
 | mo == (Just Parar) && direcao == Este && not(emEscada) =     Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!0)
 | mo == (Just Parar) && direcao == Oeste && not(emEscada) =    Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!1)
 | mo == (Just AndarDireita) && c == False =                    Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!2) 
 | mo == (Just AndarEsquerda) && c == False =                   Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!3) 
 | mo == (Just Saltar) && direcao == Este  =                    Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!4)
 | mo == (Just Saltar) && direcao == Oeste =                    Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!5)
 | mo == (Just Parar) && emEscada  =                            Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!6)
 | mo == (Just Parar) && direcao == Sul && not(emEscada) =      Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!7)
 | mo == (Just Descer) && emEscada =                            Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!7)
 | mo == Nothing && direcao == Este =                           Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!4)
 | mo == Nothing && direcao == Oeste =                          Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!5)
 | mo == (Just AndarDireita) && c == True =                     Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!10)
 | mo == (Just AndarEsquerda) && c == True =                    Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!11)
 | otherwise =                                                  Translate ((double2Float x)-300)  ((-double2Float y)+500) (i!!6)
 


-- | Desenha o Mapa

desenhaMapa :: Mapa -> [Picture] -> Picture
desenhaMapa (Mapa _ _ m) pic = Pictures $ juntaListas(desenhaMatriz m 0 pic)


-- | Junta as as pictures em listas de listas

desenhaMatriz :: [[Bloco]] -> Double -> [Picture] -> [[Picture]]
desenhaMatriz [] _ _ = [[]]
desenhaMatriz (h:t) y pic = (desenhaLinha h (0,y) pic ) :(desenhaMatriz t (y-50) pic)

-- | Desenha uma linha do mapa

desenhaLinha :: [Bloco] -> (Double,Double) -> [Picture] -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (h:t) (x,y) pic = desenhaPeca h (x,y) pic :  desenhaLinha t (x+50,y) pic


-- | Desenha os blocos 

desenhaPeca :: Bloco -> (Double,Double) -> [Picture] -> Picture
desenhaPeca h (x,y) pic
 | h == Plataforma = Translate ((double2Float x)-275) ((double2Float y)+475)  (pic !! 0) 
 | h == Alcapao = Translate ((double2Float x)-275) ((double2Float y)+475)  (pic !! 1) 
 | h == Escada = Translate ((double2Float x)-275) ((double2Float y)+475)  (pic !! 2) 
 | h == Vazio = Translate ((double2Float x)-275) ((double2Float y)+475)  (pic !! 3)
 | h == Portal = Translate ((double2Float x)-275) ((double2Float y)+475)  (pic !! 4) 


-- | Junta a matriz as linhas numa lista só

juntaListas :: [[Picture]] -> [Picture]
juntaListas [[]] = []
juntaListas (h:t) = h ++ juntaListas t



-- | Desenha os inimigos 

desenhaEnemies :: [Personagem] -> [Picture] -> [Picture]
desenhaEnemies [] _ = []
desenhaEnemies ((Personagem _ tipo (x,y) direcao _ _ _ vida _ _ ):t) i
 | tipo == Fantasma && vida <= 0 = desenhaEnemies t i
 | tipo == Fantasma && direcao == Este =  Translate ((double2Float x)-300) ((-double2Float y)+500) (i!!0) : desenhaEnemies t i
 | tipo == Fantasma && direcao == Oeste = Translate ((double2Float x)-300) ((-double2Float y)+500) (i!!1) : desenhaEnemies t i
 | tipo == MacacoMalvado =                Translate ((double2Float x)-300) ((-double2Float y)+500) (i!!4) : desenhaEnemies t i






-- | - - - - - - INPUT- - - - - - - - - -

-- | Função que muda o estado do jogo de acordo com o input

reage :: Event -> PrimateKong -> PrimateKong

-- | INPUT MENU INICIAL
reage (EventKey (Char 's')Down _ _) e@PrimateKong {modo = MenuInicial Jogar} =  e {modo = MenuInicial Mapas}
reage (EventKey (Char 's')Down _ _) e@PrimateKong {modo = MenuInicial Mapas} =  e {modo = MenuInicial Sair}
reage (EventKey (Char 's')Down _ _) e@PrimateKong {modo = MenuInicial Sair} = e {modo = MenuInicial Jogar}
reage (EventKey (Char 'w')Down _ _) e@PrimateKong {modo = MenuInicial Sair} = e {modo = MenuInicial Mapas}
reage (EventKey (Char 'w')Down _ _) e@PrimateKong {modo = MenuInicial Mapas} = e {modo = MenuInicial Jogar} 
reage (EventKey (Char 'w')Down _ _) e@PrimateKong {modo = MenuInicial Jogar} = e {modo = MenuInicial Sair}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuInicial Jogar, nivel= n} = e {modo = EmJogo (n+1)} 
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuInicial Mapas} = e {modo = MenuMapas 1}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuInicial Sair} = error "Fim de Jogo"

-- | INPUT MENU MAPAS
reage (EventKey (Char 'd')Down _ _) e@PrimateKong {modo = MenuMapas 1} = e {modo = MenuMapas 2}
reage (EventKey (Char 'd')Down _ _) e@PrimateKong {modo = MenuMapas 2} = e {modo = MenuMapas 3}
reage (EventKey (Char 'a')Down _ _) e@PrimateKong {modo = MenuMapas 3} = e {modo = MenuMapas 2}
reage (EventKey (Char 'a')Down _ _) e@PrimateKong {modo = MenuMapas 2} = e {modo = MenuMapas 1}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuMapas 1} = e {jogo= jogo1, modo = EmJogo 1}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuMapas 2} = e {jogo= jogo2, modo = EmJogo 2}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuMapas 3} = e {jogo= jogo3, modo = EmJogo 3}
reage (EventKey (Char 'q') Down _ _) e@PrimateKong {modo = MenuMapas n} = e {modo = MenuInicial Mapas}


-- | INPUT MENUU DERROTA
reage (EventKey (Char 'w')Down _ _) e@PrimateKong {modo = MenuDerrota Normal} = e {modo = MenuDerrota Sair}
reage (EventKey (Char 's')Down _ _) e@PrimateKong {modo = MenuDerrota Sair} = e {modo = MenuDerrota Normal}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuDerrota Sair, seed = seed, imagens = pic} = (estadoInicial seed pic)

-- | INPUT MENUU DERROTA
reage (EventKey (Char 'w')Down _ _) e@PrimateKong {modo = MenuVitoria Normal} = e {modo = MenuVitoria Sair}
reage (EventKey (Char 's')Down _ _) e@PrimateKong {modo = MenuVitoria Sair} = e {modo = MenuVitoria Normal}
reage (EventKey (SpecialKey KeyEnter)Down _ _) e@PrimateKong {modo = MenuVitoria Sair, seed = seed, imagens = pic} = (estadoInicial seed pic)

-- | INPUT JOGO 
reage (EventKey (Char 'q') Down _ _) e@PrimateKong {modo = EmJogo n} = e {modo = MenuInicial Jogar}
reage (EventKey (Char 'w') Down _ _) e@PrimateKong {modo = EmJogo n, jogo = Jogo {jogador = Personagem {emEscada = True}}} = e {movimentoJogador = Just Subir}
reage (EventKey (Char 'w') Down _ _) e@PrimateKong {modo = EmJogo n, jogo = Jogo {jogador = Personagem {emEscada = False}}} = e {movimentoJogador = Just Saltar}
reage (EventKey (Char 's') Down _ _) e@PrimateKong {modo = EmJogo n, jogo = Jogo {jogador = Personagem {emEscada = True}}} =e {movimentoJogador = Just Descer}
reage (EventKey (Char 's') Down _ _) e@PrimateKong {modo = EmJogo n, jogo = Jogo {jogador = Personagem {emEscada = False}}} = e {movimentoJogador = Just Parar}
reage (EventKey (Char 'd') Down _ _) e@PrimateKong {modo = EmJogo n} = e {movimentoJogador = Just AndarDireita}
reage (EventKey (Char 'a') Down _ _) e@PrimateKong {modo = EmJogo n} = e {movimentoJogador = Just AndarEsquerda}
reage (EventKey _ Up _ _ ) e = e {movimentoJogador = Just Parar}


reage _ e = e

-- mudaposicaoEstado :: PrimateKong -> PrimateKong
-- mudaposicaoEstado (PrimateKong game n (EmJogo ni) pic mj mi seed t ) = (PrimateKong (movimentaeatualiza mi mj seed (float2Double t) game) n (EmJogo ni) pic mj mi seed t)


 

-- | - - - - - - - - TEMPO - - - - - - - - 



reageTempo :: Float ->  PrimateKong -> PrimateKong 

reageTempo f e@(PrimateKong 
                  j@(Jogo{mapa = m,inimigos = inimigos, jogador = p@Personagem{tamanho=(c,l),vida = v,emEscada=emE,aplicaDano=(ativo,tp) }})
                  ni 
                  (EmJogo nivel) 
                  i 
                  mo 
                  mi 
                  s 
                  movimentos 
                  t 
                  nextLevel 
                  vida) 

 | v == 0 =    (PrimateKong 
                  j 
                  ni 
                  (MenuDerrota Normal) 
                  i 
                  mo 
                  mi 
                  s 
                  movimentos 
                  t 
                  nextLevel 
                  vida)


 | colisoesInimigos p inimigos && not(ativo)= 
               (PrimateKong 
                  (tiraVida (jogos!!ni) vida)
                  ni 
                  (EmJogo nivel) 
                  i 
                  mo 
                  mi 
                  s 
                  movimentos 
                  0
                  False 
                  (vida+1) ) 
 
 | nextLevel && nivel == 3 = e {modo = MenuVitoria Normal } 
 | nextLevel = mudaNivel jogos e 

 | otherwise = 
               (PrimateKong 
                  (atualiza mi mo (movimenta s 1 (tiratempomartelo (t+1) j )))
                  ni 
                  (EmJogo nivel) 
                  i 
                  mo 
                  (inimigosmovemacao1' movimentos (mapa j) (inimigos ) mi)
                  (s+1)
                  (geraAleatorios s (length inimigos))
                  (t+1)
                  (verificaNivel m p)
                  vida) 

reageTempo _ e = e



tiratempomartelo :: Float -> Jogo -> Jogo 
tiratempomartelo t (Jogo m ini col p )  = Jogo m ini col (tiratempomarteloaux t p)


tiratempomarteloaux :: Float -> Personagem -> Personagem
tiratempomarteloaux t p1 = if ativa == True 
                           then p1 { aplicaDano = (ativa,( n- ( float2Double t)))}
                           else p1 {aplicaDano = (False,n)}
                where (ativa,(n)) = aplicaDano p1 



colisoesInimigos :: Personagem -> [Personagem] -> Bool
colisoesInimigos _ [] = False
colisoesInimigos p (f:t) 
 | colisoesPersonagens p f = True
 | otherwise = colisoesInimigos p t


tiraVida :: Jogo -> Int ->  Jogo 
tiraVida (Jogo m i c j@Personagem{vida=v}) vida = (Jogo m i c j{vida= v-vida })



mudaNivel :: [Jogo] -> PrimateKong -> PrimateKong
mudaNivel jogos (PrimateKong j ni (EmJogo nivel) i mo mi s movimentos t nextLevel v) = (PrimateKong (jogos!!(ni+1)) (ni+1) (EmJogo (nivel+1)) i mo mi (s+1) movimentos (t+1) False v)

  

verificaNivel :: Mapa -> Personagem -> Bool
verificaNivel m@(Mapa _ f _) p = coincideHitboxes (hitboxp (posicao p) (tamanho p)) (hitboxp f (tamanhoBloco m))



 

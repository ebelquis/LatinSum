import Data.List (lookup)
import Text.Printf (printf)
import Control.Monad (replicateM)

-- definimos os tipos de dados e constantes
-- data: criar novos tipos de dados
-- type: colocar um sinonimo para um tipo de dado já existente

-- grupo é composto por celulas (coordenadas) adjacentes a uma soma
-- deriving: prepara este tipo de dado para ser printado (precisa ser convertido para string (show)
data Grupo = Grupo { celulas :: [(Int, Int)], somaAlvo :: Int } deriving (Show)

-- uma celula pode ser vazia, com uma dica do jogo (valor inicial) 
-- ou um valor que o jogador vai definir

-- deriving: prepara este tipo de dado para ser printado (Show) e comparado (Eq)
data Celula = Vazia | Dica Int | Valor Int deriving (Eq, Show)

-- tabuleiro é uma matriz de celulas
type Tabuleiro = [[Celula]]

n :: Int
-- no caso de o jogo ser 8 x 8 com 3 dicas em cada linha/coluna:
n = 5 -- os números a serem preenchidos vão de 1 a 5 (8 - 3)


-- inicialização dos dados do jogo (dicas/tabuleiro)

-- coordenada da celula e valor da "dica" que estará nela
dicasSoma :: [((Int, Int), Int)]
dicasSoma = [
    ((0,0), 4), ((0,2), 8), ((0,7), 9),
    ((1,1), 12), ((1,3), 18), ((1,4), 19),
    ((2,1), 20), ((2,3), 15), ((2,6), 17),
    ((3,2), 19), ((3,5), 13), ((3,7), 8),
    ((4,2), 24), ((4,5), 11), ((4,6), 11),
    ((5,0), 11), ((5,4), 16), ((5,6), 20),
    ((6,0), 10), ((6,3), 15), ((6,4), 14),
    ((7,1), 12), ((7,5), 10), ((7,7), 10)
    ]

-- definição de todas as celulas que geram a soma do valor de uma dica
todosOsGrupos :: [Grupo]
todosOsGrupos = 
    [ Grupo { celulas = [(0, 1), (1, 0)], somaAlvo = 4 }
    , Grupo { celulas = [(0, 1), (0, 3), (1, 2)], somaAlvo = 8 }
    , Grupo { celulas = [(0, 6), (1, 6), (1, 7)], somaAlvo = 9 }
    , Grupo { celulas = [(0, 1), (1, 0), (1, 2), (2, 0), (2, 2)], somaAlvo = 12 }
    , Grupo { celulas = [(0, 3), (0, 4), (1, 2), (2, 2), (2, 4)], somaAlvo = 18 }
    , Grupo { celulas = [(0, 3), (0, 4), (0, 5), (1, 5), (2, 4), (2, 5)], somaAlvo = 19 }
    , Grupo { celulas = [(1, 0), (1, 2), (2, 0), (2, 2), (3, 0), (3, 1)], somaAlvo = 20 }
    , Grupo { celulas = [(1, 2), (2, 2), (2, 4), (3, 3), (3, 4)], somaAlvo = 15 }
    , Grupo { celulas = [(1, 5), (1, 6), (1, 7), (2, 5), (2, 7), (3, 6)], somaAlvo = 17 }
    , Grupo { celulas = [(2, 2), (3, 1), (3, 3), (4, 1), (4, 3)], somaAlvo = 19 }
    , Grupo { celulas = [(2, 4), (2, 5), (3, 4), (3, 6), (4, 4)], somaAlvo = 13 }
    , Grupo { celulas = [(2, 7), (3, 6), (4, 7)], somaAlvo = 8 }
    , Grupo { celulas = [(3, 1), (3, 3), (4, 1), (4, 3), (5, 1), (5, 2), (5, 3)], somaAlvo = 24 }
    , Grupo { celulas = [(3, 4), (3, 6), (4, 4), (5, 5)], somaAlvo = 11 }
    , Grupo { celulas = [(3, 6), (4, 7), (5, 5), (5, 7)], somaAlvo = 11 }
    , Grupo { celulas = [(4, 0), (4, 1), (5, 1), (6, 1)], somaAlvo = 11 }
    , Grupo { celulas = [(4, 3), (4, 4), (5, 3), (5, 5), (6, 5)], somaAlvo = 16 }
    , Grupo { celulas = [(4, 7), (5, 5), (5, 7), (6, 5), (6, 6), (6, 7)], somaAlvo = 20 }
    , Grupo { celulas = [(5, 1), (6, 1), (7, 0)], somaAlvo = 10 }
    , Grupo { celulas = [(5, 2), (5, 3), (6, 2), (7, 2), (7, 3), (7, 4)], somaAlvo = 15 }
    , Grupo { celulas = [(5, 3), (5, 5), (6, 5), (7, 3), (7, 4)], somaAlvo = 14 }
    , Grupo { celulas = [(6, 1), (6, 2), (7, 0), (7, 2)], somaAlvo = 12 }
    , Grupo { celulas = [(6, 5), (6, 6), (7, 4), (7, 6)], somaAlvo = 10 }
    , Grupo { celulas = [(6, 6), (6, 7), (7, 6)], somaAlvo = 10 }
    ]

-- determinacao do que está em cada celula inicialmente (vazio ou dica)
tabuleiroLogico :: Tabuleiro
tabuleiroLogico = 
-- lookup para procurar coordenada (r, c) na lista de dicasSoma -> retorna Maybe que pode ser Nothing ou Just d
    [ [ case lookup (r,c) dicasSoma of
        -- Se a coordenada for encontrada (Just d), define como Dica e coloca o valor
        Just d -> Dica d
        -- se nao encontrar define como vazio
        Nothing -> Vazia
        -- List Comprehension para definir todos os r e c dentro do tabuleiro
        | c <- [0..7] ] 
    | r <- [0..7] ]


-- geracao de possibilidades (backtrap)

-- recebe : uma cordenada (0, 3) e seu possivel valor 4
-- testa para todos as outras posicoes se o valor nao esta presente nessa linha,
-- nem nessa coluna , se passar por todas as verficacoes retorna True ,
-- significando que a combinacao eh válida
validoNoGrupo :: [((Int, Int), Int)] -> Bool
validoNoGrupo [] = True
validoNoGrupo (((r,c), v):xs) =
    all (\((r',c'), v') -> v /= v' || (r /= r' && c /= c')) xs
    && validoNoGrupo xs

-- A versão usando List Comprehension
gerarPossibilidades :: Grupo -> [[Int]] 
gerarPossibilidades (Grupo cels alvo) =
    [ path 
    | path <- replicateM (length cels) [1..5]  -- gera todas as combinações possiveis de 1 a 5
    -- length cel = 2 , 1,1 1,2 1,3 1,4 1,5 ... 5,5 
    , sum path == alvo                         -- filtra combinacoes que a soma é igual ao alvo
    -- passaria 2,2 1,3 3,1 
    , validoNoGrupo (zip cels path)            -- ver se passa na regra de nao cruzar linha/coluna
    -- tenta colocar no tabuleiro e ve se nao da erro 
    ]



-- validacao e atualizacao no tabuleiro

-- verifica se é valido na linha. Recebe o tabuleiro, o indice da linha e o valor que queremos colocar
validoNaLinha :: Tabuleiro -> Int -> Int -> Bool
-- (tab !! r): !! vai pegar a linha de indice r dentro de tabuleiro
validoNaLinha tab r v = Valor v `notElem` (tab !! r)
-- retorna True se não existir elemento e false se existir

-- verifica se é valido na coluna. Recebe o tabuleiro, o indice da coluna e o valor que queremos colocar
validoNaColuna :: Tabuleiro -> Int -> Int -> Bool
-- [linha !! c | linha <- tab]: vai pegar o elemento de indice c dentro de cada linha do tabuleiro (p/ formar coluna)
validoNaColuna tab c v = Valor v `notElem` [linha !! c | linha <- tab]
-- retorna True se não existir elemento e false se existir


-- recebe o tabuleiro, uma coordenada especifica e o valor desta nova celula e retorna o tabuleiro com esta celula
atualizarTab :: Tabuleiro -> (Int, Int) -> Celula -> Tabuleiro
atualizarTab tab (r, c) novaCel =
    [ [ if i == r && j == c then novaCel else tab !! i !! j 
      | j <- [0..7] ] 
    | i <- [0..7] ]

-- recebe uma lista de coordenadas, uma lista de valores e uma lista de tabuleiros
tentarPreencher :: [(Int, Int)] -> [Int] -> Tabuleiro -> [Tabuleiro]
-- passo zero / caso de parada: quando encontra uma solucao valida, retorna somente ela
tentarPreencher [] [] tab = [tab]
-- tenta preencher com a primeira coordenada e valor das listas
tentarPreencher ((r,c):cs) (v:vs) tab =
    case tab !! r !! c of
        -- caso a celula ja estiver marcada com um valor
        Valor valExistente -> 
            -- se o valor existente for = ao que queremos tentar preencher, chama recursivamente o próximo, se não, retorna lista vazia
            if valExistente == v then tentarPreencher cs vs tab else []
        -- se a celula estiver vazia
        Vazia -> 
            -- verifica se valores sao validos nas linhas e colunas
            if validoNaLinha tab r v && validoNaColuna tab c v
            -- tenta preencher os próximos a partir do tabuleiro atualizado com esse valor
            then tentarPreencher cs vs (atualizarTab tab (r,c) (Valor v))
            -- se nao for, retorna lista vazia (sem tabs validos)
            else []
        -- se for uma dica, retorna lista vazia (sem tabs validos)
        Dica _ -> []


-- resolve de fato, juntando tudo

-- recebe uma lista de grupos, o tabuleiro inicial e retorna uma lista de tabuleiros
resolver :: [Grupo] -> Tabuleiro -> [Tabuleiro]
-- passo zero / caso de parada: se lista estiver vazia é pq processamos tudo
resolver [] tab = [tab]
-- resolve o primeiro grupo 
resolver (g:gs) tab = 
    [ solucaoFinal 
    -- gera caminho(s) de possibilidades, combinação de numeros possiveis para este grupo
    | caminho <- gerarPossibilidades g
    -- gera um novo tab com o preenchimento das celulas deste grupo com o(s) caminho(s) encontrado(s0
    , novoTab <- tentarPreencher (celulas g) caminho tab
    -- chama recursivamente para os próximos grupos com este novo tabuleiro
    , solucaoFinal <- resolver gs novoTab
    -- se algum dos passos falhar, vai retornar [], indicando que o tab n existe
    ]


-- imprime e executa tudo

-- funcao para facilitar print das celulas
imprimirCelula :: Celula -> String
imprimirCelula Vazia = "  . "
imprimirCelula (Dica v) = printf "[%2d]" v
imprimirCelula (Valor v) = printf "  %d " v

main = do
    putStrLn "--- RESOLVENDO TABULEIRO ---"
    
    let solucoes = resolver todosOsGrupos tabuleiroLogico
    
    if null solucoes
        then putStrLn "Nenhuma solução encontrada!"
        else do
            let solucaoFinal = head solucoes
            mapM_ (putStrLn . unwords . map imprimirCelula) solucaoFinal
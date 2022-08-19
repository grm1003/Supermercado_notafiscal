main = do
  putStrLn "Bem vindo ao supermercado do Gabriel Rezende Machado - Matricula 121BSI217"


type Codigo = Int 
type Nome = String 
type Preço = Float 

type Produto = (Codigo,Nome, Preço)
  
tabelaprodutos:: [Produto]
tabelaprodutos = [(001, "Chocolate", 5.00),
                  (002, "Nikito", 2.50),
                  (003, "Patinho", 37.50),
                  (004, "Doritos", 4.50),
                  (005, "Toddynho", 2.00)]

isCodigo:: Produto -> Codigo -> Bool 
isCodigo (a, _ , _) b
  | a == b = True
  | otherwise = False

getCodigo:: Produto -> Codigo
getCodigo (a, _ , _) = a

getNome:: Produto -> Nome
getNome (_, b , _) = b

getPreço:: Produto ->  Preço
getPreço (_, _ , c) = c

codigodebarras:: [Produto] -> [Codigo]
codigodebarras [] = []
codigodebarras (a:as) = map getCodigo (a:as)

buscaporCodigo a =  buscaelemento a tabelaprodutos

buscaelemento:: Int -> [Produto] -> Produto
buscaelemento a [] = (0,"produto nao encontrado",0.00)
buscaelemento a (x:xs)
  | a == getCodigo x = x
  | otherwise = buscaelemento a xs

buscaPrecoporCodigo a = getPreço(buscaporCodigo a)

buscaNomeporCodigo a = getNome(buscaporCodigo a)

calculapreço a = sum(map buscaPrecoporCodigo a)




formataStr :: Codigo -> String
formataStr a = buscaNomeporCodigo a ++ (replicate n '.') ++ show(buscaPrecoporCodigo a) 
  where n = 50 - (length (buscaNomeporCodigo a) + length (show(buscaPrecoporCodigo a)))

  

formataStrProduto [] = ""
formataStrProduto (a:as) =  formataStr a ++ "\n" ++ formataStrProduto as

totalFormatado :: Preço -> String
totalFormatado a = "Total" ++ (replicate n '.') ++ show a
      where
        n = 50 - (length (show a) + length("Total"))

geraNotaFiscal :: [Codigo] -> IO()
geraNotaFiscal a = do 
                    appendFile "notaFiscal.txt" ((formataStrProduto a ) ++ (totalFormatado (calculapreço a )))



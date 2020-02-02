module Main where

import Lib
import Tipos.LinhaType
import Data.List.Split

main :: IO ()
main = do
    arq1 <- lerArquivo "../dados/arquivo1.txt"
    arq2 <- lerArquivo "../dados/arquivo2.txt"
    let difArq1ParaArq2 = filter (\x -> notElem x arq2) arq1
    let difArq2ParaArq1 = filter (\x -> notElem x arq1) arq2
    putStrLn "----------------------------"
    mapM_ print $ difArq1ParaArq2 <> difArq2ParaArq1

lerArquivo :: FilePath -> IO [Maybe Linha]
lerArquivo nomeArquivo = do
    todoConteudo <- readFile nomeArquivo
    let apenasConteudo = drop 1 $ lines todoConteudo
    return $ map (mkLinha . obterLinha) $ apenasConteudo
    
obterLinha :: String -> [String]
obterLinha linha = splitOn "|" linha

mkLinha :: [String]-> Maybe Linha
mkLinha (ie:razao:origem:vl:_) =
  case (toType origem) of
    Left _ -> Nothing
    Right o -> Just $ Linha ie razao o vl

toType :: String -> Either String Origem
toType s = case s of
  "EFD"    -> Right EFD
  "DASN"   -> Right DASN
  "PGDASD" -> Right PGDASD
  "GIVA"   -> Right GIVA
  "DEFIS"  -> Right DEFIS
  _ -> Left "Tipo de declaração inexistente"

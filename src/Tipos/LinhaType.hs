module Tipos.LinhaType where

data Origem 
    = EFD 
    | PGDASD 
    | DASN 
    | GIVA 
    | DEFIS
    deriving (Show, Eq)

data Linha = 
    Linha 
    { nrInscrEstadual :: String
    , noRazaoSocial :: String
    , origem :: Origem
    , valor :: String
    } deriving (Show)


instance Eq Linha where
    (==) l1 l2 = nrInscrEstadual l1 == nrInscrEstadual l2 && origem l1 == origem l2

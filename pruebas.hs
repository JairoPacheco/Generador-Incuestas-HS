{-# LANGUAGE TemplateHaskell #-}
import Data.Char 
import Data.List
import Data.IORef
import Control.Concurrent
import Control.Exception
import Data.Functor
import Data.Time
import Control.Monad.State
import Foreign.Marshal.Unsafe

  
main :: IO ()
main = do
    mainloop []
    
mainloop :: [[(String,([String],[Int]))]] -> IO ()
mainloop listaIncuestas = do
    print listaIncuestas
    numero <- imprimirMenu
    if numero == 1
        then do
            formulario <- hacerFormulario
            mainloop (listaIncuestas ++ [formulario])
        else if numero == 2
            then do
                incuestaNumero <- escogerIncuesta
                mainloop listaIncuestas
        else if numero == 3
            then print "Por implementar Responder"
        else if numero == 4
            then print "Por implementar Responder"
        else mainloop listaIncuestas 

preguntass :: Int -> [(String,([String],[Int]))] -> IO [(String,([String],[Int]))]
preguntass 0 a = return a
preguntass n lista = do
    getTipopregunta <- tipoPregunta
    if getTipopregunta == "1"
        then do
            getPregunta <- sPregunta
            todasRespuestas <- preguntasPorEscala 5 ([],[])
            preguntass (n-1) (lista++[(getPregunta,todasRespuestas)])
        else if getTipopregunta == "2"
            then do
                getPregunta <- sPregunta
                getCRespuestas <- cRespuestas
                todasRespuestas <- respuestass getCRespuestas ([],[])
                preguntass (n-1) (lista++[(getPregunta,todasRespuestas)])
        else do
            putStrLn "No se encontro ese tipo de pregunta" 
            preguntass n lista
    
    
hacerFormulario :: IO [(String,([String],[Int]))]
hacerFormulario = do
    cantidadPreguntas <- cPreguntas
    totalRespuesta <- preguntass cantidadPreguntas []
    return totalRespuesta

preguntasPorEscala :: Int -> ([String],[Int]) -> IO ([String],[Int])
preguntasPorEscala 0 a = return a
preguntasPorEscala n lista = do
    valor <- valorEscala n
    preguntasPorEscala (n-1) ((fst lista)++[valor],[])
    

respuestass :: Int -> ([String],[Int]) -> IO ([String],[Int])
respuestass 0 a = return a
respuestass n lista = do
    getRespuesta <- sRespuesta
    respuestass (n-1) ((fst lista)++[getRespuesta],[])


responderManual :: Int -> [[(String,([String],[Int]))]] -> IO [Int]
responderManual n lista = do
    let incuesta = lista !! n
    respuestas <- imprimirPregunta incuesta []
    return respuestas 

imprimirPregunta :: [(String,([String],[Int]))] -> [Int] -> IO [Int]
imprimirPregunta encuesta respuestas= do
    if length encuesta == 0
        then return respuestas
        else do
            return respuestas

imprimirMenu :: IO Int
imprimirMenu = do
    putStrLn "Que decea hacer \n1 crear incuesta \n2 responder incuesta manual \n3 responder incuesta automatico \n4 Generar Estadisticas"
    accion <- getLine
    let numero = (read accion :: Int)
    return numero

cPreguntas :: IO Int
cPreguntas = do
    putStrLn "Cuantas preguntas desea añadir"
    accion <- getLine
    let numero = (read accion :: Int)
    return numero

cRespuestas :: IO Int
cRespuestas = do
    putStrLn "Cuantas respuestas desea añadir a la pregunta"
    accion <- getLine
    let numero = (read accion :: Int)
    return numero

sRespuesta :: IO String
sRespuesta = do
    putStrLn "Cual es la respuesta"
    accion <- getLine
    return accion

sPregunta :: IO String
sPregunta = do
    putStrLn "Cual es la pregunta"
    accion <- getLine
    return accion

tipoPregunta :: IO String
tipoPregunta = do
    putStrLn "Digite 1 para pregunta de escala  \nDigite 2 para pregunta de seleccion unica"
    accion <- getLine
    return accion

valorEscala :: Int -> IO String
valorEscala x = do
    putStr "Que valor decea para la escala "
    putStrLn (show x)
    accion <- getLine
    return accion

escogerIncuesta :: IO Int
escogerIncuesta = do
    putStrLn "Cual incuesta desea responder"
    accion <- getLine
    let numero = (read accion :: Int)
    return numero

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
    mainloop [] [[("como estan",["muy bien","bien","mas o menos","mal","muy mal"])],[("todo bien",["si","no"]),("como estas",["bien","mal"])]]
    
mainloop :: [[(Int,(Int,Int))]] -> [[(String,[String])]] -> IO ()
mainloop listaRespuestas listaIncuestas = do
    print listaIncuestas
    print listaRespuestas
    numero <- imprimirMenu
    if numero == 1
        then do
            formulario <- hacerFormulario
            mainloop (listaRespuestas) (listaIncuestas ++ [formulario])
        else if numero == 2
            then do
                incuestaNumero <- escogerIncuesta
                let tamañoIncuesta = length (listaIncuestas !! incuestaNumero)
                nuevasRespuestas <- responderManual (tamañoIncuesta - 1) incuestaNumero listaIncuestas listaRespuestas
                mainloop (nuevasRespuestas) (listaIncuestas)
        else if numero == 3
            then do
                incuestaNumero <- escogerIncuesta
                let tamañoIncuesta = length (listaIncuestas !! incuestaNumero)
                cantidadRespuesta <- cRespuestasIncuesta
                nuevasRespuestas <- responderAutomatico cantidadRespuesta (tamañoIncuesta - 1) incuestaNumero listaIncuestas listaRespuestas
                mainloop (nuevasRespuestas) (listaIncuestas)
        else if numero == 4
            then do
                putStrLn "Las variables que se escogieron para el modulo de estadistica son las siguientes"
                variable1 listaRespuestas
        else mainloop listaRespuestas listaIncuestas 

preguntass :: Int -> [(String,[String])] -> IO [(String,[String])]
preguntass 0 a = return a
preguntass n lista = do
    getTipopregunta <- tipoPregunta
    if getTipopregunta == "1"
        then do
            getPregunta <- sPregunta
            todasRespuestas <- preguntasPorEscala 5 []
            preguntass (n-1) (lista++[(getPregunta,todasRespuestas)])
        else if getTipopregunta == "2"
            then do
                getPregunta <- sPregunta
                getCRespuestas <- cRespuestas
                todasRespuestas <- respuestass getCRespuestas []
                preguntass (n-1) (lista++[(getPregunta,todasRespuestas)])
        else do
            putStrLn "No se encontro ese tipo de pregunta" 
            preguntass n lista
    
    
hacerFormulario :: IO [(String,[String])]
hacerFormulario = do
    cantidadPreguntas <- cPreguntas
    totalRespuesta <- preguntass cantidadPreguntas []
    return totalRespuesta

preguntasPorEscala :: Int -> [String] -> IO [String]
preguntasPorEscala 0 a = return a
preguntasPorEscala n lista = do
    valor <- valorEscala n
    preguntasPorEscala (n-1) (lista++[valor])
    

respuestass :: Int -> [String] -> IO [String]
respuestass 0 a = return a
respuestass n lista = do
    getRespuesta <- sRespuesta
    respuestass (n-1) (lista++[getRespuesta])

responderManual :: Int -> Int -> [[(String,[String])]] -> [[(Int,(Int,Int))]] -> IO [[(Int,(Int,Int))]]
responderManual (-1) n listaEncuestas listaRespuestas = return listaRespuestas
responderManual t n lista listaRespuestas = do
    let pregunta = fst ((lista !! n) !! t)
    printPregunta (pregunta)
    let respuestas = snd ((lista !! n) !! t)
    printRespuestas ((length respuestas)-1) (respuestas)
    respuesta <- getLine
    let numero = (read respuesta :: Int)
    responderManual (t-1) (n) (lista) (listaRespuestas++[[(n,(t,numero))]])

auxResponderAutomatico :: Int -> Int -> [[(String,[String])]] -> [[(Int,(Int,Int))]] -> IO [[(Int,(Int,Int))]]
auxResponderAutomatico (-1) n listaEncuestas listaRespuestas = return listaRespuestas
auxResponderAutomatico t n lista listaRespuestas = do
    let respuestas = snd ((lista !! n) !! t)
    let respuesta = (length respuestas)-1
    responderManual (t-1) (n) (lista) (listaRespuestas++[[(n,(t,respuesta))]])

responderAutomatico :: Int -> Int -> Int -> [[(String,[String])]] -> [[(Int,(Int,Int))]] -> IO [[(Int,(Int,Int))]]
responderAutomatico 0 b c d e = return e
responderAutomatico a b c d e = do
    auxMandar <- auxResponderAutomatico b c d e
    responderAutomatico (a-1) b c d auxMandar

variable1 :: [[(Int,(Int,Int))]] -> IO ()
variable1 x = do
    let cantidadContestada = length x
    putStr "El nuemero total de preguntas respodidas es de "
    putStrLn (show ((length x)))
    
variable2 :: [[(Int,(Int,Int))]] -> IO ()
variable2 x = do
    putStrLn "por implementar"

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

cRespuestasIncuesta :: IO Int
cRespuestasIncuesta = do
    putStrLn "Cuantas respuestas desea para la incuesta"
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

printPregunta :: String -> IO String
printPregunta x = do
    putStrLn x
    return ""

printRespuestas :: Int -> [String] -> IO String
printRespuestas (-1) lista = return ""
printRespuestas n lista = do
    let variable = lista !! n
    putStr (variable ++ " ")
    printRespuestas (n-1) lista
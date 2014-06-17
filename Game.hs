-----------------------
-- TIPOS DE DATOS -----
-----------------------
-- El tipo de dato personaje puede ser tanto un lobo, una oveja, una lechuga o el pastor
data Personaje = Pastor | Lobo | Oveja | Lechuga deriving (Eq, Enum)

--Un Escenario es un conjunto de Personajes
type Escenario = [Personaje] 

--Los personajes pueden situarse en origen o en destino
data Posicion = Origen | Destino deriving (Show, Eq)

-- Instance show para hacer un poco más bonito la salida por pantalla
instance Show Personaje where
    show Pastor  = "\tPastor solo\n"
    show Lobo    = "\tLobo y Pastor\n"
    show Oveja   = "\tOveja y Pastor\n"
    show Lechuga = "\tLechuga y Pastor\n"
	

-----------------------
----- MÉTODOS ---------
-----------------------
--Dado un escenario decide si se ha llegado a la solución si todos los personajes están en destino
esSolucion :: Escenario -> Bool
esSolucion p = all (== Destino) $ map (posicionDe p) [Pastor .. Lechuga]

encuentraSoluciones :: Int -> [Escenario]
encuentraSoluciones n = [p | p <- hazNMovimientos, esSolucion p]
    where -- Le pasamos [[]] porque es un plan, una lista de movimientos vacios 
        hazNMovimientos = iterate (>>= hazMovimiento) [[]] !! n

--Dado un escenario, devuelve una lista con los posibles escenarios  que le pueden suceder siempre y cuando los movimientos cumplan que:
--	El movimiento sea legal.
--	El movimiento no sea un movimiento inmediatamente repetido.
--	Al realizar el movimiento los personajes no se coman entre ellos. 
hazMovimiento :: Escenario -> [Escenario]
hazMovimiento p = [ (p ++ [mp]) | mp <- [Pastor .. Lechuga], movimientoLegal p mp, not $ movimientoRepetido p mp, noSeComen (p ++ [mp])]

-- Decide si un movimiento es legal o no, un movimiento siempre será legal si el pastor se mueve o si la posición del personaje a mover es igual a la del pastor
movimientoLegal :: Escenario -> Personaje -> Bool
movimientoLegal p Pastor  = True
movimientoLegal p x       = posicionDe p x == posicionDe p Pastor

-- Devuelve la posición de un personaje dado un escenario
-- Funcion auxiliar para determinar si un movimiento es legal o no
posicionDe :: Escenario -> Personaje -> Posicion
posicionDe p x = case x of
    Pastor  -> posicionDadoN . length $ p
    x       -> posicionDadoN . length $ filter (== x) p
    where	--Si es par estás en el origen, si no en destino
        posicionDadoN n | even n    = Origen
                        | otherwise = Destino

--Evita movimientos repetidos y bucles inncesarios
movimientoRepetido :: Escenario -> Personaje -> Bool
movimientoRepetido [] _ = False
movimientoRepetido p m  = last p == m

--Dado un escenario la función analiza si el lobo se come a la oveja, o si la oveja se come la lechuga. Siempre que la oveja este con el pastor no se comeran entre ellos
noSeComen :: Escenario -> Bool
noSeComen p = ovejaPos == pastorPos || ovejaSegura && lechugaSegura
    where
        ovejaPos      = posicionDe p Oveja
        pastorPos     = posicionDe p Pastor
        ovejaSegura   = ovejaPos /= posicionDe p Lobo
        lechugaSegura = posicionDe p Lechuga /= ovejaPos

-----------------
----- MAIN ------
-----------------
main :: IO ()
main = do
    print $ encuentraSoluciones 7
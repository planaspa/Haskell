import Control.Applicative ((<$>))

-----------------------
-- TIPOS DE DATOS -----
-----------------------
data Personaje = Pastor | Lobo | Oveja | Lechuga deriving (Show, Eq, Enum)

newtype Movimiento = MovimientoDe Personaje deriving (Eq)

type Plan = [Movimiento]

data Position = Origen | Destino deriving (Show, Eq)

instance Show Movimiento where
    show (MovimientoDe Pastor)  = "Pastor\n"
    show (MovimientoDe Lobo)    = "Lobo y Pastor\n"
    show (MovimientoDe Oveja)   = "Oveja y Pastor\n"
    show (MovimientoDe Lechuga) = "Lechuga y Pastor\n"

-----------------------
----- MÉTODOS ---------
-----------------------
encuentraSoluciones :: Int -> [Plan]
encuentraSoluciones n = [p | p <- hazNMovimientos, esSolucion p]
    where -- Le pasamos [[]] porque es un plan, una lista de movimientos vacios 
        hazNMovimientos = iterate (>>= hazMovimiento) [[]] !! n

--Dado un escenario o plan, devuelve una lista con los posibles escenarios o planes que le pueden suceder siempre y cuando los movimientos cumplan que:
--	El movimiento sea legal.
--	El movimiento no sea un movimiento inmediatamente repetido.
--	Al realizar el movimiento los personajes no se coman entre ellos. 
hazMovimiento :: Plan -> [Plan]
hazMovimiento p = [ (p ++ [mp]) | mp <- MovimientoDe <$> [Pastor .. Lechuga], movimientoLegal p mp, not $ movimientoRepetido p mp, noSeComen (p ++ [mp])]


-- Devuelve la posición de un personaje dado un escenario
posicionDe :: Plan -> Personaje -> Position
posicionDe p c = case c of
    Pastor  -> posicionDadoN . length $ p
    c       -> posicionDadoN . length $ filter (== MovimientoDe c) p
    where
        posicionDadoN n | even n    = Origen
                        | otherwise = Destino

-- Decide si un movimiento es legal o no, un movimiento siempre será legal si el pastor se mueve o si la posición del personaje a mover es igual a la del pastor
movimientoLegal :: Plan -> Movimiento -> Bool
movimientoLegal p (MovimientoDe Pastor)  = True
movimientoLegal p (MovimientoDe c)       = posicionDe p c == posicionDe p Pastor

--Evita movimientos repetidos y bucles inncesarios
movimientoRepetido :: Plan -> Movimiento -> Bool
movimientoRepetido [] _ = False
movimientoRepetido p m  = last p == m

--Dado un escenario la función analiza si el lobo se come a la oveja, o si la oveja se come la lechuga. Siempre que la oveja este con el pastor no se comeran entre ellos
noSeComen :: Plan -> Bool
noSeComen p = ovejaPos == pastorPos || ovejaSegura && lechugaSegura
    where
        ovejaPos      = posicionDe p Oveja
        pastorPos     = posicionDe p Pastor
        ovejaSegura   = ovejaPos /= posicionDe p Lobo
        lechugaSegura = posicionDe p Lechuga /= ovejaPos

--Dado un escenario decide si se ha llegado a la solución si todos los personajes están en destino
esSolucion :: Plan -> Bool
esSolucion p = all (== Destino) $ map (posicionDe p) [Pastor .. Lechuga]

-----------------
----- MAIN ------
-----------------
main :: IO ()
main = do
    print $ encuentraSoluciones 7
    --print $ encuentraSoluciones 13
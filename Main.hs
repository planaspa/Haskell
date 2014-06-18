module Main where
import Game

-----------------
----- MAIN ------
-----------------
main :: IO ()
main = do

	putStrLn "Starting test..."
	putStrLn ""

	putStrLn "--------------------------------------"
	putStrLn "--- EL LOBO, LA OVEJA Y LA LECHUGA ---"
	putStrLn "--------------------------------------"
	putStrLn ""

	putStrLn "Busca todas las soluciones con 0 movimientos. Opcion imposible:"
	print $ encuentraSoluciones 0
	putStrLn ""

	putStrLn "Busca todas las soluciones con 3 movimientos. Opcion sin resultados:"
	print $ encuentraSoluciones 3
	putStrLn ""
	
	putStrLn "Busca todas las soluciones con 6 movimientos. Opcion imposible, siempre tiene que haber un nuemero impar de movimientos:"
	print $ encuentraSoluciones 6
	putStrLn ""

	putStrLn "Busca todas las soluciones con 7 movimientos. Opcion con resultados más corta:"
	print $ encuentraSoluciones 7
	putStrLn ""
	
	putStrLn "Busca todas las soluciones con 9 movimientos. Opcion sin resultados:"
	print $ encuentraSoluciones 9
	putStrLn ""

	putStrLn "Busca todas las soluciones con 11 movimientos. Opcion sin resultados:"
	print $ encuentraSoluciones 11
	putStrLn ""

	putStrLn "Busca todas las soluciones con 13 movimientos. Siguiente Opcion con resultados:"
	print $ encuentraSoluciones 13
	putStrLn ""
-- LIBRERIAS
import Data.List

-- PIRATAS

jackSparrow = ("Jack Sparrow", [("Brujula", 10000), ("Frasco de arena", 0)])
davidJones = ("David Jones", [("Cajita musical", 1)])
anneBonny = ("Anne Bonny", [("Doblones", 100), ("Frasco de arena", 1)])
elizabethSwann = ("Elizabeth Swann", [("Moneda del cofre muerto", 100), ("Espada de hierro", 50)])
willTurner = ("Will Turner", [("Cuchillo", 5)])

-- BARCOS

barcoPerlaNegra = (formaValioso, [jackSparrow, anneBonny])
barcoHolandesErrante = (formaPorNombre "Oro", [davidJones])

-- ISLAS

islaTortuga = [("Frasco de arena", 1)]
islaRon = [("Botella de ron", 25)]

-- CIUDADES

ciudadGrande = [("Oro", 100), ("Amuleto", 500), ("Frasco de arena", 1), ("Oro", 250), ("Botella de vodka", 230), ("Botella de whisky", 550), ("Cuchillo", 5), ("Bala", 10)]
ciudadChica = [("Chupete", 500)]

-- FUNCIONES PARTE 1

cantidadTesoros (nombre, tesoros) = length tesoros
esAfortunado (nombre, tesoros) = sum(map snd (tesoros)) >= 10000
mismoTesoro (nombre1, tesoros1) (nombre2, tesoros2) nombreTesoro = (elem nombreTesoro (map fst tesoros1)) && (elem nombreTesoro (map fst tesoros2)) && (null (intersect (map snd tesoros1) (map snd tesoros2)))
valorTesoroMasValioso (nombre, tesoros) = maximum(map snd (tesoros))
adquirirTesoro (nombre, tesoros) tesoro = (nombre, tesoros ++ [tesoro])
perderTesorosMayoresACien (nombre, tesoros) = (nombre, filter (esMenorACien) tesoros)
esMenorACien tesoro = snd tesoro<100
perderTesorosConNombre (nombre, tesoros) nombreTesoro = (nombre, filter (tieneOtroNombre nombreTesoro) tesoros)
tieneOtroNombre nombre tesoro = nombre /= fst tesoro

-- FUNCIONES PARTE 2

saquear formaSaqueo tesoro pirata
 | formaSaqueo tesoro = adquirirTesoro pirata tesoro
 | otherwise = pirata
 
formaValioso tesoro = (snd tesoro >= 100)
formaPorNombre tesoroBuscado tesoro = fst tesoro == tesoroBuscado
formaCombinacion tesoroBuscado tesoro = (formaValioso tesoro) || (formaPorNombre tesoroBuscado tesoro)
formaDeCorazon tesoro = False

-- FUNCIONES PARTE 3

incorporoTripulacion barco pirata = (snd barco) ++ [pirata]
abandonoTripulacion barco pirata = filter (/= pirata) (snd barco)

anclarIsla isla (formaSaqueo, tripulacion) = zip (map fst tripulacion) (map (++isla) (map snd tripulacion))
anclarCiudad ciudad (formaSaqueo, tripulacion) = zipWith (saquear formaSaqueo) ciudad tripulacion
 
abordarBarco (formaSaqueo1, tripulacion1) (formaSaqueo2, tripulacion2)
 | (length tripulacion1) > (length tripulacion2) = anclarCiudad (concat(map snd tripulacion2)) (formaSaqueo1,tripulacion1)
 | (length tripulacion1) < (length tripulacion2) = anclarCiudad (concat(map snd tripulacion1)) (formaSaqueo2,tripulacion2)
 | otherwise = tripulacion1 ++ tripulacion2
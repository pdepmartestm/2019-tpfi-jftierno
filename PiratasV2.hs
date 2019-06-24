import Text.Show.Functions

-- CONSTRUCTORES

data Tesoro = Tesoro {
    nombreT :: String,
    valorT :: Float
} | Bono {
    cotizaciones :: [Float]
} | LeLiq {
    importe :: Float,
    pais :: String
} deriving (Show, Eq)

data Pirata = Pirata {
    nombreP :: String,
    botin :: [Tesoro]
} deriving (Show, Eq)

data Barco = Barco {
    nombreB :: String,
    caracteristica :: Tesoro -> Bool,
    tripulacion :: [Pirata]
} deriving (Show)

-- TIPADO DE FUNCIONES

obtenerNombre :: Pirata -> String
obtenerBotin :: Pirata -> [Tesoro]
perteneceAlBotin :: [Tesoro] -> Tesoro -> Bool
mismoNombreTesoro :: Tesoro -> Tesoro -> Bool
distintoValorTesoro :: Tesoro -> Tesoro -> Bool
mismoNombreYDistintoValor :: Tesoro -> Tesoro -> Bool
esValioso :: Tesoro -> Bool
distintoNombre :: String -> Tesoro -> Bool
cantidadTesoros :: Pirata -> Int
esAfortunado :: Pirata -> Bool
mismoTesoroConDistintoValor :: Pirata -> Pirata -> Bool
valorTesoroMasValioso :: Pirata -> Float
adquirirTesoro :: Tesoro -> Pirata -> Pirata
perderTesorosValiosos :: Pirata -> Pirata
perderTesorosConNombre :: Pirata -> String -> Pirata
saquear :: (Tesoro -> Bool) -> Pirata -> Tesoro -> Pirata
formaBuitres :: Tesoro -> Bool
formaCompleja :: [(Tesoro -> Bool)] -> Tesoro -> Bool
formaCorazon :: Tesoro -> Bool
formaEspecifico :: String -> Tesoro -> Bool
formaFobicos :: String -> Tesoro -> Bool
formaValioso :: Tesoro -> Bool
puedeSaquear :: Tesoro -> (Tesoro -> Bool) -> Bool
incorporarTripulacion :: Pirata -> Barco -> Barco
abandonarTripulacion :: Pirata -> Barco -> Barco
anclarIsla :: Tesoro -> Barco -> Barco
atacarCiudad :: [Tesoro] -> Barco -> Barco
abordarBarco :: Barco -> Barco -> Barco
tasaDeInteres :: String -> Float
irAUniversidad :: ((Tesoro -> Bool) -> (Tesoro -> Bool)) -> Barco -> Barco
uade :: (Tesoro -> Bool) -> (Tesoro -> Bool)
uba :: (Tesoro -> Bool) -> (Tesoro -> Bool)
uai :: (Tesoro -> Bool) -> (Tesoro -> Bool)
generarHistoria :: Foldable t => t (a -> a) -> a -> a
historiaInofensiva :: (Eq a, Foldable t) => t (a -> a) -> [a] -> [a]
esInofensiva :: (Eq a, Foldable t) => t (a -> a) -> a -> Bool
mismosTripulantes :: [Pirata] -> [Pirata] -> Bool
perteneceALaTripulacion :: [Pirata] -> Pirata -> Bool
historiaTripulacion :: (Ord a, Foldable t) => t (a -> a) -> [a] -> a
generarPirata :: Float -> Pirata

-- DEFINICIONES

-- Tesoros
brujula = Tesoro "Brujula" 10000
frascodearena0 = Tesoro "Frasco de arena" 0
frascodearena1 = Tesoro "Frasco de arena" 1
cajitamusical = Tesoro "Cajita musical" 1
doblones = Tesoro "Doblones" 100
oro = Tesoro "Oro" 100
monedacofremuerto = Tesoro "Moneda del cofre muerto" 100
espadadehierro = Tesoro "Espada de hierro" 50
cuchillo = Tesoro "Cuchillo" 5
botelladeron = Tesoro "Botella de ron" 25
-- Piratas
jackSparrow = Pirata "Jack Sparrow" [brujula, frascodearena0]
davidJones = Pirata "David Jones" [cajitamusical]
anneBonny = Pirata "Anne Bonny" [frascodearena1, doblones]
elizabethSwann = Pirata "Elizabeth Swann" [monedacofremuerto, espadadehierro]
willTurner = Pirata "Will Turner" [cuchillo]
-- Barcos
perlaNegra = Barco "Perla Negra" formaValioso [jackSparrow, anneBonny]
holandesErrante = Barco "Holandes Errante" formaCorazon [davidJones]
-- Islas
islaTortuga = frascodearena1
islaRon = botelladeron
-- Ciudades
ciudadRica = [doblones, oro, monedacofremuerto, frascodearena1, botelladeron]
ciudadPobre = [frascodearena0]
-- Tasas de interes
tasas = [("Argentina", 0.74), ("Chile", 0.25)]
-- Instances
instance Eq Barco where
    (Barco nombre1 caracteristica1 tripulacion1) == (Barco nombre2 caracteristica2 tripulacion2) = mismosTripulantes tripulacion1 tripulacion2
instance Ord Barco where
    (Barco nombre1 caracteristica1 tripulacion1) <= (Barco nombre2 caracteristica2 tripulacion2) = length tripulacion1 <= length tripulacion2
-- FUNCIONES DE USO GENERAL
valor (Tesoro _ valor) = valor
valor (Bono cotizaciones) = (maximum(cotizaciones) - minimum(cotizaciones))*1.5
valor (LeLiq importe pais) = importe * (1 + tasaDeInteres pais)
nombre (Tesoro nombre _) = nombre
nombre (Bono _) = "Bono"
nombre (LeLiq _ pais) = "LeLiq " ++ pais
obtenerNombre (Pirata nombre _) = nombre
obtenerBotin (Pirata _ botin) = botin
perteneceAlBotin botin tesoro = any (mismoNombreYDistintoValor tesoro) botin
mismoNombreTesoro tesoro1 tesoro2 = nombre tesoro1 == nombre tesoro2
distintoValorTesoro tesoro1 tesoro2 = valor tesoro1 /= valor tesoro2
mismoNombreYDistintoValor tesoro1 tesoro2 = (mismoNombreTesoro tesoro1 tesoro2) && (distintoValorTesoro tesoro1 tesoro2)
esValioso (Tesoro _ valor) = valor>=100
distintoNombre nombreBuscado tesoro = nombreBuscado /= nombre tesoro
-- FUNCIONES TESOROS PIRATAS
cantidadTesoros (Pirata _ botin) = length botin
esAfortunado (Pirata _ botin) = sum(map valor botin) >= 10000
mismoTesoroConDistintoValor (Pirata _ botin1) (Pirata _ botin2) = any (perteneceAlBotin botin1) botin2
valorTesoroMasValioso (Pirata _ botin) = maximum(map valor botin)
adquirirTesoro tesoro (Pirata nombre botin) = Pirata nombre (botin ++ [tesoro])
perderTesorosValiosos (Pirata nombre botin) = Pirata nombre (filter (not.esValioso) botin)
perderTesorosConNombre (Pirata nombre1 botin) nombre2 = Pirata nombre1 (filter (distintoNombre nombre2) botin)
-- FUNCIONES TEMPORADA DE SAQUEOS
saquear formaSaqueo pirata tesoro
 | formaSaqueo tesoro = adquirirTesoro tesoro pirata
 | otherwise = pirata

formaValioso tesoro = esValioso tesoro
formaEspecifico tesoroBuscado tesoro = tesoroBuscado == (nombre tesoro)
formaCorazon tesoro = False
formaCompleja listaFormasSaqueo tesoro = any (puedeSaquear tesoro) listaFormasSaqueo
puedeSaquear tesoro formaSaqueo = formaSaqueo tesoro
-- FUNCIONES NAVEGANDO LOS SIETE MARES
incorporarTripulacion pirata (Barco nombre caracteristica tripulacion) = Barco nombre caracteristica (tripulacion ++ [pirata])
abandonarTripulacion pirata (Barco nombre caracteristica tripulacion) = Barco nombre caracteristica (filter (/=pirata) tripulacion)
anclarIsla isla (Barco nombre caracteristica tripulacion) = Barco nombre caracteristica (map (adquirirTesoro isla) tripulacion)
atacarCiudad ciudad (Barco nombre caracteristica tripulacion) = Barco nombre caracteristica (zipWith (saquear caracteristica) tripulacion ciudad)
abordarBarco (Barco nombre1 caracteristica1 tripulacion1) (Barco nombre2 caracteristica2 tripulacion2)
 | length tripulacion1 > length tripulacion2 = atacarCiudad (concat(map obtenerBotin tripulacion2)) (Barco nombre1 caracteristica1 tripulacion1)
 | length tripulacion2 > length tripulacion1 = atacarCiudad (concat(map obtenerBotin tripulacion1)) (Barco nombre2 caracteristica2 tripulacion2)
 | otherwise = Barco "Amigos" formaCorazon (tripulacion1 ++ tripulacion2)
-- FUNCIONES TESOROS PARA TODOS
tasaDeInteres pais = (snd.head.filter ((pais==).fst)) tasas
-- FUNCIONES SAQUEOS SOFISTICADOS
formaBuitres (Bono _) = True
formaFobicos nombreFobia tesoro = nombreFobia /= nombre tesoro
-- FUNCIONES UNIVERSIDAD PIRATA
irAUniversidad universidad (Barco nombre caracteristica tripulacion) = Barco nombre (universidad caracteristica) tripulacion
uade caracteristica = not.caracteristica
uba caracteristica = formaCompleja ([caracteristica] ++ [formaBuitres, formaValioso])
uai caracteristica = caracteristica
-- FUNCIONES HISTORIAS DE BARCOS
generarHistoria situaciones barco = foldr ($) barco situaciones
historiaInofensiva historia barcos = filter (esInofensiva historia) barcos
esInofensiva historia barco = barco == (generarHistoria historia barco)
mismosTripulantes tripulacion1 tripulacion2 = all (perteneceALaTripulacion tripulacion1) tripulacion2
perteneceALaTripulacion tripulacion (Pirata nombre _) = any ((nombre==).obtenerNombre) tripulacion 
historiaTripulacion historia barcos = maximum (map (generarHistoria historia) barcos)
-- FUNCIONES PROLIFERACION DE PIRATAS
barcoInfinito = Barco "Infinita tripulacion" (formaEspecifico "Oro") (map generarPirata [1..])
generarPirata valor = Pirata "Pirata X" [Tesoro "Garrote" valor]

{-

FUNCIONES QUE NO DEVUELVEN NADA POR QUEDARSE EVALUANDO INFINITAMENTE
- abordarBarco
- historiaInofensiva
- esInofensiva
- historiaTripulacion

FUNCIONES QUE DEVUELVEN UNA RESPUESTA INFINITA
- incorporarTripulacion
- abandonarTripulacion
- anclarIsla
- irAUniversidad

FUNCIONES QUE DEVUELVEN UNA REPUESTA FINITA
- atacarCiudad

FUNCIONES QUE SEGUN LOS PARAMETROS PASA UNA COSA U OTRA
- generarHistoria

-}
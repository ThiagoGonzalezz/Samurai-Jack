module Library where
import PdePreludat

data Elemento = UnElemento { 
tipo :: String,
ataque :: (Personaje -> Personaje),
defensa :: (Personaje -> Personaje) 
}

data Personaje = UnPersonaje { 
nombre :: String,
salud ::    Number,
elementos :: [Elemento],
anioPresente :: Number 
}

--1

--a
mandarAlAnio::Number->Personaje->Personaje
mandarAlAnio anio pj = pj{anioPresente = anio}

--b
meditar::Number->Personaje->Personaje
meditar num = modificarSalud (num/2)

--c
causarDanio::Number->Personaje->Personaje
causarDanio num = modificarSalud (-num) 

modificarSalud::Number->Personaje->Personaje
modificarSalud num pj = pj{salud = max 0 (salud pj + num)}

--2

--a
esMalvado::Personaje->Bool
esMalvado = elem "Maldad" . map tipo . elementos

--b
danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce pj = diferenciaSalud pj . flip ($) pj . ataque


diferenciaSalud::Personaje->Personaje->Number
diferenciaSalud pj1 pj2 = salud pj1 - (salud pj2)

--c
type Enemigos = [Personaje]

enemigosMortales::Personaje->Enemigos->Enemigos
enemigosMortales pj = filter (sePuedeMatarCon (elementos pj))

sePuedeMatarCon :: [Elemento] -> Personaje -> Bool
sePuedeMatarCon elementos pj = any (loMata pj) elementos

loMata :: Personaje -> Elemento -> Bool
loMata pj elemento = (estaMuerto . ataque elemento) pj

estaMuerto::Personaje->Bool
estaMuerto = (== 0) . salud

--3

--a
type Intensidad = Number

concentracion::Intensidad->Elemento
concentracion intensidad = UnElemento{
    tipo = "Magia",
    ataque = id,
    defensa = (repetir intensidad (meditar intensidad))
}

repetir::Number->(Personaje->Personaje)->Personaje->Personaje
repetir 0 _ pj =  pj
repetir contador funcion pj = repetir (contador -1) funcion (funcion pj) 

--b
esbirro::Elemento
esbirro = UnElemento {
    tipo = "Maldad",
    ataque = (causarDanio 1),
    defensa = id 
}

esbirrosMalvados::Number->[Elemento]
esbirrosMalvados num = replicate num esbirro

--c
jack::Personaje
jack = UnPersonaje{
nombre = "Jack", 
salud = 300,
elementos = [concentracion 3, katanaMagica],
anioPresente = 200
}

katanaMagica::Elemento 
katanaMagica = UnElemento{
tipo = "Magia",
ataque = (causarDanio 1000),
defensa = id
}

--d
aku::Number->Number->Personaje
aku anio salud = UnPersonaje{
    nombre = "Aku",
    salud = salud,
    elementos = generarElementosAku anio, 
    anioPresente = anio
}

generarElementosAku::Number->[Elemento]
generarElementosAku anio = concentracion 4 : generarPortal anio : esbirrosMalvados (100 * anio)

generarPortal::Number -> Elemento
generarPortal anio = UnElemento {
    tipo = "Magia",
    ataque = mandarAlAnio (futuro anio),
    defensa = aku (futuro anio) . salud
}

futuro::Number->Number
futuro = (+) 280

--4

luchar::Personaje->Personaje->(Personaje, Personaje)
luchar pj1 pj2 
    |murioPostAtaqueDe pj1 pj2 = luchar (atacarConTodo pj1 pj2) (curarseConTodo pj1) 
    |otherwise = ((atacarConTodo pj1 pj2), (curarseConTodo pj1))

murioPostAtaqueDe::Personaje->Personaje->Bool
murioPostAtaqueDe pj1 = estaMuerto . atacarConTodo pj1

atacarConTodo::Personaje->Personaje->Personaje
atacarConTodo atacante rival = foldr (ataque) rival (elementos atacante)

curarseConTodo::Personaje->Personaje
curarseConTodo pj = foldr (defensa) pj (elementos pj)


--5 Inferir el tipo

f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

{-
 
f :: Eq a => (a -> -> c -> (b,b)) -> (Number->a) -> a -> ([d] -> [b])                       

x :: Eq a => (a -> c -> (b,b))

y :: Eq a => (Number->a)

z :: Eq a => a 

-}
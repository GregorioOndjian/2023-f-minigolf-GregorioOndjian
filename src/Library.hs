module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--Punto 1 PALOS
--a)
habilidadJugador :: Jugador -> Habilidad
habilidadJugador jugador = habilidad jugador

type Palo = (Jugador -> Tiro)

putter :: Palo
putter jugador = UnTiro {velocidad = 10, precision = precisionJugador (habilidadJugador jugador) *2, altura = 0}

madera :: Palo 
madera jugador = UnTiro {velocidad = 100, precision = precisionJugador (habilidadJugador jugador) /2, altura = 5}


hierros :: Number -> Palo
hierros n jugador  | 0 > n && n >= 10 = UnTiro {velocidad = fuerzaJugador (habilidadJugador jugador) * n, precision = precisionJugador (habilidadJugador jugador) /n, altura = n-3}

--b) 

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo jugador

-- Punto 3

--type Obstaculo = (Tiro -> Tiro)

data Obstaculo = UnObstaculo {
condicion :: Condicion,
efecto :: Efecto
}deriving (Eq,Show)

rampita :: Obstaculo 
rampita = UnObstaculo tunelRampitaSuperada efectoTunelRampita
laguna :: Number -> Obstaculo 
laguna num = UnObstaculo lagunaSuperada (efectoLaguna num)
hoyo :: Obstaculo
hoyo =  UnObstaculo hoyoSuperada efectoHoyo


type Condicion = (Tiro -> Bool)
type Efecto = (Tiro -> Tiro)

tiroEnCero :: Efecto
tiroEnCero tiro = UnTiro {velocidad = 0, precision = 0, altura = 0}

tunelRampitaSuperada :: Condicion
tunelRampitaSuperada tiro = precision tiro > 90 && altura tiro == 0

efectoTunelRampita :: Tiro -> Tiro
efectoTunelRampita tiro = UnTiro {velocidad = velocidad tiro *2, precision = 100, altura = 0}


lagunaSuperada :: Condicion
lagunaSuperada tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna :: Number -> Efecto
efectoLaguna largoLaguna tiro = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = precision tiro/largoLaguna}
                        
hoyoSuperada :: Condicion
hoyoSuperada tiro = altura tiro == 0 && between 5 20 (velocidad tiro) && precision tiro > 95

efectoHoyo :: Efecto
efectoHoyo tiro = tiroEnCero tiro 

obstaculoSuperado :: Tiro -> Obstaculo  -> Tiro
obstaculoSuperado tiro obstaculo | (condicion obstaculo) tiro = (efecto obstaculo) tiro
                                 | otherwise = tiroEnCero tiro


-- Punto 4

tiro = UnTiro {velocidad =10, precision = 95, altura = 0}

--a)

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo jugador obstaculo) palos

superaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
superaObstaculo jugador obstaculo palo = condicion obstaculo (golpe jugador palo)

supera :: [Obstaculo] -> Tiro -> [Obstaculo]
--supera [] _ = []
--supera (x:xs) tiro | (condicion x) tiro = x: supera xs tiro
--                   | otherwise = [x]
supera [] _ = []
supera (x:xs) tiro | (condicion x) tiro = x : supera xs ((efecto x) tiro)
                   | otherwise = xs

--superarObstaculosConsecutivos :: [Obstaculo] -> Tiro -> Number
--superarObstaculosConsecutivos listaObstaculos tiro = 


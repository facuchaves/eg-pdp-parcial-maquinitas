module ParcialMaquinitas where 

{- 1. Conocer la suerte total de una persona. Si no tiene un amuleto, es su suerte normal, si tiene uno, 
su suerte se multiplica por el valor de ese amuleto.

En general, sólo se considera que una persona tiene un factor si el valor del mismo es mayor a cero.
Tener un amuleto de valor 0 es lo mismo que no tenerlo en absoluto.
-}

type Nombre = String
type Dinero = Float
type Suerte = Int
type Pacienca = Bool

type Factor = Int 
type Inteligencia = Factor
type Amuleto = Factor

data Persona = CPersona {
  nombre :: Nombre,
  dinero :: Dinero,
  suerte :: Suerte,
  factores :: [Factor],
  amuleto :: Amuleto,
  pacienca :: Pacienca,
  apuesta :: Dinero
} deriving (Eq , Ord)

instance  Show Persona where
  show ( CPersona nombre dinero suerte factores amuleto pacienca apuesta ) =  " Nombre :  " ++ show nombre ++ " Dinero : " ++ show dinero ++ " Suerte : " ++ show suerte ++ " Apuesta : " ++ show apuesta

calcularSuerte :: Persona -> Persona
calcularSuerte persona 
	| tieneAmuleto persona = persona { suerte = (suerte persona) * (amuleto persona) } -- Ver si esto puede mejorar
	| otherwise = persona

tieneAmuleto :: Persona -> Bool
tieneAmuleto = mayorA0 . amuleto

mayorA0 :: Int -> Bool
mayorA0 = (>0)

{-2. Desarrollar los juegos ruleta y maquinita sabiendo que un juego se compone por un nombre, 
un premio que determina cuánto dinero se ganaría a partir de un monto apostado 
y una serie de criterios determinantes para ganar. Para modelar el dinero usar el tipo Float.

a- La ruleta que se gana 37 veces lo apostado. Para ganar la persona debe tener una suerte total mayor a 80.
b- La maquinita que se basa en un jackpot y lo que se gana es la apuesta más el jackpot. 
Para ganar se deben cumplir dos condiciones: que la persona tenga una suerte total mayor a 95 y
 además que tenga paciencia.
 -}

type Recompensa = Dinero
type Apuesta = Dinero
type Criterio = Persona -> Bool
type Premio = Apuesta -> Recompensa

data Juego = CJuego {
  nombreJuego :: Nombre,
  premio :: Premio,
  criterios :: [Criterio]
}

ruleta :: Juego
ruleta = CJuego { nombreJuego = "Ruleta" , premio = premioRuleta , criterios = [suerteMayorA80] }

type MontoJackpot = Dinero

jackpot :: MontoJackpot -> Juego
jackpot montoJackpot = CJuego { nombreJuego = "Jackpot" , premio = premioJackpot montoJackpot , criterios = [ suerteMayorA95 , pacienca ] }

premioRuleta :: Premio
premioRuleta = (37*) 

suerteMayorA80 :: Criterio
suerteMayorA80 = mayorA80 . suerte

mayorA80 :: Int -> Bool
mayorA80 = (>80)

premioJackpot :: MontoJackpot -> Premio
premioJackpot montoJackpot apuesta = montoJackpot --(+montoJackpot) Ver Bien Esto !

suerteMayorA95 :: Criterio
suerteMayorA95 = mayorA95 . suerte

mayorA95 :: Int -> Bool
mayorA95 = (>95)

--3. Saber si un jugador puede ganar un juego, lo cual sucede si cumple todas las condiciones para ganar ese juego.

puedeGanarJuego :: Persona -> Juego -> Bool
puedeGanarJuego persona juego = pasaTodosLosCriterios persona ( criterios juego )

pasaTodosLosCriterios :: Persona -> [Criterio] -> Bool
pasaTodosLosCriterios persona = and . map ( aplicarCriterio persona ) 

aplicarCriterio :: Persona -> Criterio -> Bool
aplicarCriterio persona criterio = criterio persona

{-4. Dada una apuesta inicia, obtener la cantidad total de dinero que puede conseguir un jugador con ese monto 
si va a un casino. Cuando alguien va a un casino apuesta en cada juego lo conseguido en el juego anterior,
 evitando los juegos en los cuales no pueda ganar.
Si no puede ganar en ningún juego, el resultado sería la apuesta inicial, ya que no apostaría en ninguno.
-}

type Casino = [Juego]

apostar :: Persona -> Juego -> Persona
apostar persona juego 
      | puedeGanarJuego persona juego = aumentarDineroPersona persona juego 
      | otherwise = persona
      where
        aumentarDineroPersona :: Persona -> Juego -> Persona
        aumentarDineroPersona persona juego = persona { dinero =  ( dinero persona ) + ( obtenerRecompensa persona juego ) }
        obtenerRecompensa :: Persona -> Juego -> Dinero
        obtenerRecompensa persona juego = ( premio juego ) ( apuesta persona )

apostarEnElCasino :: Persona -> Casino -> Persona
apostarEnElCasino = foldl apostar

{-5. Algunos jugadores van a los casinos en grupos. Queremos saber, dado un casino,
 los nombres de los jugadores de un grupo que no pueden ganar a ningún juego.
-}

type Grupo = [Persona]

puedeGanarAlMenosUnJuego :: Casino -> Persona -> Bool
puedeGanarAlMenosUnJuego casino persona = or ( map (puedeGanarJuego persona) casino ) -- TODO Ver como componer y usar el flip

perdedoresDelGrupo :: Casino -> Grupo -> Grupo
perdedoresDelGrupo casino = filter ( not . puedeGanarAlMenosUnJuego casino ) 

--TODO Pasar a nombre un grupo

{-6. Hacer que un jugador apueste una cantidad en un juego, que implica que la persona baje su saldo esa cantidad 
y luego juegue al juego. Si puede ganar en ese juego, la persona incrementa su saldo en lo que gana en el juego,
 de lo contrario no gana nada.-}

jugar :: Persona -> Juego -> Persona
jugar persona = apostar ( pagarApuesta persona )

pagarApuesta :: Persona -> Persona
pagarApuesta persona = persona { dinero = ( dinero persona ) - ( apuesta persona ) }
# eg-pdp-parcial-maquinitas
Resolucion del parcial de maquinitas para la materia PDP de UNSAM.

Correcciones a hacerle:

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


Acá creo que se entendió mal el enunciado: "La información que disponemos de las personas es su nombre, la cantidad de dinero que tienen, su suerte y una lista de factores que pueden ayudarles a ganar distintos juegos, como ser su inteligencia, algún amuleto, etc. De cada factor tenemos un valor numérico que nos indica qué tan valioso es."

"Inteligencia", "Amuleto" y "Paciencia" son ejemplos de factores que pueden ayudar a ganar un juego. Y para cada uno hay un valor numérico que nos dice que tan valioso es. En tu solución hay redundancia de datos.

calcularSuerte :: Persona -> Persona

Yo esperaría que esto devuelva algo del tipo Suerte. Y ojo que el enunciado diferencia la "suerte total" y la "suerte normal".


-----------------
2) Muy bien!

premioJackpot :: MontoJackpot -> Premio
premioJackpot montoJackpot apuesta = montoJackpot --(+montoJackpot) Ver Bien Esto !

Acá debería devolver apuesta + montoJackpot
Si lo querés hacer con aplicación parcial sería premioJackpot montoJackpot = (montoJackpot +)

-----------------
3) Bien!

puedeGanarJuego :: Persona -> Juego -> Bool
puedeGanarJuego persona juego = pasaTodosLosCriterios persona ( criterios juego )

Acá se podría hacer point-free y aprovechar una composición.

pasaTodosLosCriterios :: Persona -> [Criterio] -> Bool
pasaTodosLosCriterios persona = and . map ( aplicarCriterio persona ) 

and . map es un all!

-----------------
4) No me gusta mucho. Usás a la persona para guardar la apuesta cuando podría ser algo externo a ésta. Además te pide "obtener la cantidad total de dinero que puede conseguir un jugador con ese monto si va a un casino" y vos devolvés a la persona.

También hacés un mal uso del where, esas definiciones podrían ser "globales".

A mi me gustaría tener algo del estilo
apostar :: Apuesta -> Persona -> Juego -> Recompensa

-----------------
5) Ojo que pide los nombres, no las personas:
perdedoresDelGrupo :: Casino -> Grupo -> Grupo

Y 
puedeGanarAlMenosUnJuego :: Casino -> Persona -> Bool
puedeGanarAlMenosUnJuego casino persona = or ( map (puedeGanarJuego persona) casino ) -- TODO Ver como componer y usar el flip

or . map es un any!

Y para hacer el TODO es más fácil cambiar el orden de los parámetros.

-----------------
6) En éste punto sí habría que modificar el dinero de la persona, pero la apuesta debería poder ser impuesta desde afuera.

---------------------------

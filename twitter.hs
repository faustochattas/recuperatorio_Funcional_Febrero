data Usuario = Usuario{
    nombre :: Nombre,
    seguidos :: [Nombre]
    } deriving Show

type Nombre = String

marsupial,churrasco,tito,pipi :: Usuario
marsupial = Usuario "@marsupialRengo" ["@don_churrasco", "@titoOk"]
churrasco = Usuario "@don_churrasco" []
tito = Usuario "@titoOk" ["@lapipi", "@marsupialRengo"]
pipi = Usuario "@lapipi" ["@titoOk"]

data Mensaje = Mensaje {
usuario :: Nombre,
texto :: Texto,
favs :: Cantidad 
    } deriving Show

type Texto = String
type Cantidad = Int

unMensaje :: Mensaje
unMensaje = Mensaje "@titoOk" "Las personas que viajan en tren se quejan de llenos" 100
otroMensaje :: Mensaje
otroMensaje = Mensaje "@lapipi" "En mi mundo todos son un pony" 0
otroMensajeMas :: Mensaje
otroMensajeMas = Mensaje "@lapipi" "y comen arcoiris y su popó son mariposas" 0
elUltimoMensaje :: Mensaje
elUltimoMensaje = Mensaje "@titoOk" "No hay problema en cometer errores, el secreto es no pasarlos a producción" 3


--- punto 1 ---

valoracion :: Mensaje -> Int
valoracion mensaje = favs mensaje - (length (texto mensaje) + length (usuario mensaje))

--- punto 2 ---

sigueA :: Usuario -> Usuario -> Bool
sigueA usuario1 usuario2 = nombre usuario2 `elem` seguidos usuario1

--- punto 3 ---

seguidores::Usuario->[Usuario]->[Nombre]
seguidores u (x:xs) | elem (nombre u) (seguidos x) = (nombre x):seguidores u xs
                    | otherwise = seguidores u xs
seguidores u [ ] = [ ]

--- CAMBIAR LA X POR LA U

--- punto 4 ---

type Accion = Mensaje -> Mensaje

agregarUnFav :: Accion
agregarUnFav mensaje = mensaje{favs = favs mensaje + 1}

editar :: String -> Accion
editar palabras mensaje = mensaje{texto = palabras ++ texto mensaje}

repipear :: Usuario -> Accion
repipear repipeador mensaje = mensaje{usuario = nombre repipeador, texto = usuario mensaje ++ ": " ++ texto mensaje, favs = 0}

--- punto 5 ---

queAccionTomar :: Mensaje -> Accion -> Accion -> Mensaje
queAccionTomar mensaje accion1 accion2
                                        | valoracion(accion1 mensaje) > valoracion(accion2 mensaje) = accion1 mensaje
                                        | valoracion(accion2 mensaje) > valoracion(accion1 mensaje) = accion2 mensaje
                                        | otherwise = accion1 mensaje
                                        
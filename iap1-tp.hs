-- Nombre de Grupo: xx
-- Integrante 1: Marcos Wendy, wendymarcos2@gmail.com, DNI 44254843
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- describir qué hace la función: Recibe una Red social, de la cual se posiciona en el primer usuario,
-- toma su nombre, lo agrega en una lista dada por los nombres de usuarios de la red social inicial, pero
-- quitando al usuario mencionado en primer lugar, iterando de esta forma hasta obtener la red social con
-- la lista de usuarios vacía, lo que da fin a la recursión y devuelve la lista con todos los nombres.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | usuarios red == [] = []
                      | otherwise = nombreDeUsuario (primerUsuario red) : nombresDeUsuarios (quitarPrimerUsuario red)


-- Recibe una red social y un usuario 
-- usa la funcion buscarUsuariosRelacionados para obtener la lista de relaciones del usuario
-- en la red social
-- Devuelve la lista de amigos del usuario en la red social
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red usuario = buscarUsuariosRelacionados (relaciones red) usuario


-- Recibe una red social y un usuario 
-- busca los amigos del usuario en la red social y devuelve la cantidad de amigos
-- Devuelve la cantidad de amigos del usuario en la red social
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = length (amigosDe red usuario)

-- Recibe una red social y un usuario
-- devuelve el usuario con mas amigos en la red social
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (primerUsuario red)


-- Recibe la red social
-- Busca si "Roberto Carlos" está en la red
-- Devuelve True si está, False si no está
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaNombreEnRed red "Roberto Carlos"


-- Recibe la red social y un usuario
-- Devuelve una lista con todas las publicaciones de ese usuario 
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = listaDePublicaciones (publicaciones red) usuario 

--Recibe la red social y un usuario
--Recorre todas las publicaciones de la red y retorna una lista con todas 
--las publicaciones que le gustaron al usuario parámetro
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red usuario = listaDePublicacionesQueLeGustanA (publicaciones red) usuario 

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
--Recibe dos usuarios y una red social
--Verifica si las listas de publicaciones que les gustan a cada uno tienen los mismos elementos
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos' (publicacionesQueLeGustanA red u1)(publicacionesQueLeGustanA red u2)

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- Recibe una red social y dos usuarios
-- Retorna un booleano que indica si existe una cadena de relaciones, en formato de lista
-- que inicie con el primer usuario que recibe como parámetro y termine con el segundo
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = listaIncluida (usuarios red) (sublistaDesdeHasta (usuarios red) u1 u2) && sonDeLaRed red (usuarios red) && cadenaDeAmigos (usuarios red) red


-- Funciones auxiliares:

-- Dada una red social válida, devuelve el dato del primer usuario dentro
-- de la lista de usuarios.
primerUsuario :: RedSocial -> Usuario
primerUsuario red = head (usuarios red)

-- Dada una red social válida, devuelve la misma red social, pero
-- eliminando el primer usuario de la lista de usuarios.
quitarPrimerUsuario :: RedSocial -> RedSocial
quitarPrimerUsuario (us, rs, ps) = (tail us, rs, ps)


-- Recibe una lista de relaciones y un usuario
-- Busca en la lista de relaciones si el usuario pertenece a alguna relacion.
-- Si pertenece, suma a la lista de usuarios relacionados al usuario que no es el usuario recibido.
-- Devuelve la lista de usuarios relacionados con el usuario
buscarUsuariosRelacionados :: [Relacion] -> Usuario -> [Usuario] 
buscarUsuariosRelacionados [] _ = []
buscarUsuariosRelacionados  ((u1,u2):rs) usuario | idDeUsuario u1 == idDeUsuario usuario = u2 : buscarUsuariosRelacionados rs usuario
                                  | idDeUsuario u2 == idDeUsuario usuario = u1 : buscarUsuariosRelacionados rs usuario
                                  | otherwise = buscarUsuariosRelacionados rs usuario


-- Recibe una red social y un usuario (primer usuario de la red social)
-- el usuario enviado es comparado con la cantidad de amigos de cada usuario en la red social
-- Finalmente, devuelve el usuario con mas amigos en la red social

usuarioConMasAmigosAux :: RedSocial -> Usuario -> Usuario 
usuarioConMasAmigosAux red usuario | usuarios red == [] = usuario
                                   | cantidadDeAmigos red usuario > cantidadDeAmigos red (primerUsuario red) = usuarioConMasAmigosAux (quitarPrimerUsuario red) usuario
                                   | otherwise = usuarioConMasAmigosAux (quitarPrimerUsuario red) (primerUsuario red)

-- Recibe la red social y un nombre
-- Busca si el nombre está en la red
-- Devuelve True si está, False si no está
estaNombreEnRed :: RedSocial -> String -> Bool
estaNombreEnRed red nombre   | nombresDeUsuarios red == [] = False
                             | nombre == head (nombresDeUsuarios red) = True
                             | otherwise = estaNombreEnRed (quitarPrimerUsuario red) nombre

-- Recibe una lista de publicaciones y un usuario 
-- Recorre toda la lista y si las publicaciones se corresponde al usuario las pone en otra lista
-- Termina retornando una lista con todas las publicaciones de ese usuario
listaDePublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
listaDePublicaciones [] usuario = []
listaDePublicaciones (x:xs) usuario
    |usuario == first x = x:listaDePublicaciones xs usuario
    |otherwise = listaDePublicaciones xs usuario
    where first (a,b,c) = a

--Recibe una lista de publicaciones y un usuario
--Recorre toda las publicaciones de la red social y entra en el apartado de likes
--Devuelve lista con todas las publicaciones que le gustan al usuario recibido como parámetro
listaDePublicacionesQueLeGustanA :: [Publicacion] -> Usuario -> [Publicacion]
listaDePublicacionesQueLeGustanA [] usuario = []
listaDePublicacionesQueLeGustanA (x:xs) usuario
    |pertenece usuario (third x) = x:listaDePublicacionesQueLeGustanA xs usuario
    |otherwise = listaDePublicacionesQueLeGustanA xs usuario
    where third (_,_,c) = c

--Recibe dos listas con publicaciones y las compara
--Si tienen los mismos elementos retorna True, sino False
mismosElementos' :: [Publicacion] -> [Publicacion] -> Bool
mismosElementos' [] [] = True
mismosElementos' _ [] = False
mismosElementos' [] _ = False
mismosElementos' (x:xs) (y:ys) 
    |pertenece x (y:ys) && mismosElementos' xs (quitarTodos x ys) = True
    |x == y && mismosElementos' xs (quitarTodos x ys) = True
    |otherwise = False

--Quita el elemento que recibe como parámetro de la lista que recibe como parámetro
quitar :: (Eq t) => t -> [t] -> [t] 
quitar _ [] = []
quitar a (x:xs) | a == x = xs 
                |otherwise = x: quitar a xs 

--Quita todas las apariciones del elemento que recibe como parámetro de la lista que recibe como parámetro
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos a (x:xs) 
    |estaRepetido a (x:xs) = quitarTodos a (quitar a (x:xs)) 
    |otherwise = quitar a (x:xs)

--Verifica si el elemento dado está repetido en la lista que recibe como parámetro
estaRepetido :: (Eq t) => t -> [t] -> Bool
estaRepetido _ [] = False
estaRepetido a (x:xs) = a == x || estaRepetido a xs


-- Dada una lista de usuarios devuelve una lista recortada
-- Esta lista contiene todos los usuarios de la original desde el primer parámetro hasta el segundo
sublistaDesdeHasta :: [Usuario] -> Usuario -> Usuario -> [Usuario]
sublistaDesdeHasta [] _ _ = []
sublistaDesdeHasta (x:xs) u1 u2
    |posicion (x:xs) x < posicion (x:xs) u1 = sublistaDesdeHasta xs u1 u2
    |posicion (x:xs) x >= posicion (x:xs) u1 && posicion (x:xs) x <= posicion (x:xs) u2 = x: sublistaDesdeHasta xs u1 u2
    |posicion (x:xs) x > posicion (x:xs) u2 = []

-- Dada una lista y un usuario retorna la posicion que ocupa este en dicha lista
posicion :: [Usuario] -> Usuario -> Integer
posicion [] _ = -1
posicion (x:xs) u1 
    |not (pertenece u1 (x:xs)) = 0
    |x /= u1 = 1 + posicion xs u1
    |x == u1 = 1
    |otherwise = posicion xs u1

-- Predicado que dada una red social y una lista de usuarios verifica si esta pertenece a la red
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True
sonDeLaRed red (x:xs) = pertenece x (usuarios red) && sonDeLaRed red xs

-- Predicado que dada una lista de usuarios y una red social
-- Retorna True existe una cadena, tal que si un usuario se relaciona con uno, este se relaciona con el siguiente 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool 
cadenaDeAmigos [] red = False
cadenaDeAmigos [_] red = True
cadenaDeAmigos (x:y:xs) red 
    |relacionadosDirecto x y red && cadenaDeAmigos (y:xs) red = True
    |otherwise = False

-- Dadas dos listas retorna True si la lista del segundo parámetro está incluida en la primera
listaIncluida :: [Usuario] -> [Usuario] -> Bool
listaIncluida [] [] = True
listaIncluida [] _ = False
listaIncluida _ [] = True
listaIncluida (x:xs) (y:ys) = pertenece y (x:xs) && listaIncluida (x:xs) ys

-- Polimorfismo que dado un elemento retorna True si este se encuentra dentro de la lista que recibe como segundo parámetro
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece a (x:xs)
    |a == x = True
    |otherwise = pertenece a xs
    
-- Dados dos usuarios y una red social retorna True si existe una relación entre ambos dentro de la red
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto x y (a,b,c) = pertenece (x,y) b || pertenece (y,x) b     
 

-- Ejemplos de Redes Sociales


-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
--usuario4 = (4, "Mariela")
usuario4 = (4, "Roberto Carlos")

usuario5 = (5, "Natalia")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

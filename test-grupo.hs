module Test where
import Test.HUnit
import Tplisto

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    --test case: Redes sociales con nombres distintos:
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redC) ~?= ["Carlos","Norma","Ruben"], 
    --test case: Ids distintos pero nombres iguales:
    " nombresDeUsuarios 3" ~: (nombresDeUsuarios redB) ~?= ["Juan", "Natalia","Pedro","Natalia"], 

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    --test case: Tiene amigos
    " amigosDe 2" ~: (amigosDe redC (43,"Norma")) ~?= [(23,"Carlos"),(2,"Ruben")],
    --test case: No tiene amigos
    " amigosDe 3" ~: (amigosDe ([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (1,"Pepe")) ~?= [], 
    --test case: Tiene amigos con mismo nombre pero distinto ID
    " amigosDe 4" ~: (amigosDe ([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe"),(40,"Norma")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma")),((40,"Norma"),(23,"Carlos"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= [(43,"Norma"),(40,"Norma")], 



    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    -- test case: Tiene amigos
    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redB (2, "Natalia")) ~?= 2,
    -- test case: No tiene amigos
    " cantidadDeAmigos 3" ~: (cantidadDeAmigos redB (5, "Natalia")) ~?= 0, 
    -- test case: Tiene amigos con distinto ID pero mismo nombre
    " cantidadDeAmigos 4" ~: (cantidadDeAmigos ([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe"),(40,"Norma")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma")),((40,"Norma"),(23,"Carlos"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= 2, 


    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    -- test case: Hay dos usuarios con misma cantidad de amigos y esta es la mayor
    " usuarioConMasAmigos 2" ~: expectAny (usuarioConMasAmigos redA) [(2,"Natalia"), (4,"Mariela")], 
    -- test case: Todos tienen misma cantidad de amigos
    " usuarioConMasAmigos 3" ~: expectAny (usuarioConMasAmigos ([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(1,"Pepe"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])])) [(23,"Carlos"), (2,"Ruben"), (43,"Norma"), (1,"Pepe")], 
    -- test case: Hay un solo usuario con mayor cantidad de amigos
    " usuarioConMasAmigos 4" ~: expectAny (usuarioConMasAmigos redB) [(2,"Natalia")], 
    -- test case: Nadie tiene amigos
    " usuarioConMasAmigos 5" ~: expectAny (usuarioConMasAmigos ([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe")],[],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])])) [(23,"Carlos"), (2,"Ruben"), (43,"Norma"), (1,"Pepe")], 






    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,





    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    -- Tiene más de una publicación
    " publicacionesDe 2" ~: (publicacionesDe redB (1,"Juan")) ~?= [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")])], 
    -- Tiene una sola publicación
    " publicacionesDe 3" ~: (publicacionesDe redC (23,"Carlos")) ~?= [((23,"Carlos"),"hoy se salee",[(1,"Pepe")])], 
    -- Tiene publicaciones no seguidas
    " publicacionesDe 4" ~: (publicacionesDe([(23,"Carlos"),(2,"Ruben"),(43,"Norma"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(1,"Pepe"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")]),((23,"Carlos"),"que frio",[(43,"Norma")])]) (23,"Carlos")) ~?= [((23,"Carlos"),"hoy se salee",[(1,"Pepe")]),((23,"Carlos"),"que frio",[(43,"Norma")])], 
    -- No tiene publicaciones
    " publicacionesDe 5" ~: (publicacionesDe redB (5,"Natalia")) ~?= [], 




    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    -- test case: Hay likes
    " publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA redB (2,"Natalia")) ~?= [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia")])], 
    -- test case: No hay likes
    " publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA ([(23,"Carlos"),(43,"Norma"),(2,"Ruben")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[]),((43,"Norma"),"hace calor",[])]) (43,"Norma")) ~?= [], 
    -- test case: Ninguna publicacion le gusta al usuario parámetro
    " publicacionesQueLeGustanA 4" ~: (publicacionesQueLeGustanA redA (3,"Pedro")) ~?= [], 


    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    -- test case: No les gustan las mismas cosas
    " lesGustanLasMismasPublicaciones 3" ~: (lesGustanLasMismasPublicaciones redB (2,"Natalia") (5,"Natalia")) ~?= False, 
    -- test case: Les gustan las mismas
    " lesGustanLasMismasPublicaciones 4" ~: (lesGustanLasMismasPublicaciones ([(1,"Juan"),(2,"Natalia"),(3,"Pedro"),(5,"Natalia")],[((1,"Juan"),(2,"Natalia")),((3,"Pedro"),(2,"Natalia"))],[((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia"),(2,"Natalia")]),((3,"Pedro"),"Lorem Ipsum",[]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia"),(5,"Natalia")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia")])]) (5,"Natalia") (2,"Natalia")) ~?= True, 
    -- test case: A ninguno le gusta una publicacion
    " lesGustanLasMismasPublicaciones 5" ~: (lesGustanLasMismasPublicaciones redC (23,"Carlos") (43,"Norma")) ~?= True, 
    -- test case: A nadie le gusta nada
    " lesGustanLasMismasPublicaciones 6" ~: (lesGustanLasMismasPublicaciones ([(1,"Juan"),(2,"Natalia"),(3,"Pedro"),(5,"Natalia")],[((1,"Juan"),(2,"Natalia")),((3,"Pedro"),(2,"Natalia"))],[((1,"Juan"),"Este es mi tercer post",[]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[]),((3,"Pedro"),"Lorem Ipsum",[]),((3,"Pedro"),"dolor sit amet",[]),((3,"Pedro"),"consectetur adipiscing elit",[])]) (5,"Natalia") (2,"Natalia")) ~?= True, 





    -- " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    -- -- test case: Alguien le dió like a todas sus publicaciones
    -- " tieneUnSeguidorFiel 2" ~: (tieneUnSeguidorFiel ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe"),(2,"Ruben")]),((23,"Carlos"),"Que genio que es Carlos",[(2,"Ruben")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= True,
    -- -- test case: Tiene más de un seguidor fiel
    -- " tieneUnSeguidorFiel 3" ~: (tieneUnSeguidorFiel ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe"),(2,"Ruben")]),((23,"Carlos"),"Que genio que es Carlos",[(2,"Ruben"),(1,"Pepe")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= True,
    -- -- test case: Alguien le dió like a algunas (no todas) de sus publicaciones
    -- " tieneUnSeguidorFiel 4" ~: (tieneUnSeguidorFiel ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe"),(23,"Carlos")]),((23,"Carlos"),"Que genio que es Carlos",[(23,"Carlos")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= False,
    -- -- test case: El que sería su seguidor fiel no pertenece a la red
    -- " tieneUnSeguidorFiel 5" ~: (tieneUnSeguidorFiel redC (43,"Norma")) ~?= False,
    -- -- test case: Él es el único al que le gustaron todas sus publicaciones
    -- " tieneUnSeguidorFiel 6" ~: (tieneUnSeguidorFiel ([(23,"Carlos"),(43,"Norma"),(2,"Ruben")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[((23,"Carlos"),"hoy se salee",[(1,"Pepe"),(23,"Carlos")]),((23,"Carlos"),"Que genio que es Carlos",[(23,"Carlos")]),((43,"Norma"),"hace calor",[(53,"Jorge")])]) (23,"Carlos")) ~?= False,



    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    -- test case: Existe una secuencia
    " existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos ([(23,"Carlos"),(43,"Norma"),(2,"Ruben")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma"))],[]) (23,"Carlos") (2,"Ruben")) ~?= True,
    -- test case: Uno rompe la cadena
    " existeSecuenciaDeAmigos 3" ~: (existeSecuenciaDeAmigos ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(43,"Norma")),((1,"Pepe"),(23,"Carlos"))],[]) (23,"Carlos") (2,"Ruben")) ~?= False,
    -- test case: No existe cadena
    " existeSecuenciaDeAmigos 4" ~: (existeSecuenciaDeAmigos ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((43,"Norma"),(23,"Carlos")),((2,"Ruben"),(1,"Pepe"))],[]) (23,"Carlos") (2,"Ruben")) ~?= False,
    -- test case: Usuarios parámetro no son cabeza y cola de subcadena
    " existeSecuenciaDeAmigos 5" ~: (existeSecuenciaDeAmigos ([(23,"Carlos"),(43,"Norma"),(2,"Ruben"),(1,"Pepe")],[((23,"Carlos"),(43,"Norma")),((43,"Norma"),(53,"Jorge")),((23,"Carlos"),(1,"Pepe")),((1,"Pepe"),(2,"Ruben")),((2,"Ruben"),(27,"Juana"))],[]) (23,"Carlos") (2,"Ruben")) ~?= False

 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (23, "Carlos")
usuario7 = (43, "Norma")
usuario8 = (2, "Ruben") 


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion7_6 = (usuario7, usuario6)
relacion8_7 = (usuario8, usuario7)


publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario6, "hoy se salee", [(1,"Pepe")])
publicacion1_7 = (usuario7, "hace calor", [(53,"Jorge")])

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

usuariosC = [usuario6, usuario7, usuario8]
relacionesC = [relacion7_6, relacion8_7]
publicacionesC = [publicacion1_6, publicacion1_7]
redC = (usuariosC, relacionesC, publicacionesC)
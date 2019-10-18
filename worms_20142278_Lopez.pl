%Dominio de los predicados
%
%ListaNueva = lista
%Lista2 = lista
%Elemento = elemento de una lista
%Lista = lista
%Daño = numero positivo
%Velocidad = numero positivo
%Equipo = string
%X = entero positivo
%Y = entero positivo
%Vida = numero positivo
%Scene = TDA escenario
%SceneStr = string
%SceneIn = TDA escenario
%Seed = number
%SceneOut = TDA escenario
%Member = entero positivo
%ShootType = string
%Angle = numero entre 0 y 90
%MoveDir = entero positivo
%N = entero positivo
%M = entero positivo
%E = entero positivo
%D = entero positivo
%Auxiliar = lista
%Personaje = TDA personaje
%Proyectil = TDA proyectil
%ListaPersonajes = lista con TDA Personaje
%ListaProyectiles = lista con TDA Proyectil
%
%Predicados
%objetivo = relacionar elementos del escenario
%escena(N,M,D,E,ListaPersonajes,ListaEnemigos,Proyectiles,Auxiliar).
%
%objetivo = construir escenarios
%escenario(N,M,D,E,SceneOut).
%
%objetivo = obtener la cabeza de una lista
%cabeza(Lista,Elemento).
%
%objetivo = obtener el elemento de una lista en una posicion determinada
%obtener(Lista,N,M,Elemento).
%
%objetivo = verificar si la coordenada de un personaje o poyectil se
%encuentra en la lista2
%
%verificarSuperPosicion(Lista,Lista2).
%
%objetivo = reemplazar un elemento de una lista por otro en una posicion
%determinada
%ReplaceInThePosition(Lista,N,Elemento,ListaNueva).
%
%objetivo = verificar que los personajes y enemigos no esten
%superpuestos
%
%VerificarSuperPosicionEquipos(ListaPersonajes,ListaEnemigos).

%objetivo = obtener el largo de una lista
%size(Lista,N).
%
%objetivo = consultar si la entrada es un tipo de disparo
%tipo(ShootType).
%
%objetivo = obtener punto donde caerá el proyectil
%tiroParabolico(ShootType).
%
%objetivo = construir personaje
%personaje(X,Y,Vida,Equipo,Personaje).
%
%objetivo = construir proyectil
%proyectil(X,Y,Angle,Velocidad,Daño,Equipo,Proyectil).
%
%objetivo = validar que la entrada sea un personaje
%esPersonaje(Personaje).
%
%objetivo = validar que la entrada sea un proyectil
%esProyectil(Proyectil).
%
%objetivo = validar que la entrada sea una lista de personajes
%esEquipoPersonajes(ListaPersonajes).
%
%objetivo = validar que la entrada sea una lista de proyectiles
%esEquipoProyectiles(ListaProyectiles).
%
%objetivo = validar que la entrada sea un auxiliar
%esAuxiliar(Auxiliar).
%
%objetivo = consultar si es posible el valor que debe tomar Scene
%createScene(N,M,E,D,Seed,Scene).
%
%objetivo = verificar que la entrada sea un escenario
%checkScene(Scene).
%
%objetivo = validar si es posible mover a un personaje del equipo del
%jugador y moverlo en caso de poder
%moveMember(SceneIn,Member,MoveDir,Seed,SceneOut).
%
%objetivo = iniciar disparo
%shoot(SceneIn,Member,ShootType,Angle,Seed,SceneOut).
%
%objetivo = actualizar escenario después del disparo
%updateScene(SceneIn,Seed,SceneOut).
%
%objetivo = representar escenario como string
%SceneToString(Scene,SceneStr).
%
%TDA'S
%Representacion
%personaje = [[X,Y],Vida,Equipo]
%proyectil = [[X,Y],Angle,Speed,Damage,Equipo]
%scene =[Estado,Dificultad,N,M,ListaPersonajes,ListaEnemigos,ListaProyectiles,Auxiliar]
%
%Selector
%predicado obtener

%Modificador
%predicado replaceInThePosition
%
%Metas
%Principales
%
%proporcionar un escenario válido
%consultar si un escenario (scene) cumple con los criterios para ser considerado como un escenario válido
%mover un personaje de algún equipo en cierta dirección.
%iniciar disparo
%actualizar escenario
%representar escenario como string
%
%Secundarias
%
%obtener elemento de una lista
%reemplazar un elemento de una lista
%verificar que los personajes no esten superpuestos en el escenario
%validar personaje
%validar proyectil
%validar auxiliar
%obtener largo de una lista
%calcular donde caerá el disparo
%
%
%
%
%Hechos

%TABLEROS 5X10
escena(5,10,2,4,[[[3,1],100,"A"],[[3,2],100,"A"],[[3,3],100,"A"],[[3,4],100,"A"]],[[[3,7],100,"E"],[[3,8],100,"E"],[[3,9],100,"E"],[[3,10],100,"E"]],[[[[3,1],45,15,30,"A"],[[3,2],45,15,30,"A"],[[3,3],45,15,30,"A"],[[3,4],45,15,30,"A"]],[[[3,7],45,15,30,"E"],[[3,8],45,15,30,"E"],[[3,9],45,15,30,"E"],[[3,10],45,15,30,"E"]]],[[0,0],0]).
escena(5,10,3,2,[[[3,1],100,"A"],[[3,2],100,"A"],[[3,3],100,"A"]],[[[3,9],100,"E"],[[3,10],100,"E"]],[[[[3,1],100,"A"],[[3,2],100,"A"],[[3,3],100,"A"]],[[[3,9],100,"E"],[[3,10],100,"E"]]],[[0,0],0]).
escena(5,10,3,5,[[[3,1],100,"A"],[[3,2],100,"A"],[[3,3],100,"A"],[[3,4],100,"A"]],[[[3,6],100,"E"],[[3,7],100,"E"],[[3,8],100,"E"],[[3,9],100,"E"],[[3,10],100,"E"]],[[[[3,1],45,15,30,"A"],[[3,2],45,15,30,"A"],[[3,3],45,15,30,"A"],[[3,4],45,15,30,"A"]],[[[3,6],45,15,30,"E"],[[3,7],45,15,30,"E"],[[3,8],45,15,30,"E"],[[3,9],45,15,30,"E"],[[3,10],45,15,30,"E"]]],[[0,0],0]).
%TABLEROS 10X12
escena(10,12,2,4,[[[5,1],100,"A"],[[5,2],100,"A"],[[5,3],100,"A"],[[5,4],100,"A"]],[[[5,9],100,"E"],[[5,10],100,"E"],[[5,11],100,"E"],[[5,12],100,"E"]],[[[[5,1],45,15,30,"A"],[[5,2],45,15,30,"A"],[[5,3],45,15,30,"A"],[[5,4],45,15,30,"A"]],[[[5,9],45,15,30,"E"],[[5,10],45,15,30,"E"],[[5,11],45,15,30,"E"],[[5,12],45,15,30,"E"]]],[[0,0],0]).
escena(10,12,3,6,[[[5,1],100,"A"],[[5,2],100,"A"],[[5,3],100,"A"],[[5,4],100,"A"],[[5,5],100,"A"]],[[[5,7],100,"E"],[[5,8],100,"E"],[[5,9],100,"E"],[[5,10],100,"E"],[[5,11],100,"E"],[[5,12],100,"E"]],[[[[5,1],100,"A"],[[5,2],100,"A"],[[5,3],100,"A"],[[5,4],100,"A"],[[5,5],100,"A"]],[[[5,7],100,"E"],[[5,8],100,"E"],[[5,9],100,"E"],[[5,10],100,"E"],[[5,11],100,"E"],[[5,12],100,"E"]]],[[0,0],0]).
%TABLEROS 20X20
escena(20,20,2,8,[[[10,1],100,"A"],[[10,2],100,"A"],[[10,3],100,"A"],[[10,4],100,"A"],[[10,5],100,"A"],[[10,6],100,"A"],[[10,7],100,"A"],[[10,8],100,"A"]],[[[10,13],100,"E"],[[10,14],100,"E"],[[10,15],100,"E"],[[10,16],100,"E"],[[10,17],100,"E"],[[10,18],100,"E"],[[10,19],100,"E"],[[10,20],100,"E"]],[[[[10,1],100,"A"],[[10,2],100,"A"],[[10,3],100,"A"],[[10,4],100,"A"],[[10,5],100,"A"],[[10,6],100,"A"],[[10,7],100,"A"],[[10,8],100,"A"]],[[[10,13],100,"E"],[[10,14],100,"E"],[[10,15],100,"E"],[[10,16],100,"E"],[[10,17],100,"E"],[[10,18],100,"E"],[[10,19],100,"E"],[[10,20],100,"E"]]],[[0,0],0]).


cabeza([Y|Ys],Y).
verificarSuperposicion([X|Xs],[]).
replaceInThePosition([_|T],1,E,[E|T]).
verificarSuperPosicionEquipos([],[Y|Ys]).
size([],0).
tipo("PARABOLICO").
esEquipoPersonajes([]).
esEquipoProyectiles([]).


%Reglas
escenario(N,M,D,E,SceneO):-escena(N,M,D,E,Pjs,Es,Ms,P),SceneO = ["PLAYING",D,N,M,Pjs,Es,Ms,P].
obtener([X|Xs],N,M,E):-N=M,E = X,!;Mn is M+1,obtener(Xs,N,Mn,E).
verificarSuperposicion([X|Xs],[Y|Ys]):-cabeza(Y,C),not([X|Xs]=C),verificarSuperposicion([X|Xs],Ys).
replaceInThePosition([H|T],P,E,[H|R]):-P > 1, NP is P-1, replaceInThePosition(T,NP,E,R).
verificarSuperPosicionEquipos([X|Xs],[Y|Ys]):-obtener(X,1,1,Par),Pos=Par,verificarSuperposicion(Pos,[Y|Ys]),verificarSuperPosicionEquipos(Xs,[Y|Ys]).
size([_|T],N):-size(T,M),N is M+1.
tiroParabolico(ShootType,Proyectil,Angle,PuntoImpacto):-ShootType=="PARABOLICO",obtener(Proyectil,3,1,Speed),obtener(Proyectil,1,1,Pos),obtener(Pos,2,1,Y),Yn = Y + (Speed**2)*(sin(Angle*2*(pi/180))/(9.81)),floor(Yn,M),obtener(Pos,1,1,X),PuntoImpacto=[X,M].
personaje(X,Y,Vida,Equipo,Personaje):-X>=1,Y>=1,(Vida>0;Vida=<100),(Equipo=="A";Equipo=="E"),Personaje=[[X,Y],Vida,Equipo].
proyectil(X,Y,Angle,Speed,Damage,Equipo,Proyectil):-X>=1,Y>=1,(Angle>=0;Angle=<90),Speed>0,Damage>0,(Equipo=="A";Equipo=="E"),Proyectil=[[X,Y],Angle,Speed,Damage,Equipo].
esPersonaje(Personaje):-size(Personaje,3),obtener(Personaje,1,1,Pos),obtener(Personaje,2,1,Vida),obtener(Personaje,3,1,Equipo),Coord=Pos,V=Vida,Team=Equipo,size(Coord,2),obtener(Coord,1,1,X),Xn=X,obtener(Coord,2,1,Y),Yn=Y,Xn>=1,Yn>=1,(V>0;V=<100),(Team=="A";Team=="E").
esProyectil(Proyectil):-size(Proyectil,5),obtener(Proyectil,1,1,Pos),obtener(Proyectil,2,1,Angle),obtener(Proyectil,3,1,Speed),obtener(Proyectil,4,1,Damage),obtener(Proyectil,5,1,Equipo),Coord=Pos,A=Angle,S=Speed,Daño=Damage,Team=Equipo,size(Coord,2),obtener(Coord,1,1,X),Xn=X,obtener(Coord,2,1,Y),Yn=Y,Xn>=1,Yn>=1,(A>=0;A=<90),S>0,Daño>0,(Team=="A";Team=="E").
esEquipoPersonajes([X|Xs]):-esPersonaje(X),esEquipoPersonajes(Xs).
esEquipoProyectiles([X|Xs]):-esProyectil(X),esEquipoProyectiles(Xs).
esAuxiliar([X|Xs]):-size([X|Xs],2),size(X,2),obtener(X,1,1,EjeX),obtener(X,2,1,EjeY),N=EjeX,M=EjeY,number(N),number(M),integer(Xs).
createScene(N,M,E,D,_,Scene):-escenario(N,M,D,E,X),Scene = X.
checkScene(Scene):-size(Scene,8).
moveMember(SceneIn,Member,MoveDir,Seed,SceneOut):-obtener(SceneIn,5,1,Personajes),obtener(Personajes,Member,1,Personaje),obtener(Personaje,1,1,Posicion),obtener(Posicion,2,1,Y),M=Y,Mn is M+MoveDir,obtener(Posicion,1,1,X),N=X,PosicionNueva=[N,Mn],obtener(SceneIn,6,1,Enemigos),verificarSuperposicion(PosicionNueva,Enemigos),replaceInThePosition(Personaje,1,PosicionNueva,NuevoPJ),replaceInThePosition(Personajes,Member,NuevoPJ,NuevosAliados),replaceInThePosition(SceneIn,5,NuevosAliados,SceneA),obtener(SceneA,7,1,Misiles),obtener(Misiles,1,1,MisilesAliados),obtener(MisilesAliados,Member,1,Proyectil),replaceInThePosition(Proyectil,1,PosicionNueva,NuevoProyectil),replaceInThePosition(MisilesAliados,Member,NuevoProyectil,NuevosMisilesAliados),replaceInThePosition(Misiles,1,NuevosMisilesAliados,NuevosMisiles),replaceInThePosition(SceneA,7,NuevosMisiles,SceneOut).
shoot(SceneIn,Member,ShootType,Angle,Seed,SceneOut):-tipo(ShootType),obtener(SceneIn,7,1,Ms),obtener(Ms,1,1,Mp),obtener(Mp,Member,1,Proyectil),tiroParabolico(ShootType,Proyectil,Angle,Newpos),obtener(SceneIn,8,1,Aux),replaceInThePosition(Aux,1,Newpos,NewAux),replaceInThePosition(NewAux,2,Member,Na),replaceInThePosition(SceneIn,8,Na,SceneOut).
updateScene(SceneIn,Seed,SceneOut):-obtener(SceneIn,8,1,A),obtener(A,1,1,Punto),obtener(A,2,1,Indice),obtener(SceneIn,7,1,Ms),obtener(Ms,1,1,Mp),obtener(Mp,Indice,1,Proyectil),replaceInThePosition(Proyectil,1,Punto,NuevoProyectil),replaceInThePosition(Mp,Indice,NuevoProyectil,MisilesPj),replaceInThePosition(Ms,1,MisilesPj,NuevosMisiles),replaceInThePosition(SceneIn,7,NuevosMisiles,SceneOut).
sceneToString(Scene,SceneStr).




%Ejemplos
%
%createScene(5, 10, 4, 2, 123, S1).
%createScene(10, 12, 6, 3, 123, S2).
%createScene(20, 20, 8, 2, 123, S3).
%
%
%createScene(5, 10, 4, 2, 123, S1),checkScene(S1).
%createScene(10,12, 6, 3, 123, S2),checkScene(S2).
%createScene(20, 20, 8, 2, 123, S3),checkScene(S3).
%
%
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut).
%
%
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut).
%
%
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut),updateScene(SceneIn, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut),updateScene(SceneIn, Seed, SceneOut).
%createScene(N, M, E, D, Seed, Scene),checkScene(Scene),moveMember(SceneIn, Member, MoveDir, Seed, SceneOut),shoot(SceneIn, Member, ShootType, Angle, Seed, SceneOut),updateScene(SceneIn, Seed, SceneOut).





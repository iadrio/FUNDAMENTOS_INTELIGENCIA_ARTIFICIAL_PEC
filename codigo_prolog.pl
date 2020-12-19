%PEC2 Fundamentos de inteligencia artificial
%Iván Adrio Muñiz

%CONOCIMIENTO ADQUIRIDO
%Constantes que representan a los partidos politicos
pp.
psoe.
podemos.
vox.
ciudadanos.

%Lista para almacenar las respuestas a lo largo de la encuesta
:-dynamic respuestas/1.
respuestas([]).

%Lista con los partidos politicos que se incluyen en la encuesta
partidos([pp,psoe,podemos,vox,ciudadanos]).

%Hechos que representan el número de veces que alguien ha elegido una respuesta en una pregunta. pregunta(NumeroPregunta,Partido,NumeroRespuesta,Repeticiones)
%Con estos datos inicializamos el programa de forma que tenga datos para hacer cálculos.
:-dynamic pregunta/4.
pregunta(1,pp,1,0).
pregunta(1,pp,2,3).
pregunta(1,psoe,1,3).
pregunta(1,psoe,2,1).
pregunta(1,podemos,1,1).
pregunta(1,podemos,2,0).
pregunta(1,vox,1,1).
pregunta(1,vox,2,1).
pregunta(1,ciudadanos,1,1).
pregunta(1,ciudadanos,2,0).
pregunta(2,pp,1,0).
pregunta(2,pp,2,1).
pregunta(2,pp,3,2).
pregunta(2,pp,4,0).
pregunta(2,psoe,1,0).
pregunta(2,psoe,2,0).
pregunta(2,psoe,3,3).
pregunta(2,psoe,4,1).
pregunta(2,podemos,1,0).
pregunta(2,podemos,2,0).
pregunta(2,podemos,3,0).
pregunta(2,podemos,4,1).
pregunta(2,ciudadanos,1,0).
pregunta(2,ciudadanos,2,1).
pregunta(2,ciudadanos,3,0).
pregunta(2,ciudadanos,4,0).
pregunta(2,vox,1,0).
pregunta(2,vox,2,2).
pregunta(2,vox,3,0).
pregunta(2,vox,4,0).
pregunta(3,pp,1,0).
pregunta(3,pp,2,0).
pregunta(3,pp,3,3).
pregunta(3,psoe,1,3).
pregunta(3,psoe,2,1).
pregunta(3,psoe,3,0).
pregunta(3,podemos,1,1).
pregunta(3,podemos,2,0).
pregunta(3,podemos,3,0).
pregunta(3,vox,1,0).
pregunta(3,vox,2,2).
pregunta(3,vox,3,0).
pregunta(3,ciudadanos,1,0).
pregunta(3,ciudadanos,2,1).
pregunta(3,ciudadanos,3,0).
pregunta(4,pp,1,3).
pregunta(4,pp,2,0).
pregunta(4,pp,3,0).
pregunta(4,psoe,1,4).
pregunta(4,psoe,2,0).
pregunta(4,psoe,3,0).
pregunta(4,podemos,1,0).
pregunta(4,podemos,2,1).
pregunta(4,podemos,3,0).
pregunta(4,vox,1,0).
pregunta(4,vox,2,0).
pregunta(4,vox,3,2).
pregunta(4,ciudadanos,1,1).
pregunta(4,ciudadanos,2,0).
pregunta(4,ciudadanos,3,0).
pregunta(5,pp,1,2).
pregunta(5,pp,2,0).
pregunta(5,pp,3,1).
pregunta(5,psoe,1,3).
pregunta(5,psoe,2,1).
pregunta(5,psoe,3,0).
pregunta(5,podemos,1,0).
pregunta(5,podemos,2,1).
pregunta(5,podemos,3,0).
pregunta(5,vox,1,0).
pregunta(5,vox,2,0).
pregunta(5,vox,3,2).
pregunta(5,ciudadanos,1,0).
pregunta(5,ciudadanos,2,1).
pregunta(5,ciudadanos,3,0).
pregunta(6,pp,1,0).
pregunta(6,pp,2,3).
pregunta(6,psoe,1,3).
pregunta(6,psoe,2,1).
pregunta(6,podemos,1,1).
pregunta(6,podemos,2,0).
pregunta(6,vox,1,0).
pregunta(6,vox,2,2).
pregunta(6,ciudadanos,1,0).
pregunta(6,ciudadanos,2,1).
pregunta(7,pp,1,1).
pregunta(7,pp,2,2).
pregunta(7,psoe,1,3).
pregunta(7,psoe,2,1).
pregunta(7,podemos,1,1).
pregunta(7,podemos,2,0).
pregunta(7,vox,1,0).
pregunta(7,vox,2,2).
pregunta(7,ciudadanos,1,1).
pregunta(7,ciudadanos,2,0).
pregunta(8,pp,1,2).
pregunta(8,pp,2,1).
pregunta(8,psoe,1,4).
pregunta(8,psoe,2,0).
pregunta(8,podemos,1,1).
pregunta(8,podemos,2,0).
pregunta(8,vox,1,1).
pregunta(8,vox,2,1).
pregunta(8,ciudadanos,1,1).
pregunta(8,ciudadanos,2,0).
pregunta(9,pp,1,2).
pregunta(9,pp,2,1).
pregunta(9,psoe,1,4).
pregunta(9,psoe,2,0).
pregunta(9,podemos,1,1).
pregunta(9,podemos,2,0).
pregunta(9,vox,1,1).
pregunta(9,vox,2,1).
pregunta(9,ciudadanos,1,1).
pregunta(9,ciudadanos,2,0).
pregunta(10,pp,1,1).
pregunta(10,pp,2,2).
pregunta(10,psoe,1,3).
pregunta(10,psoe,2,1).
pregunta(10,podemos,1,1).
pregunta(10,podemos,2,0).
pregunta(10,vox,1,0).
pregunta(10,vox,2,2).
pregunta(10,ciudadanos,1,1).
pregunta(10,ciudadanos,2,0).
pregunta(11,pp,1,1).
pregunta(11,pp,2,2).
pregunta(11,psoe,1,3).
pregunta(11,psoe,2,1).
pregunta(11,podemos,1,1).
pregunta(11,podemos,2,0).
pregunta(11,vox,1,0).
pregunta(11,vox,2,2).
pregunta(11,ciudadanos,1,1).
pregunta(11,ciudadanos,2,0).
pregunta(12,pp,1,1).
pregunta(12,pp,2,2).
pregunta(12,psoe,1,2).
pregunta(12,psoe,2,2).
pregunta(12,podemos,1,1).
pregunta(12,podemos,2,0).
pregunta(12,vox,1,1).
pregunta(12,vox,2,1).
pregunta(12,ciudadanos,1,1).
pregunta(12,ciudadanos,2,0).
pregunta(13,pp,1,0).
pregunta(13,pp,2,3).
pregunta(13,psoe,1,3).
pregunta(13,psoe,2,1).
pregunta(13,podemos,1,1).
pregunta(13,podemos,2,0).
pregunta(13,vox,1,0).
pregunta(13,vox,2,2).
pregunta(13,ciudadanos,1,1).
pregunta(13,ciudadanos,2,0).
pregunta(14,pp,1,0).
pregunta(14,pp,2,3).
pregunta(14,psoe,1,0).
pregunta(14,psoe,2,4).
pregunta(14,podemos,1,1).
pregunta(14,podemos,2,0).
pregunta(14,vox,1,1).
pregunta(14,vox,2,1).
pregunta(14,ciudadanos,1,0).
pregunta(14,ciudadanos,2,1).

%TOMA DE DECISIONES

%Predicados para añadir miembros a la lista de respuestas y de partidos
anadeRespuesta(N) :- respuestas(X),append([N],X,L),retract(respuestas(X)),asserta(respuestas(L)).
anadePartido(N) :- (partidos(Lista),miembro(Lista,N));
                   (partidos(Lista),not(miembro(Lista,N)),retract(partidos(Lista)),asserta(partidos([N|Lista]))).

%Predicados para calcular las probabilidades en función de las respuestas y el partido político
puntuacion(Partido,Puntuacion) :- ((respuestas(Lista),length(Lista,NumeroPreguntas),NumeroPreguntas=\=0, puntua(Partido,Lista,Puntuacion2),Puntuacion is (Puntuacion2/NumeroPreguntas)*100));
((respuestas(Lista),length(Lista,NumeroPreguntas),NumeroPreguntas=0,Puntuacion=0)),!.

puntua(Partido,[],Puntuacion) :- Puntuacion is 0.
puntua(Partido, [N|C],Puntuacion):- (puntua(Partido,C,Puntuacion1),length([N|C],Longitud),pregunta(Longitud,Partido,N,Puntuacion2),numeroRespuestas(Longitud,N,NumeroRespuestas),NumeroRespuestas=\=0,Puntuacion3 is Puntuacion2/NumeroRespuestas, Puntuacion is Puntuacion1 + Puntuacion3);
                                    (puntua(Partido,C,Puntuacion1),length([N|C],Longitud),pregunta(Longitud,Partido,N,Puntuacion2),numeroRespuestas(Longitud,N,NumeroRespuestas),NumeroRespuestas=0,Puntuacion3 is 0, Puntuacion is Puntuacion1 + Puntuacion3).
numeroRespuestas(Pregunta,Respuesta,NumeroRespuestas) :- findall(X,pregunta(Pregunta,_,Respuesta,X),Lista),suma(Lista,NumeroRespuestas).
suma([],Suma) :- Suma = 0,!.
suma([N|S],Suma) :- suma(S,Suma1), Suma is Suma1 + N.
calcularPuntuaciones([],List) :- List = [], !.
calcularPuntuaciones([N|C],List) :- puntuacion(N,Puntuacion),calcularPuntuaciones(C,List1),append([Puntuacion],List1,List).

%Predicados para actualizar la base de conocimiento inicial del problema
actualizaConocimiento(Partido) :- respuestas(List),auxActualiza(List,Partido).
actualizaPregunta(Pregunta,Partido,Respuesta) :- (pregunta(Pregunta,Partido,Respuesta,Old), retract(pregunta(Pregunta,Partido,Respuesta,_)); Old = 0),New is Old + 1,assert(pregunta(Pregunta,Partido,Respuesta,New)).
auxActualiza([],Partido) :- true.
auxActualiza([N|C],Partido) :- length([N|C],Longitud),actualizaPregunta(Longitud,Partido,N),auxActualiza(C,Partido).

%Predicado para calcular el partido con mayor afinidad
predecir(Lista,Maximo) :- partidos(Partidos),calcularPuntuaciones(Partidos,Puntuaciones),calcularMaximo(Puntuaciones,Maximo),setof(X,puntuacion(X,Maximo),Lista).
calcularMaximo([],Max) :- Max=0.
calcularMaximo([N|C],Max) :- calcularMaximo(C,Element1),
	((Element1>N,Max = Element1);
	(N>=Element1,Max=N)).
	
%UTILIDADES
%Reinicia la lista de respuestas
reiniciar() :- retract(respuestas(_)),asserta(respuestas([])).

%Comprobar si un elemnto pertenece a una lista
miembro([N|C],M) :- N=M;miembro(C,M).


%INTERFAZ
%Predicado para iniciar el programa
inicio() :- reiniciar,presentacion,mostrarAyuda,preguntar,fin,repetir.

%Predicados para hacer preguntas
pregunta(1):-nl,tab(5),write('¿Opinas que la constitucion Espanhola debe actualizarse?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).

pregunta(2):-nl,nl,nl,tab(5),write('¿Crees que es necesario hacer cambios en la actual legislacion laboral? '),nl,
	tab(10),write('1) no, esta bien tal cual esta ahora'),nl,
	tab(10),write('2) si, es necesario realizar una reforma profunda para flexibilizarlo y reducir de este modo la temporalidad del empleo'),nl,
	tab(10),write('3) Si, creo que seria interesante implantar el modelo conocido como mochila austriaca'),nl,
	tab(10),write('4) Si,es necesario derrogarla completamente y empezar de 0')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(3):-nl,nl,nl,tab(5),write('¿Que opinas de la conocida como ley mordaza?'),nl,
	tab(10),write('1) Es necesario derrogarla '),nl,
	tab(10),write('2) Es una ley necesaria y que funciona correctamente'),nl,
	tab(10),write('3) Es necesario reforzarla. Es insuficiente')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(4):-nl,nl,nl,tab(5),write('¿Que postura defientes sobre el derecho a referendum en catalunya?'),nl,
	tab(10),write('1) Totalmente en contra. Va contra la ley española.'),nl,
	tab(10),write('2) Es un derecho que le corresponde al pueblo catalan.'),nl,
	tab(10),write('3) Es un atentado contra el order democrático en españa y debe intervenirse inmediatamente todas las instituciones publicas')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(5):-nl,nl,nl,tab(5),write('¿Que opinas acerca del colectivo LGTBI?'),nl,
	tab(10),write('1) Creo que las cosas están bien como están ahora mismo. No es necesaria ninguna medida al respecto.'),nl,
	tab(10),write('2) Creo que son necesarias legislaciones que regulen sus derechos ya que ahora mismo se ven discriminados socialmente en muchos aspectos.'),nl,
	tab(10),write('3) Creo que es necesario legislar sus derechos. Me siento discriminado por este colectivo.')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).

pregunta(6):-nl,tab(5),write('¿Cree que un criminal convicto debería tener derecho a voto? '),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(7):-nl,tab(5),write('¿Crees que deberían subirse los impuestos a las clases sociales altas?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(8):-nl,tab(5),write('¿Crees que es necesario subir el salario minimo?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(9):-nl,tab(5),write('¿Crees que debería recuperarse el servicio militar obligatorio?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(10):-nl,tab(5),write('¿Crees que la educación publica debería ser gratuita?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(11):-nl,tab(5),write('¿Crees que los inmigrantes deben tener derecho a voto?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(12):-nl,tab(5),write('¿Cual es tu postura sobre el aborto?'),nl,
	tab(10),write('1) Libre'),nl,
	tab(10),write('2) Prohibido')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(13):-nl,tab(5),write('¿La marihuana debe ser legal?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
pregunta(14):-nl,tab(5),write('¿Debería españa salir de la union europea?'),nl,
	tab(10),write('1) Si'),nl,
	tab(10),write('2) No')
	,nl,nl,tab(5),write('¿Con que razonamiento estas mas de acuerdo?'),read(Respuesta),anadeRespuesta(Respuesta).
	
	
%Predicados auxiliares para mostrar información por pantalla
presentacion() :- tab(5),write('Hola! Soy un test para predecir tu orientacion politica'),nl,nl,
                  tab(5),write('Mi autor es Ivan Adrio Muniz, y he sido programado como ejercicio practico de la asignatura Fundamentos de Inteligencia Artificial'),nl,nl.

mostrarAyuda() :- tab(5),write('¿Deseas ver las instrucciones o ir directamente al grano?'),nl,nl,
                  tab(5),write('1) Si, muestrame la ayuda'),nl,
				  tab(5),write('2) No, dame preguntas ya'),nl,
				  read(Respuesta),((Respuesta=1,imprimirAyuda);(Respuesta=\=1)).
imprimirAyuda():- tab(5),write('A continuacion te hare una serie de preguntas para tratar de medir tu afinidad a diversos partidos'),nl,
                  tab(5),write('politicos, para ello me basare en una serie de preguntas que ya han respondido diversos personajes'),nl,
                  tab(5),write('publicos cuya idologia politica es abiertamente conocida.'),nl,
                  tab(5),write('Al final del test te mostrare la prediccion que he hecho y te pedire que me indiques si he acertado.'),nl,
				  tab(5),write('Esto lo hago con el fin de retroalimentar el test y que las proximas ejecuciones sean mas precisas.'),nl,
				  tab(5),write('Es algo totalmente anonimo, ya que como puedes ver, no te pedire ninguna informacion personal tuya.'),nl,
				  tab(5),write('Comencemos las preguntas!'),nl,nl.

mostrarAfinidades() :- predecir(X,Y),
			      tab(5),write('Tus afinidades politicas a este momento son:'),nl,
			      tab(5),write(X),write(' con una probabilidad de: '),write(Y),write('%'),nl,nl.
				  
procesar(Respuesta) :- partidos(Partidos),length(Partidos,Longitud),Indice is Longitud- Respuesta,nth0(Indice,Partidos,Partido),actualizaConocimiento(Partido),tab(5),write('Fantastico! Ahora soy un poquito mas inteligente.'),nl.

preguntar() :- findall(X,pregunta(X,psoe,1,_),List),hacerPreguntas(List).
hacerPreguntas([]):-!.
hacerPreguntas([N|C]):- pregunta(N),mostrarAfinidades,hacerPreguntas(C).

imprimePartidos([]) :- !.
imprimePartidos([N|C]) :- imprimePartidos(C),tab(5),length([N|C],Longitud),write(Longitud),write(')'),write(N),nl.
			
fin() :-    tab(5),write('Todo lo bueno se acaba... Hemos llegado al final del test.'),nl,
			mostrarAfinidades,
			tab(5),write('Ahora dime la verdad, ¿A que partido politico te sientes mas afin?'),nl,
			partidos(Partidos),imprimePartidos(Partidos),
			read(Respuesta),procesar(Respuesta).

repetir() :- tab(5),write('¿Quieres repetir el test?'),nl,
			 tab(5),write('1) Si'),nl,
			 tab(5),write('2) No'),nl,
             read(Respuesta),((Respuesta=1,nl,tab(5),write('Genial! Vamos alla de nuevo'),nl,inicio);(Respuesta=\=1,tab(5),write('Adiooooos!'))).

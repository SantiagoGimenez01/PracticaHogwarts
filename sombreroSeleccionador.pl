% esMago(Mago).
esMago(Mago):-
    tipoDeSangre(Mago, _).
% tipoDeSangre(Mago, Sangre).
tipoDeSangre(harry, mestiza).
tipoDeSangre(hermione, impura).
tipoDeSangre(draco, pura).
tipoDeSangre(neville, impura).
tipoDeSangre(luna, pura).
% caracteristicasMago(Mago, Caracteristicas).
caracteristicasMago(harry, [coraje, amistoso, orgullo, inteligencia]).
caracteristicasMago(hermione, [inteligencia, orgullo, responsabilidad, amistoso]).
caracteristicasMago(draco, [inteligencia, orgullo]).
caracteristicasMago(neville, [responsabilidad, coraje, amistoso]).
caracteristicasMago(luna, [amistoso, inteligencia, responsabilidad]).
%noQuiereirA(Mago, Casa).
noQuiereirA(harry, slytherin).  
noQuiereirA(draco, hufflepuff).
% casa(Casa).
casa(gryffindor).
casa(ravenclaw).
casa(hufflepuff).
casa(slytherin).
% caracteristicaCasa(Casa, Caracteristica).
caracteristicaCasa(gryffindor, coraje).
caracteristicaCasa(slytherin, orgullo).
caracteristicaCasa(slytherin, inteligencia).
caracteristicaCasa(ravenclaw, responsabilidad).
caracteristicaCasa(ravenclaw, inteligencia).
caracteristicaCasa(hufflepuff, amistoso).
% Punto 1
% puedeEntrar(Mago, Casa).
puedeEntrar(Mago, Casa):-
    esMago(Mago), 
    casa(Casa),
    Casa \= slytherin.
puedeEntrar(Mago, slytherin):-
    esMago(Mago),
    tipoDeSangre(Mago, Sangre),
    Sangre \= impura.
% Punto 2
% tieneCaracterPara(Mago, Casa).
tieneCaracterPara(Mago, Casa):-
    caracteristicasMago(Mago, CaracteristicasMago), casa(Casa),
    forall(caracteristicaCasa(Casa, Caracteristica), member(Caracteristica, CaracteristicasMago)).
% Punto 3
% puedeQuedarSeleccionado(Mago, Casa).
puedeQuedarSeleccionado(Mago, Casa):-
    tieneCaracterPara(Mago, Casa),
    puedeEntrar(Mago, Casa),
    not(noQuiereirA(Mago, Casa)).
puedeQuedarSeleccionado(hermione, gryffindor).
% Punto 4
% cadenaDeAmistades([Magos]).
cadenaDeAmistades(ListaMagos):-
    todosAmistosos(ListaMagos),
    puedeEstarEnMismaCasa(ListaMagos).

todosAmistosos(ListaMagos):-
    forall(member(Mago, ListaMagos), esAmistoso(Mago)).
esAmistoso(Mago):-
    caracteristicasMago(Mago, Caracteristicas),
    member(amistoso, Caracteristicas).

puedeEstarEnMismaCasa([Mago, MagoSiguiente | RestoMagos]):-
    puedeQuedarSeleccionado(Mago, Casa),
    puedeQuedarSeleccionado(MagoSiguiente, Casa),
    puedeEstarEnMismaCasa([MagoSiguiente | RestoMagos]).
puedeEstarEnMismaCasa([_]).
puedeEstarEnMismaCasa([]).
    

    
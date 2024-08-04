% esMago(Mago).
esMago(Mago):-
    tipoDeSangre(Mago, _).
% tipoDeSangre(Mago, Sangre).
tipoDeSangre(harry, mestiza).
tipoDeSangre(hermione, impura).
tipoDeSangre(draco, pura).
% caracteristicasMago(Mago, Caracteristicas).
caracteristicasMago(harry, [coraje, amistoso, orgullo, inteligencia]).
caracteristicasMago(hermione, [inteligencia, orgullo, responsabilidad]).
caracteristicasMago(draco, [inteligencia, orgullo]).
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
    
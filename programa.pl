% mago(Persona).
mago(harry).
mago(draco).
mago(hermione).
% caracteristicasMago(Mago, caracteristicas(Sangre, Caracteristicas, OdiaCasa)).
caracteristicasMago(harry, caracteristicas(mestiza, [coraje, amistoso, orgullo, inteligencia], [slytherin])).
caracteristicasMago(hermione, caracteristicas(impura, [inteligencia, orgullo, responsabilidad], [])).
caracteristicasMago(draco, caracteristicas(pura, [inteligencia, orgullo], [hufflepuff])).
% caracteristicaCasa(Casa, Caracteristica).
caracteristicaCasa(gryffindor, [coraje]).
caracteristicaCasa(slytherin, [orgullo, inteligencia]).
caracteristicaCasa(ravenclaw, [inteligencia, responsabilidad]).
caracteristicaCasa(hufflepuff, [amistoso]).
% Punto 1
% puedeEntrar(Mago, Casa).
puedeEntrar(Mago, gryffindor):-
    mago(Mago).
puedeEntrar(Mago, ravenclaw):-
    mago(Mago).
puedeEntrar(Mago, hufflepuff):-
    mago(Mago).
puedeEntrar(Mago, slytherin):-
    mago(Mago),
    caracteristicasMago(Mago, caracteristicas(Sangre, _, _)),
    not(Sangre = impura).
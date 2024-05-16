classify(_, _, PetalLength, PetalWidth) :-
    (   PetalLength =< 2.45
    ->  writeln('Iris-setosa')
    ; PetalWidth =< 1.75
    ->  (   PetalLength =< 4.95
        ->  writeln('Iris-versicolor')
        ;   writeln('Iris-virginica')
        )
    ;   writeln('Iris-virginica')
    ).


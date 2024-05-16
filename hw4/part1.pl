% Define the places
place(adm_off).
place(cafe).
place(soc_sci).
place(inst_x).
place(libr).
place(eng_bld).
place(lec_hall).
place(inst_y).


% object(ID, Weight(kg), Pickup Location, Dropoff Location, Urgency, In transit by).
object(obj1, 2, cafe, soc_sci, high, none).
object(obj2, 5, cafe, libr, medium, none).
object(obj3, 3, libr, cafe, low, none).
object(obj4, 4, eng_bld, lec_hall, high, dp1).
object(obj5, 1, lec_hall, eng_bld, medium, none). % This one is in transit.


% deliveryPerson(ID, Capacity(kg), Working Hours, Current Job, Current Location).
deliveryPerson(dp1, 10, 15, none, adm_off).
deliveryPerson(dp2, 15, 1, none, cafe).
deliveryPerson(dp3, 20, 15, obj2, libr).

% Define the routes
route(adm_off, cafe, 4).
route(adm_off, libr, 1).
route(adm_off, eng_bld, 3).
route(cafe, soc_sci, 2).
route(cafe, libr, 5).
route(cafe, adm_off, 4).
route(soc_sci, inst_x, 8).
route(soc_sci, libr, 2).
route(soc_sci, cafe, 2).
route(inst_x, soc_sci, 8).
route(libr, soc_sci, 2).
route(libr, cafe, 5).
route(libr, adm_off, 1).
route(libr, eng_bld, 5).
route(eng_bld, adm_off, 3).
route(eng_bld, libr, 5).
route(eng_bld, lec_hall, 2).
route(lec_hall, eng_bld, 2).
route(lec_hall, inst_y, 3).
route(inst_y, libr, 3).
route(inst_y, lec_hall, 3).

% Connects nodes in both directions
connected(X, Y, L) :- route(X, Y, L).
connected(X, Y, L) :- route(Y, X, L).

% Pathfinding
path(A, B, Path, Length) :-
       travel(A, B, [A], Q, Length),
       reverse(Q, Path).

travel(A, B, P, [B|P], L) :-
       connected(A, B, L).
travel(A, B, Visited, Path, L) :-
       connected(A, C, D),
       C \== B,
       \+member(C, Visited),
       travel(C, B, [C|Visited], Path, L1),
       L is D+L1.

% Find the shortest path
shortest(A, B, Path, Length) :-
   setof([P,L], path(A, B, P, L), Set),
   Set = [_|_], % fail if no path exists
   minimal(Set, [Path, Length]).

minimal([F|R], M) :- min(R, F, M).

% Determines the minimum path
min([], M, M).
min([[P,L]|R], [_,M], Min) :-
   L < M, !, min(R, [P,L], Min).
min([_|R], M, Min) :-
   min(R, M, Min).



list_delivery_status_for_object(ObjectID) :-
    findall(DeliveryPersonID, can_deliver_and_print(ObjectID, DeliveryPersonID), _).




% Modified can_deliver predicate to include printing and not fail the whole predicate if one cannot deliver
can_deliver_and_print(ObjectID, DeliveryPersonID) :-
    object(ObjectID, Weight, Pickup, Dropoff, _, In_Transit),
    deliveryPerson(DeliveryPersonID, Capacity, WorkingHours, Job, CurrentLocation),

    (   In_Transit == none
        -> true
        ;
        format('Object ~w is in transit\n', [ObjectID]), fail
    ),


    (   (Job == none)
    ->  true
    ;
        (   Job == ObjectID
        ->  format('~w is already delivering ~w\n',[DeliveryPersonID, ObjectID]), fail
        ;   format('~w is busy delivering ~w\n', [DeliveryPersonID, Job]), fail
        )
    ),

    (   Capacity >= Weight
        -> true
        ;
        writeln('weight is more than capacity'), fail
    ),

    (   CurrentLocation == Pickup
    ->  TimeToPickup = 0
    ;   shortest(CurrentLocation, Pickup, _, TimeToPickup)
    ),

    shortest(Pickup, Dropoff, _, TimeToDelivery),
    TotalTime is TimeToPickup + TimeToDelivery,


    (   TotalTime =< WorkingHours
    ->  format('~w can deliver ~w in ~w hours\n', [DeliveryPersonID, ObjectID, TotalTime]), true
    ;   format('~w cannot deliver because total time exceeds\n',[DeliveryPersonID]), false
    ).


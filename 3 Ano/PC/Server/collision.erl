-module(collision).
-export([check_collisions/2, check_collisions_sun/1, check_collisions_astronauts/1, distance/2, collision/2, collision_sun/1, collision_astronauts/2,check_outbordas/1,borda/1]).

-import(math, [sqrt/1, pow/2]).

distance({X1, Y1}, {X2, Y2}) ->
    sqrt(pow(X2 - X1, 2) + pow(Y2 - Y1, 2)).

check_collisions(Astronauts, Planets) ->
    [{Astronaut, Planet} || Astronaut <- Astronauts, Planet <- Planets, collision(Astronaut, Planet)].

check_collisions_sun(Astronauts) ->
    [Astronaut || Astronaut <- Astronauts, collision_sun(Astronaut)].

check_collisions_astronauts(Astronauts) ->
    [{A1, A2} || A1 <- Astronauts, A2 <- Astronauts, A1 =/= A2, collision_astronauts(A1, A2)].

check_outbordas(Astronauts) ->
    [Astronaut || Astronaut <- Astronauts, borda(Astronaut)].

borda({{{X,Y}, _, _, _, _, _, _},{_,_}} ) ->
    X =< -650 + 1300/2 orelse X >= 650 + 1300/2  orelse Y =< -350 + 700/2 orelse Y >= 350 + 700/2.

collision({{PosA, _, _, _, _, _, _},{_,_}}, {PosP, _, RadiusP, _, _, _}) ->
    distance(PosA, PosP) =< (20.0 + RadiusP).

collision_sun({{PosA, _, _, _, _, _, _},{_,_}}) ->
    distance(PosA, {650, 350}) =< 50.

collision_astronauts({{PosA1, _, _, _, _, _, _},{_,_}}, {{PosA2, _, _, _, _, _, _},{_,_}}) ->
    distance(PosA1, PosA2) =< 20.0.


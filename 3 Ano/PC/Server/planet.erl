-module(planet).
-export([newPlanet/0, updatePlanetList/1, updatePlanet/1]).
-import(math, [sqrt/1, pow/2, cos/1, sin/1, pi/0]).
-define(GravitationalConstant,1).
-define(SunRadius,50).

newPlanet() ->
    Color = 100 + rand:uniform(105),
    Angle = float(rand:uniform(360)),
    Radius = float(10 + rand:uniform(30-10 - 1)),
    OrbitRadius = float(250 + rand:uniform(75)),
    Speed = float(22/Radius),
    Position = {float(rand:uniform(1300))+50, float(rand:uniform(700))+50},
    {Position, Angle, Radius, OrbitRadius, Speed, Color}.

updatePlanet(Planet) ->
    {_, Angle, Radius, OrbitRadius, Speed, Color} = Planet,
    NewAngle =  Angle + Speed,
    NewX = 1300 / 2 + OrbitRadius * cos(deg2rad(NewAngle)),
    NewY = 700 / 2 + OrbitRadius * sin(deg2rad(NewAngle)),
    {{NewX, NewY}, NewAngle, Radius, OrbitRadius, Speed, Color}.

updatePlanetList(Planets) ->
    [updatePlanet(Planet) || Planet <- Planets].

% Utility function to convert degrees to radians
deg2rad(Deg) -> (Deg * pi()) / 180.

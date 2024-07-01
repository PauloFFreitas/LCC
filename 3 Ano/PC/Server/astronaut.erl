-module(astronaut).
-export([newAstronaut/0, updateAstronautList/1, updateAstronaut/1,update_astronaut_position/2]).
-import(math, [cos/1, sin/1, pi/0]).
-define(GravitationalConstant,1).

newAstronaut() ->
    Boost = float(100),
    Color = 50 + rand:uniform(205),
    OrbitRadius = float(75 + rand:uniform(125)),
    Angle = float(rand:uniform(360)),
    RotationSpeed = -0.5,
    OrbitSpeed = 20.0,
    Position = {float(rand:uniform(1300))+50, float(rand:uniform(700))+50},
    {Position, Angle, OrbitRadius, RotationSpeed, OrbitSpeed, Color, Boost}.

updateAstronaut(Astronaut) ->
    {{_, Angle, OrbitRadius, RotationSpeed, OrbitSpeed, Color,Boost},UserData} = Astronaut,
    SunRadius = 50,
    Force = ?GravitationalConstant * (1 * SunRadius) / (OrbitRadius * OrbitRadius),
    Acceleration = Force / 1,
    Velocity = RotationSpeed, 
    NewOrbitRadius = OrbitRadius - 0.03,
    NewVelocity = Velocity + Acceleration,
    NewAngle = Angle + NewVelocity,
    NewX = 1300 / 2 + OrbitRadius * cos(deg2rad(NewAngle)),
    NewY = 700 / 2 + OrbitRadius * sin(deg2rad(NewAngle)),
    {{{NewX, NewY}, NewAngle, NewOrbitRadius, RotationSpeed, OrbitSpeed, Color,Boost},UserData}.

% Function to update a list of astronauts
updateAstronautList([]) ->
    [];
updateAstronautList(Astronauts) ->
    [updateAstronaut(Astronaut) || Astronaut <- Astronauts].

update_astronaut_position({{{X, Y}, Angle, OrbitRadius, RotationSpeed, OrbitSpeed, Color, Boost}, UserData}, Movement) ->
    NewOrbitRadius = case {Movement, Boost > 0} of
        {<<"R\n">>, true} -> OrbitRadius + OrbitSpeed;
        {<<"L\n">>, true} -> OrbitRadius - OrbitSpeed;
        _ -> OrbitRadius
    end,
    NewBoost = case Boost > 0 of
        true -> Boost - 0.5;
        false -> Boost
    end,
    NewRotationSpeed = case {Movement, Boost > 0} of
        {<<"U\n">>, true} -> RotationSpeed - 0.01;
        _ -> RotationSpeed
    end,
    NewAngle = Angle,
    {{{X, Y}, NewAngle, NewOrbitRadius, NewRotationSpeed, OrbitSpeed, Color, NewBoost}, UserData}.

deg2rad(Deg) -> (Deg * pi()) / 180.

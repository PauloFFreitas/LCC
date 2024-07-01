-module(conversores).
-export([formatState/1, formataTecla/1]).

formataTecla( Data ) ->
    Key = re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    Key.

jogador_para_string(Jogador) ->
    {{{X, Y}, _, _, _, _, Color,Boost},U} = Jogador,
    Lista = [U,float_to_list(X, [{decimals, 3}]), float_to_list(Y, [{decimals, 3}]),integer_to_list(Color),float_to_list(Boost, [{decimals, 3}])],
    string:join(Lista, " ").


jogadores_para_string([]) -> "";
jogadores_para_string([H]) -> jogador_para_string(H) ++ " ";
jogadores_para_string([H|T]) -> jogador_para_string(H) ++ " " ++  jogadores_para_string(T).

planeta_para_string(Planet) ->
    {{X, Y}, _, Radius, _, _, Color} = Planet,
    Lista = [float_to_list(X, [{decimals, 3}]), float_to_list(Y, [{decimals, 3}]),float_to_list(Radius, [{decimals, 3}]),integer_to_list(Color)],
    string:join(Lista, " ").


planetas_para_string([]) -> "";
planetas_para_string([H]) -> planeta_para_string(H) ++ " ";
planetas_para_string([H|T]) -> planeta_para_string(H) ++ " " ++ planetas_para_string(T).

formatState (Estado) ->
    {ListaJogadores,ListaPlanetas, _} = Estado,
    Len1 = integer_to_list(length(ListaJogadores)) ++ " ",
    L1 = [{J,U} || {J, {U,_}} <- ListaJogadores ],
    R1 = jogadores_para_string(L1),
    Len2 = integer_to_list(length(ListaPlanetas)) ++ " ",
    R2 = planetas_para_string(ListaPlanetas),
    Resultado = "Estado " ++ Len1 ++ R1 ++ Len2 ++ R2++"\n",
    Resultado.
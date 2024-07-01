-module (estado).
-export ([start_state/0, update/1, handle_planet_collisions/1, handle_astronaut_collisions/2, handle_sun_collisions/1, update_astronaut_position/3]).
-import(planet, [newPlanet/0, updatePlanetList/1]).
-import(astronaut, [newAstronaut/0, updateAstronaut/1, updateAstronautList/1, update_astronaut_positions/2]).
-import(collision, [distance/2, check_collisions/2, check_collisions_sun/1, check_collisions_astronauts/1, collision/2,check_outbordas/1,borda/1]).
-import (timer, [send_after/3]).
-import (conversores, [formatState/1, formataTecla/1]).


start_state() ->
    io:format(" Novo Jogo~n"),
    register(game,spawn( fun() -> gameManager (novoEstado()) end )),
    Timer = spawn( fun() -> refresh(game) end),
    Salas = criaSalas(),
    register(statePid,spawn( fun() -> lounge(Salas)  end)).

refresh (Pid) -> receive after 10 -> Pid ! {refresh, self()}, refresh(Pid) end.

novaSala() -> 
    [{spawn(fun() -> estado([],[]) end),[]}].

%Cria 4 salas com Pids diferentes 
criaSalas() -> 
    novaSala() ++ novaSala() ++ novaSala() ++ novaSala(). 

lounge(Salas) -> 

    receive 
        {ready,Username, UserProcess} -> 

            Sala = verificaSala(Salas), 
            {Pid,ListaJogadores} = Sala,
            NovasSalas = remove(Sala,Salas),
            Pid ! {ready,Username,UserProcess},

            NovoJogadores = ListaJogadores ++ [{Username,UserProcess}],
            NovoSala = {Pid,NovoJogadores},

            lounge([NovoSala | NovasSalas]);

        {leave, Username, UserProcess}  ->                                            
            ondEstaJogador(Salas,UserProcess) ! {leave,Username,UserProcess},
            lounge(Salas)
    end. 

remove(X, L) ->
    [Y || Y <- L, Y =/= X].


%Encontra o Jogador na sala onde ele se encontra
ondEstaJogador([],UserProcess) -> []; 

ondEstaJogador([{Pid,[X|Y]}|T],UserProcess) -> 
    {User,Process} = X, 

    if Process == UserProcess ->
        Pid;
    true -> 
        if length(Y) == 0 -> 
            ondEstaJogador([T],UserProcess);
        true-> 
            ondEstaJogador([{Pid,Y}|T],UserProcess)
        end
    end.

%Verifica se a sala esta cheia ou nao 
%Se estiver entao vai para a sala seguinte 

verificaSala([H|T]) -> 
    {Pid,ListaJogadores} = H, 

    if length(ListaJogadores) < 4 -> 
        H;
    true -> 
        verificaSala(T)
    end.


esperar_jogadores(Espera_Jogadores, TimerRef) ->
    receive
        {ready, Username, UserProcess} ->
            % Cancela o temporizador atual
            timer:cancel(TimerRef),
            Espera_JogadoresNovo = Espera_Jogadores ++ [{Username, UserProcess}],
            % Define um novo temporizador de 5 segundos
            {ok, NewTimerRef} = timer:send_after(5000, self(), {timeout}),
            esperar_jogadores(Espera_JogadoresNovo, NewTimerRef);

        {timeout} ->
            % Quando o tempo expira, inicia o jogo se houver jogadores suficientes
            if length(Espera_Jogadores) >= 2 ->
                [JogadorPid ! {comeca, game} || {_, JogadorPid} <- Espera_Jogadores],
                [game ! {geraJogador, {Username, UserProcess}} || {Username, UserProcess} <- Espera_Jogadores],
                estado(Espera_Jogadores, []);
            true ->
                % Se não houver jogadores suficientes, continua esperando
                io:format("Não há jogadores suficientes, esperando mais~n"),
                {ok, NewTimerRef} = timer:send_after(10000, self(), {timeout}),
                esperar_jogadores(Espera_Jogadores, NewTimerRef)
            end;

        {leave, Username, UserProcess} ->
            Espera_JogadoresNovo = lists:delete({Username, UserProcess}, Espera_Jogadores),
            esperar_jogadores(Espera_JogadoresNovo, TimerRef)
    end.


estado(Atuais_Jogadores, Espera_Jogadores) ->
    io:format("Entrei no estado ~n"),
    receive
        {ready, Username, UserProcess} ->
            io:format("len ~p ~n", [length(Espera_Jogadores)]),
            if
                length(Espera_Jogadores) < 1 ->
                    io:format("Recebi ready de ~p mas ele vai esperar ~n", [Username]),
                    Espera_JogadoresNovo = Espera_Jogadores ++ [{Username, UserProcess}],
                    {ok, TimerRef} = timer:send_after(10000, self(), {timeout}),
                    esperar_jogadores(Espera_JogadoresNovo, TimerRef);
                true ->
                    io:format("Recebi ready do User ~p e vou adicionar-lo ao jogo ~n", [Username]),
                    Espera_JogadoresAux = Espera_Jogadores ++ [{Username, UserProcess}],
                    [JogadorPid ! {comeca, game} || {_, JogadorPid} <- Espera_JogadoresAux],
                    [game ! {geraJogador, {Username, UserProcess}} || {Username, UserProcess} <- Espera_JogadoresAux],
                    estado(Espera_JogadoresAux, [])
            end;

        {leave, Username, UserProcess} ->
            io:format("Recebi leave do User ~p ~n", [Username]),
            case length(Espera_Jogadores) of
                0 ->
                    Lista = Atuais_Jogadores -- [{Username, UserProcess}],
                    io:format("CASE 0 - A lista de jogadores ativos atuais ~p ~n", [Lista]),
                    estado(Lista, Espera_Jogadores);
                _ ->
                    io:format("A lista de jogadores ativos atuais ~p ~n", [Atuais_Jogadores]),
                    io:format("Vou tirar o da lista ~p ~n", [{Username, UserProcess}]),
                    Lista = Atuais_Jogadores -- [{Username, UserProcess}],
                    io:format("Lista jogador removido ~p ~n", [Lista]),
                    if
                        length(Espera_Jogadores) > 0 ->
                            [H | T] = Espera_Jogadores,
                            {_, UP} = H,
                            UP ! {comeca, game},
                            game ! {geraJogador, H},
                            ListaA = Lista ++ [H];
                        true ->
                            T = [],
                            ListaA = Lista
                    end,
                    estado(ListaA, T)
            end
    end.


novoEstado() ->
    %player, planets, screensize
    NewPlanet1 = planet:newPlanet(),
    NewPlanet2 = planet:newPlanet(),
    NewPlanet3 = planet:newPlanet(),
    State = {[], [NewPlanet1,NewPlanet2,NewPlanet3], {1300,700}},
    io:fwrite("Estado novo Gerado: ~p ~n", [State]),
    State.

adicionaJogador(Estado,Jogador) ->
    {ListaJogadores, ListaPlanetas, TamanhoEcra} = Estado,
    State = { ListaJogadores ++ [{astronaut:newAstronaut(), Jogador}], ListaPlanetas,TamanhoEcra},
    io:fwrite("Estado: ~p ~n", [State]),
    State.

removeJogador(Estado,Jogador) ->
    {ListaJogadores, ListaPlanetas, TamanhoEcra} = Estado,
    State = { ListaJogadores -- [Jogador], ListaPlanetas, TamanhoEcra},
    io:fwrite("Estado com jogador removido: ~p ~n", [State]),
    State.

%Controla tudo o que se passa no jogo 
gameManager(Estado)->
    receive
        {geraJogador, From} ->
            io:format("Vou gerar jogador~n"),    
            gameManager(adicionaJogador(Estado,From));
        

        %Recebe os argumentos de movimentação e atualiza a posição dos jogadores
        {Coordenadas, Data, From} ->
            {ListaJogadores, ListaPlanetas, TamanhoEcra} = Estado,

            % Encontrar e atualizar o jogador
            NovaListaJogadores = lists:map(
                fun({PlayerCordinates, {Username, Pid}} = Jogador) ->
                    case Pid of
                        From ->
                            NovoJogador = astronaut:update_astronaut_position({PlayerCordinates, {Username, Pid}}, Data),
                            NovoJogador;
                        _ ->
                            Jogador
                    end
                end,
                ListaJogadores
            ),

            NovoEstado = {NovaListaJogadores, ListaPlanetas, TamanhoEcra},

            gameManager(NovoEstado);
            
        {refresh, _} ->

            NovoEstado = update(Estado),
            {ListaJogadores,_, _} = Estado,
            Pids = [Pid || {_, {User, Pid}} <- ListaJogadores ],
            [ H ! {line,formatState(NovoEstado)} || H <- Pids],
            gameManager(NovoEstado);

       {leave, From} ->
            io:format("Alguem enviou leave~n"),
            {ListaJogadores, _ , _} = Estado,
            if
                length(ListaJogadores) == 1->
                    [H|T] = ListaJogadores,
                    {_, {_, Pid1}} = H,
                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H));
                        true ->
                            gameManager(Estado)
                    end;
                length(ListaJogadores) == 2 ->
                    [H1,H2 | T] = ListaJogadores,
                    {_, {_, Pid1}} = H1,
                    {_, {_, Pid2}} = H2,
                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H1));
                        Pid2 == From ->
                            gameManager(removeJogador(Estado,H2));
                        true ->
                            gameManager(Estado)
                    end;
                length(ListaJogadores) == 3 ->
                    [H1, H2, H3 |T] = ListaJogadores,
                    {_, {_, Pid1}} = H1,
                    {_, {_, Pid2}} = H2,
                    {_, {_, Pid3}} = H3,
                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H1));
                        Pid2 == From ->
                            gameManager(removeJogador(Estado,H2));
                        Pid3 == From ->
                            gameManager(removeJogador(Estado,H3));
                        true ->
                            gameManager(Estado)
                    end;
                length(ListaJogadores) == 4 ->
                    [H1, H2, H3, H4 | T] = ListaJogadores,
                    {_, {_, Pid1}} = H1,
                    {_, {_, Pid2}} = H2,
                    {_, {_, Pid3}} = H3,
                    {_, {_, Pid4}} = H4,

                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H1));
                        Pid2 == From ->
                            gameManager(removeJogador(Estado,H2));
                        Pid3 == From ->
                            gameManager(removeJogador(Estado,H3));
                        Pid4 == From ->
                            gameManager(removeJogador(Estado,H4));
                        true ->
                            gameManager(Estado)
                    end
            end

    end.



%Verifica as colisoes, atualiza todos os objectos e verifica tambem se alguem ganhou ou perdeu 
update(Estado) ->
    {ListaJogadores, ListaPlanetas, TamanhoEcra} = Estado,
    
    NewPlanets = planet:updatePlanetList(ListaPlanetas),
    NewAstronauts = astronaut:updateAstronautList(ListaJogadores),

    % Verificação de colisões
    CollisionsPlanets = collision:check_collisions(NewAstronauts, NewPlanets),
    CollisionsAstronauts = collision:check_collisions_astronauts(NewAstronauts),
    CollisionsSun = collision:check_collisions_sun(NewAstronauts),
    CollisionsBorder = collision:check_outbordas(NewAstronauts),

    handle_planet_collisions(CollisionsPlanets),
    handle_sun_collisions(CollisionsSun),
    handle_bordas(CollisionsBorder),
    FullyUpdatedAstronauts = handle_astronaut_collisions(CollisionsAstronauts,NewAstronauts),
    verify_victory(Estado),
    {FullyUpdatedAstronauts, NewPlanets, TamanhoEcra}.

verify_victory(Estado) ->
    {ListaJogadores, _, _} = Estado,
    case length(ListaJogadores) of
        1 ->
            {ok, NewTimerRef} = timer:send_after(5000, self(), {timeout}),
            receive
                {timeout} ->
                    if length(ListaJogadores) == 1 ->
                        {_, {Username, Pid}} = hd(ListaJogadores),
                        handle_win(Username, Pid),
                        io:format("~p ganhou~n", [Username]),
                        game ! {leave, Pid};
                    true -> ok
                    end
            after 0 -> ok
            end;
        _ ->
            ok
    end.


handle_loss(User,Pid) ->
    Pid ! {line,"Perdeu\n"},
    login_manager:logout(User),
    login_manager:update_loss(User).

handle_win(User,Pid) ->
    Pid ! {line,"Venceu\n"},
    login_manager:logout(User),
    login_manager:update_win(User).

handle_planet_collisions([]) ->
    [];
handle_planet_collisions([{Astronaut, Planet} | Rest]) ->
    {_, {U, Pid}} = Astronaut,
    io:format("Colisão do astronauta ~p e ~p~n", [Astronaut, Planet]),
    handle_loss(U,Pid),
    game ! {leave, Pid},
    handle_planet_collisions(Rest).

handle_sun_collisions([]) ->
    [];
handle_sun_collisions([Astronaut | Rest]) ->
    {_, {U, Pid}} = Astronaut,
    io:format("Colisão entre ~p e o sol~n", [Astronaut]),
    handle_loss(U,Pid),
    game ! {leave, Pid},
    handle_sun_collisions(Rest).

handle_bordas([]) ->
    [];
handle_bordas([Astronaut | Astronauts]) ->
    {_,{U,Pid}} = Astronaut,
    io:format("O jogador ~p perdeu-se no espaço.~n",[Astronaut]),
    handle_loss(U,Pid),
    game ! {leave, Pid},
    handle_bordas(Astronauts).

handle_astronaut_collisions([], Astronauts) ->
    Astronauts;
handle_astronaut_collisions([{Astronaut1, Astronaut2} | Rest], Astronauts) ->
    {{_, _, OrbitRadius1, _, _, _, _}, _} = Astronaut1,
    {{_, _, OrbitRadius2, _, _, _, _}, _} = Astronaut2,
    io:format("Colisão entre ~p e ~p~n", [Astronaut1, Astronaut2]),
    UpdatedAstronauts =
        case OrbitRadius1 > OrbitRadius2 of
            true ->
                TempAstronauts = update_astronaut_position(Astronauts, Astronaut1, <<"L\n">>),
                update_astronaut_position(TempAstronauts, Astronaut2, <<"R\n">>);
            false ->
                TempAstronauts = update_astronaut_position(Astronauts, Astronaut1, <<"R\n">>),
                update_astronaut_position(TempAstronauts, Astronaut2, <<"L\n">>)
        end,
    handle_astronaut_collisions(Rest, UpdatedAstronauts).

update_astronaut_position([], _, _) ->
    [];
update_astronaut_position([{AstronautData, State} | Rest], Astronaut, Position) when AstronautData == Astronaut ->
    UpdatedAstronaut = astronaut:update_astronaut_position(AstronautData, Position),
    [{UpdatedAstronaut, State} | update_astronaut_position(Rest, Astronaut, Position)];
update_astronaut_position([AstronautData | Rest], Astronaut, Position) ->
    [AstronautData | update_astronaut_position(Rest, Astronaut, Position)].

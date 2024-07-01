-module (server).
-export ([start/0]).
-import (login_manager, [start_Login_Manager/1, create_account/2, close_account/2, login/2, logout/1]). 
-import (estado, [start_state/0]). 

%Cria o servidor 
start () ->
    io:format("Iniciei o Server~n"),
    PidState = spawn ( fun() -> estado:start_state() end),  %Iniciar o processo com o estado do servidor
    register(state,PidState),

    {_, L} = file:consult("Logins.txt"),
    Mapa = maps:from_list(L),

    register(login_manager, spawn( fun() -> login_manager:start_Login_Manager(Mapa) end)), % Login manager
    Port = 22346,
    {ok, Socket} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),    %Socket
    acceptor(Socket).

acceptor ( Socket )->
    {ok, Sock} = gen_tcp:accept(Socket),
    spawn( fun() -> acceptor( Socket ) end), 
    authenticator(Sock).

authenticator(Sock) ->
    io:format("Iniciei o Autenticador~n"),
    receive
        {tcp, _ , Data}->
            StrData = binary:bin_to_list(Data),
            %io:format("Recebi estes Dados~p~n",[StrData]),
            ListaDados = string:tokens(string:substr(StrData,1,(string:len(StrData)-2)), " "),
            LenghtListaDados = length(ListaDados),
            if 
                LenghtListaDados == 1 ->
                    [Acao | _ ] = ListaDados,
                    User = "",
                    Pass = "";
                LenghtListaDados == 2 ->
                    [Acao | Aux] = ListaDados,
                    [User | _ ] = Aux,
                    Pass = "";
                true ->
                    [Acao | Aux] = ListaDados,
                    [User | Passs] = Aux,
                    [Pass1 | _ ] = Passs,
                    Pass = Pass1
            end,

            case Acao of
                "login" when User =:= "" ->
                    io:format("Login Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Login Falhou User inválido\n">>),
                    authenticator(Sock);

                "login" when Pass =:= "" ->
                    io:format("Login Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Login Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "login" ->
                   
                    U = re:replace(User, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),                   
                    
                    case login(U,P) of
                        ok ->
                            io:format("Login Deu ~n"),
                            gen_tcp:send(Sock, <<"Login feito com sucesso!\n">>),
                            user(Sock, U);
                        _ ->
                            io:format("Login nao deu ~n"),
                            gen_tcp:send(Sock,<<"Erro ao fazer login!\n">>),
                            authenticator(Sock) % Volta a tentar autenticar-se
                    end;
                "create_account" when User =:= "" ->
                    io:format("Create Account Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Create Account Falhou User inválido\n">>),
                    authenticator(Sock);

                "create_account" when Pass =:= "" ->
                    io:format("Create Account Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Create Account Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "create_account" ->
                    
                    U = re:replace(User, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    case create_account(U,P) of
                        ok ->
                            io:format("Create Account feito com sucesso! ~n"),
                            gen_tcp:send(Sock, <<"Create Account feito com sucesso!\n">>),
                            %user(Sock, U);
                            authenticator(Sock);
                        _ ->
                            io:format("Username e Password não correspondem! ~n"),
                            gen_tcp:send(Sock,<<"Conta já existente!\n">>),
                            authenticator(Sock)
                    end;

                "close_account" when User =:= "" ->
                    io:format("Close Account Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Close Account Falhou User inválido \n">>),
                    authenticator(Sock);

                "close_account" when Pass =:= "" ->
                    io:format("Close Account Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Close Account Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "close_account" ->
                    
                    U = re:replace(User, "(^\s+)|(\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\s+)|(\s+$)", "", [global,{return,list}]),
                    case close_account(U,P) of
                        ok ->
                            io:format("Close Account feito com sucesso! ~n"),
                            gen_tcp:send(Sock, <<"Close Account feito com sucesso!\n">>),
                            %user(Sock, U);
                            authenticator(Sock);
                        _ ->
                            io:format("Username e Password não correspondem! ~n"),
                            gen_tcp:send(Sock,<<"Username e Password não correspondem!\n">>),
                            authenticator(Sock)
                    end;

                "pontos" ->
                    io:format("PONTOS ~n");


                _ ->
                    gen_tcp:send(Sock,<<"Opção Inválida \n">>),
                    %io:format("dados ~p~n",[Data]),
                    authenticator(Sock)
            end
    end.

user(Sock, Username) ->
    statePid ! {ready, Username, self()},
    gen_tcp:send(Sock, <<"Há espera por vaga\n">>),
    io:format("Estou á espera de um Começa!~n"),
    receive % Enquanto não receber resposta fica bloqueado
        {comeca, GameManager} ->
            gen_tcp:send(Sock, <<"Comeca\n">>),
            io:format("Desbloquiei vou começar o jogo~n"),
            cicloJogo(Sock, Username, GameManager) % Desbloqueou vai para a função principal do jogo
    end.

cicloJogo(Sock, Username, GameManager) -> 
    receive
        {line, Data} -> % line é dados do game manager
            %io:format("ENVIEI ESTES DADOS~p~n",[Data]),
            gen_tcp:send(Sock, Data),
            cicloJogo(Sock, Username, GameManager);
        {tcp, _, Data} -> % Recebemos alguma coisa do socket (Cliente), enviamos para o GameManager
            NewData = re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
            case NewData of
                "quit" ->
                    io:format("Recebi quit~n"),
                    statePid ! {leave, Username, self()},
                    logout(Username),
                    authenticator(Sock);
                _ ->
                    %io:format("RECEBI ESTES DADOS~p~n",[Data]),
                    GameManager ! {keyPressed, Data, self()},
                    cicloJogo(Sock, Username, GameManager)
            end;
        {tcp_closed, _} ->
            statePid ! {leave, Username, self()},
            logout(Username);
        {tcp_error, _} ->
            statePid ! {leave, Username, self()},
            logout(Username)
    end.
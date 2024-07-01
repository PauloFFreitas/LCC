-module(login_manager).
-export([start_Login_Manager/1, create_account/2, close_account/2, login/2, logout/1, mapa_para_string/1, maps_para_string/1, booleanoString/1,update_loss/1,update_win/1]).

% Classe para fazer login
start_Login_Manager(Mapa) ->
    Pid = spawn(fun() -> loop(Mapa) end),
    register(module, Pid).

call(Request) ->
    module ! {Request, self()},  % Enviar request ao módulo com o meu pid
    receive Res -> Res end.      % Esperar receber resposta

update_loss(Username) -> call({update_loss, Username}).

update_win(Username) -> call({update_win, Username}).

create_account(Username, Passwd) -> call({create_account, Username, Passwd}).

close_account(Username, Passwd) -> call({close_account, Username, Passwd}).

login(Username, Passwd) -> call({login, Username, Passwd}).

logout(Username) -> call({logout, Username}).

booleanoString(Estado) ->
    case Estado of
        true -> "true";
        false -> "false"
    end.

mapa_para_string({Username, {Pass, Estado, Vitorias, Derrotas, Nivel}}) ->
    Lista = "{" ++ "\"" ++ Username ++ "\"" ++ ", {" ++ "\"" ++ Pass ++ "\"" ++ "," ++ booleanoString(Estado) ++ "," ++ integer_to_list(Vitorias) ++ "," ++ integer_to_list(Derrotas) ++ "," ++ integer_to_list(Nivel) ++ "}}",
    io:format("TESTE ~s~n", [Lista]),
    Lista.

maps_para_string([]) -> "";
maps_para_string([H]) -> mapa_para_string(H) ++ ".";
maps_para_string([H | T]) -> mapa_para_string(H) ++ "." ++ "\n" ++ maps_para_string(T).

loop(Map) ->
    receive
        {{create_account, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                error when Username =:= "", Pass =:= "" ->
                    From ! bad_arguments,  % Argumentos inválidos
                    loop(Map);
                error ->  % Conta criada com sucesso
                    From ! ok,
                    Map2 = maps:put(Username, {Pass, false, 0, 0, 1}, Map),
                    file:delete("Logins.txt"),
                    file:open("Logins.txt", write),
                    F = maps_para_string(maps:to_list(Map2)),
                    file:write_file("Logins.txt", io_lib:fwrite("~s~n", [F])),
                    loop(Map2);
                _ ->
                    From ! invalid,  % Usuário já existe
                    loop(Map)
            end;

        {{close_account, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, _, _, _, _}} ->  % Conta encontrada e senha correta
                    From ! ok,
                    Map2 = maps:remove(Username, Map),
                    file:delete("Logins.txt"),
                    file:open("Logins.txt", write),
                    F = maps_para_string(maps:to_list(Map2)),
                    file:write_file("Logins.txt", io_lib:fwrite("~s~n", [F])),
                    loop(Map2);
                _ ->
                    From ! invalid,  % Conta não encontrada ou senha incorreta
                    loop(Map)
            end;

        {{login, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, false, V, D, N}} ->
                    From ! ok,
                    loop(maps:put(Username, {Pass, true, V, D, N}, Map));
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{logout, Username}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, true, V, D, N}} ->
                    From ! ok,
                    loop(maps:put(Username, {Pass, false, V, D, N}, Map));
                _ ->
                    From ! invalid,
                    loop(Map)
            end;
        {{update_loss, Username}, From} ->
    case maps:find(Username, Map) of
        {ok, {Pass, Estado, Vitorias, Derrotas, Nivel}} ->
            NovaDerrota = Derrotas + 1,
            NovoNivel = if Nivel == 1 ->  % Nível 1, não faça nada
                            Nivel;
                        true ->
                            case NovaDerrota == erlang:ceil(Nivel / 2) of
                                true -> Nivel - 1;  % Decrementa o nível se o número de derrotas atingir o limite
                                false -> Nivel
                            end
                        end,
            NovoEstado = case NovoNivel /= Nivel of
                            true -> {Pass, Estado, 0, 0, NovoNivel};  % Reinicia vitórias e derrotas se o nível mudar
                            false -> {Pass, Estado, 0, NovaDerrota, NovoNivel}  % Zera vitórias consecutivas
                        end,
            Map2 = maps:put(Username, NovoEstado, Map),
            file:delete("Logins.txt"),
            {ok, File} = file:open("Logins.txt", [write]),
            F = maps_para_string(maps:to_list(Map2)),
            file:write(File, io_lib:fwrite("~s~n", [F])),
            file:close(File),
            From ! ok,
            loop(Map2);
        _ ->
            From ! invalid,  % Usuário não encontrado
            loop(Map)
    end;

{{update_win, Username}, From} ->
    case maps:find(Username, Map) of
        {ok, {Pass, Estado, Vitorias, Derrotas, Nivel}} ->
            NovaVitoria = Vitorias + 1,
            NovoNivel = if NovaVitoria == Nivel -> Nivel + 1;  % Incrementa o nível se o número de vitórias atingir o limite
                        true -> Nivel
                    end,
            NovoEstado = case NovoNivel /= Nivel of
                            true -> {Pass, Estado, 0, 0, NovoNivel};  % Reinicia vitórias e derrotas se o nível mudar
                            false -> {Pass, Estado, NovaVitoria, 0, NovoNivel}  % Zera derrotas consecutivas
                        end,
            Map2 = maps:put(Username, NovoEstado, Map),
            file:delete("Logins.txt"),
            {ok, File} = file:open("Logins.txt", [write]),
            F = maps_para_string(maps:to_list(Map2)),
            file:write(File, io_lib:fwrite("~s~n", [F])),
            file:close(File),
            From ! ok,
            loop(Map2);
        _ ->
            From ! invalid,  % Usuário não encontrado
            loop(Map)
    end



    end.

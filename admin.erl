-module(admin).
-export([start_server/0, server/2, start_client/0, client_listens/1, start/0,
         registra_asistente/2, imprimir_conferencias/0, imprimir_asistentes/0,
         elimina_asistente/1, registra_conferencia/6, inscribe_conferencia/2,
         elimina_conferencia/1,
         asistentes_inscritos/1, lista_asistentes/0, lista_conferencias/0, desinscribe_conferencia/2, change_attendee_limit/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FALTA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% inscribe_conferencia/2
%% desinscribe_conferencia/2
%% conferencias_inscritas/1,
%% lista_asistentes

%% una vez implementadas inscribe_conferencia y desinscribe_conferencia
%% se podra checar elimina_asistente, elimina_conferencia y asistentes_inscritos

%% Validar listas vacias

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FORMATOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Attendee   -> {Uniq_ID, Name, Num_Of_Conf}
%%% Conference -> {Uniq_ID, Title, Spoke_Person, Hour, Limit, [Attendee]}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% starts the server, registers the name to "server"
start_server() -> register(admin_server, spawn(admin, server, [[], []])).

% Denotes the node's name
server_node() -> admin@central.

% (server):
% Main server action handling
server(Attendee_List, Conference_List) ->
    receive
        {Requester, register_attendee, Uniq_ID, Name} ->
            New_Attendees = server_register_attendee(Requester, Uniq_ID, Name, Attendee_List),
            server(New_Attendees, Conference_List);
        {Requester, delete_attendee, Uniq_ID} ->
        %% Borra attendee, falta actualizar datos
            case lists:keymember(Uniq_ID, 1, Attendee_List) of
                true ->
                    New_Attendees = proplists:delete(Uniq_ID, Attendee_List),
                    Requester ! {admin, deleted, attendee, Uniq_ID},
                    New_Attendees,
                    server(New_Attendees, Conference_List);
                _ ->
                    Requester ! {admin, stop, attendee_doesnt_exist},
                    server(Attendee_List, Conference_List)
            end;
        {Requester, register_conference, Uniq_ID, Name, Spoke_Person, Hour,
        Attendee_Limit, Attendees_List} ->
            New_Conference = server_register_conference(Requester, Uniq_ID, Name,
                Spoke_Person, Hour, Attendee_Limit, Attendees_List, Conference_List),
            server(Attendee_List, New_Conference);
        {Requester, delete_conference, Uniq_ID} ->
        %% Borra conference, falta actualizar datos
            case lists:keymember(Uniq_ID, 1, Conference_List) of
                true ->
                    New_Conferences = proplists:delete(Uniq_ID, Conference_List),
                    Requester ! {admin, deleted, conference, Uniq_ID},
                    New_Conferences,
                    server(Attendee_List, New_Conferences);
                _ ->
                    Requester ! {admin, stop, conference_doesnt_exist},
                    server(Attendee_List, Conference_List)
            end;
        {Requester, print_attendees} ->
            if length(Attendee_List) =:= 0 ->
                Requester ! {admin, stop, no_attendees},
                Attendee_List,
                server(Attendee_List, Conference_List);
            true ->
                Requester ! {admin, print, attendees, Attendee_List},
                Attendee_List,
                server(Attendee_List, Conference_List)
            end;
        {Requester, print_attendees, Uniq_ID} ->
            case lists:keyfind(Uniq_ID, 1, Conference_List) of
                {Uniq_ID, _, _, _, _, [Attendee]} ->
                    if length(Attendee) =:= 0 ->
                        Requester ! {admin, stop, no_attendees},
                        Attendee_List,
                        server(Attendee_List, Conference_List);
                    true ->
                        Requester ! {admin, print, attendees, Attendee},
                        Attendee_List,
                        server(Attendee_List, Conference_List)
                    end;
                true ->
                    Requester ! {admin, stop, no_conference_found},
                    server(Attendee_List, Conference_List)
            end;
        {Requester, print_conferences} ->
            if length(Conference_List) =:= 0 ->
                Requester ! {admin, stop, no_conferences},
                Conference_List,
                server(Attendee_List, Conference_List);
            true ->
                Requester ! {admin, print, conferences, Conference_List},
                Conference_List,
                server(Attendee_List, Conference_List)
            end;
        {Requester, print_attendees_with_conferences} ->
            io:fwrite("IMPRIME ASISTENTES CON CONFERENCIAS ~n", []),
            server(Attendee_List, Conference_List);
        {Requester, add_attendee_to_conference, Uniq_ID_Attendee, Uniq_ID_Conference} ->
            %%checks to see attendee is registered
            case lists:keyfind(Uniq_ID_Attendee, 1, Attendee_List) of
                {Uniq_ID_Att, Name_Att, Num_Of_Conf} ->
                    %% Checks attendee has conferences available
                    if Num_Of_Conf > 0 ->
                        %% Checks conference exists
                        case lists:keyfind(Uniq_ID_Conference, 1, Conference_List) of
                            {Uniq_ID_Conf, _, _, _, _, [Attendee]} ->
                                %% Check attendee is not already registered to that conference
                                case lists:keymember(Uniq_ID_Attendee, 1, Attendee) of
                                    true ->
                                        Requester ! {admin, stop, attendee_already_exists},
                                        Attendee,
                                        server(Attendee_List, Conference_List);
                                    _ ->
                                        %%Substract 1 from Num_of_Conf from attendee
                                        New_Attendees = change_attendee_limit(Uniq_ID_Att, Attendee_List, -1),
                                        %%Add attendee to conference attendee list
                                        New_Conf = add_attendee_to_conf(Uniq_ID_Att, Uniq_ID_Conf, Conference_List),
                                        server(New_Attendees, New_Conf)
                                end;
                            true ->
                                Requester ! {admin, stop, no_conference_found},
                                server(Attendee_List, Conference_List)
                        end;
                    true ->
                        Requester ! {admin, stop, attendee_has_full_conferences},
                        Attendee_List,
                        server(Attendee_List, Conference_List)
                    end;
                true ->
                    Requester ! {admin, stop, attendee_is_not_registered},
                    Attendee_List,
                    server(Attendee_List, Conference_List)
            end;
            %% ?????
            %New_Conference = server_delete_conference(Requester, Uniq_ID, Conference_List),
            %server(Attendee_List, New_Conference);
        {Requester, unsubscribe, Att_ID, Conf_Name} ->
            {New_Attendees, New_Conferences} = server_unsubscribe_attendee(Requester, Att_ID, Conf_Name, Attendee_List, Conference_List),
            server(New_Attendees, New_Conferences)
    end.

% (server_register_attendee):
% Registers an attendee
server_register_attendee(Requester, Uniq_ID, Name, Attendee_List) ->
    case lists:keymember(Uniq_ID, 1, Attendee_List) of
        true ->
            Requester ! {admin, stop, attendee_already_exists},
            Attendee_List;
        _ ->
            Requester ! {admin, registered, Name},
            [{Uniq_ID, Name, 3} | Attendee_List]
    end.

% (server_register_conference)
% Registers a new conference 
server_register_conference(Requester, Uniq_ID, Name, Spoke_Person, Hour,
    Attendee_Limit, Attendees_List, Conference_List) ->
    case lists:keymember(Uniq_ID, 1, Conference_List) of
        true ->
            Requester ! {admin, stop, conference_already_exists},
            Conference_List;
        _ ->
            Requester ! {admin, registered, Name},
            [{Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List} | Conference_List]
    end.

% (server_delete_conference )
% Deletes a conference 
server_delete_conference(Requester, Uniq_ID, Conference_List) ->
    case lists:keymember(Uniq_ID, 1, Conference_List) of
        true ->
            lists:keydelete(Uniq_ID, 1, Conference_List),
            Requester ! {admin, deleted, conference, Uniq_ID},
            Conference_List;
        _ ->
            Requester ! {admin, stop, conference_doesnt_exist}
    end.    

%
% (server_unsubscribe_attendee)
% Unsubscribes an attendee from the conference list provided
server_unsubscribe_attendee(Requester, Att_ID, Conf_Name, Att_List, Conf_List) ->
    New_Attendees = change_attendee_limit(Att_ID, Att_List, 1),
    New_Conferences = delete_attendee_from_conference(Att_ID, Conf_Name, Conf_List),
    {New_Attendees, New_Conferences}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

registra_asistente(Uniq_ID, Name) ->
    admin_client ! {register, Uniq_ID, Name}.

elimina_asistente(Uniq_ID)->
    admin_client ! {delete_attendee, Uniq_ID}.

registra_conferencia(Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List) -> 
    admin_client ! {register, Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List}.

elimina_conferencia(Uniq_ID)->
    admin_client ! {delete_conference, Uniq_ID}.

imprimir_asistentes() ->
    admin_client ! print_attendees.

imprimir_conferencias() ->
    admin_client ! print_conferences.

asistentes_inscritos(Uniq_ID) ->
    admin_client ! {print_attendees, Uniq_ID}.

lista_asistentes() ->
    admin_client ! print_attendees_with_conferences.

lista_conferencias() ->
    imprimir_conferencias().

inscribe_conferencia(Uniq_ID_Attendee, Uniq_ID_Conference) ->
    admin_client ! {add_attendee_to_conference, Uniq_ID_Attendee, Uniq_ID_Conference}.

desinscribe_conferencia(Attendee_ID, Conference) ->
    admin_client ! {unsubscribe, Attendee_ID, Conference}.

start_client() ->
    case whereis(admin_client) of
        undefined -> register(admin_client, 
                              spawn(admin, client_listens, [server_node()]));
        _ -> already_created_client
    end.

client_listens(Server_Node) ->
    receive
        {register, Uniq_ID, Name} ->
            {admin_server, Server_Node} ! {self(), register_attendee, Uniq_ID, Name},
            await_result();
        {delete_attendee, Uniq_ID} ->
            {admin_server, Server_Node} ! {self(), delete_attendee, Uniq_ID},
            await_result();
        {register, Uniq_ID, Name, Lecturer, Hour, Attendee_Limit, Attendees_List} ->
            {admin_server, Server_Node} ! {self(), register_conference, Uniq_ID, Name,
            Lecturer, Hour, Attendee_Limit, Attendees_List},
            await_result();
        {delete_conference, Uniq_ID} ->
            {admin_server, Server_Node} ! {self(), delete_conference, Uniq_ID},
            await_result();
        {unsubscribe, Attendee_ID, Conference_Name} ->
            {admin_server, Server_Node} ! {self(), unsubscribe, Attendee_ID, Conference_Name};
        print_attendees ->
            {admin_server, Server_Node} ! {self(), print_attendees},
            await_result();
        {print_attendees, Uniq_ID} ->
            {admin_server, Server_Node} ! {self(), print_attendees, Uniq_ID},
            await_result();
        print_conferences ->
            {admin_server, Server_Node} ! {self(), print_conferences},
            await_result();
        print_attendees_with_conferences ->
            {admin_server, Server_Node} ! {self(), print_attendees_with_conferences},
            await_result();
        {add_attendee_to_conference, Uniq_ID_Attendee, Uniq_ID_Conference} ->
            {admin_server, Server_Node} ! {self(), add_attendee_to_conference, Uniq_ID_Attendee, Uniq_ID_Conference},
            await_result()
    end,
    client_listens(Server_Node).

await_result() ->
    receive
        {admin, registered, Who} ->
            io:format("Se registro a ~p~n", [Who]);
        {admin, stop, Why_Man} ->
            io:format("Stopped, reason: ~p~n", [Why_Man]);
        {admin, deleted, What, Identifer} ->
            io: format("Se borro a un ~p con Identificador ~p~n", [What, Identifer]);
        {admin, print, What, List} ->
            io:format("~p~n", [List]),
            case What of
                attendees ->
                    lists:foreach(fun print_attendee/1, List);
                conferences ->
                    lists:foreach(fun print_conference/1, List)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> 
    start_server(), start_client(),
    registra_asistente(1, "Luis_1"),
    registra_asistente(2, "Luis_2"),
    registra_asistente(3, "Luis_3"),
    registra_asistente(4, "Luis_4"),
    registra_asistente(5, "Luis_5"),
    registra_asistente(6, "Luis_6"),
    registra_asistente(7, "Luis_7"),
    registra_asistente(8, "Luis_8"),
    registra_asistente(9, "Luis_9"),
    registra_asistente(10, "Luis_10"),
    registra_asistente(11, "Luis_11"),
    registra_asistente(12, "Luis_12"),
    registra_asistente(13, "Luis_13"),
    registra_asistente(14, "Luis_14"),
    registra_asistente(15, "Luis_15"),
    registra_asistente(16, "Luis_16"),
    registra_conferencia(1, "Evento_1", "Marco_1", 8, 20,[]),
    registra_conferencia(2, "Evento_2", "Marco_2", 8, 20,[]),
    registra_conferencia(3, "Evento_3", "Marco_3", 10, 20,[]),
    registra_conferencia(4, "Evento_4", "Marco_4", 10, 20,[]),
    registra_conferencia(5, "Evento_5", "Marco_5", 16, 20,[]).


print_attendee({Uniq_ID, Name, _}) ->
    io:format("ID: ~p Nombre: ~p~n", [Uniq_ID, Name]).

print_conference({Uniq_ID, Name, Lecturer, Hour, Attendee_Limit, Attendees_List}) ->
    io:format("ID: ~p Nombre: ~p Conferencista: ~p Hora: ~p Limite de asistentes:
                ~p Lista de asistentes: ~p~n", [Uniq_ID, Name, Lecturer, Hour,
                Attendee_Limit, Attendees_List]).


% 
% (change_attendee_limit)
% 
% Changes the attendee event limit by some number Num.
% @param Attendee Name
% @param Attendee List
% @param The number to be added to the limit (can be negative)
% @return the changed list (unchanged if not found)
change_attendee_limit(_, [], _) -> [];
change_attendee_limit(Att_ID, [{ID, Name, Limit}|XS], Num) when Att_ID == ID ->
    [{ID, Name, Limit + Num}] ++ XS;
change_attendee_limit(Att_ID, [X|XS], Num) ->
    [X] ++ change_attendee_limit(Att_ID, XS, Num).


% 
% (delete_attendee_from_conference)
% 
% Deletes the attendee from a given conference.
% @param Attendee Name
% @param Conference Name
% @param Conference List
% @return the list of modified conferences (unchanged if not found)
delete_attendee_from_conference(_, _, []) -> [];
delete_attendee_from_conference(Att_ID, Conf_Name, [{Uniq_ID, Title, Speaker, Hour, Limit, Att_List}|XS]) when Conf_Name == Title ->
    [{Uniq_ID, Title, Speaker, Hour, Limit, (Att_List -- [Att_ID])}] ++ XS; % (--) deletes element from array
delete_attendee_from_conference(Att_ID, Conf_Name, [X|XS]) ->
    [X] ++ delete_attendee_from_conference(Att_ID, Conf_Name, XS).

% 
% (add_attendee_to_conf)
% 
% Deletes the attendee from a given conference.
% @param Attendee Name
% @param Conference Name
% @param Conference List
% @return the list of modified conferences (unchanged if not found)
add_attendee_to_conf(_, _, []) -> [];
add_attendee_to_conf(Att_ID, Conf_Name, [{Uniq_ID, Title, Speaker, Hour, Limit, Att_List}|XS]) when Conf_Name == Title ->
    [{Uniq_ID, Title, Speaker, Hour, Limit, (Att_List ++ [Att_ID])}] ++ XS;
add_attendee_to_conf(Att_ID, Conf_Name, [X|XS]) ->
    [X] ++ add_attendee_to_conf(Att_ID, Conf_Name, XS).

% 
% (get_conference_attendees)
% 
% Retrieves the attendee list of a given conference name.
% @param Conference Name
% @param Conference List
% @return the list of attendees (empty if not found)
get_conference_attendees(_, []) -> [];
get_conference_attendees(Conf_Name, [{_, Title, _,_,_, Att_List}]) when Conf_Name == Title ->
    Att_List;
get_conference_attendees(Conf_Name, [_|XS]) ->
    get_conference_attendees(Conf_Name, XS).

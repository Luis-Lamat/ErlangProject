-module(admin).
-export([start_server/0, server/2, registra_asistente/2, 
         start_client/0, client_listens/1, start/0, print_attendee/1,
         print_conference/1, imprimir_conferencias/0 ,
         imprimir_asistentes/0, elimina_asistente/1, registra_conferencia/6, elimina_conferencia/1]).

%%% FORMATS:
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
            New_Attendees = server_delete_attendee(Requester,Uniq_ID, Attendee_List),
            server(New_Attendees, Conference_List);
        {Requester, register_conference, Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List} ->
            New_Conference = server_register_conference(Requester, Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List, Conference_List),
            server(Attendee_List, New_Conference);
        {Requester, delete_conference, Uniq_ID} ->
            New_Conference = server_delete_conference(Requester, Uniq_ID, Conference_List),
            server(Attendee_List, New_Conference);
        print_attendees ->
            io:format("~p~n", [Attendee_List]),
            lists:foreach(fun print_attendee/1, Attendee_List),
            server(Attendee_List, Conference_List);
        print_conferences ->
            io:format("~p~n", [Conference_List]),
            lists:foreach(fun print_conference/1, Conference_List),
            server(Attendee_List, Conference_List)
    end.

%Fixes errors

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

% (server_delete_atendee )
% Deletes an attendee 
server_delete_attendee(Requester, Uniq_ID, Attendee_List) ->
    case lists:keymember(Uniq_ID, 1, Attendee_List) of
        true ->
            lists:keydelete(Uniq_ID, 1, Attendee_List),
            Requester ! {admin, deleted, attendee, Uniq_ID},
            Attendee_List;
        _ ->
            Requester ! {admin, stop, attendee_doesnt_exist}
    end.

% (server_register_conference)
% Registers a new conference 
server_register_conference(Requester, Uniq_ID, Name, Spoke_Person, Hour, Attendee_Limit, Attendees_List, Conference_List) ->
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
            {admin_server, Server_Node} ! {self(), register_conference, Uniq_ID, Name, Lecturer, Hour, Attendee_Limit, Attendees_List},
            await_result();
        {delete_conference, Uniq_ID} ->
            {admin_server, Server_Node} ! {self(), delete_conference, Uniq_ID},
            await_result();
        print_attendees ->
            {admin_server, Server_Node} ! print_attendees;
        print_conferences ->
            {admin_server, Server_Node} ! print_conferences
    end,
    client_listens(Server_Node).

await_result() ->
    receive
        {admin, registered, Who} ->
            io:format("Se registro a ~p~n", [Who]);
        {admin, stop, Why_Man} ->
            io:format("Stopped, reason: ~p~n", [Why_Man]);
        {admin, deleted, What, Identifer} ->
            io: format("Se borro a un ~p con Identificador ~p~n", [What, Identifer])
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
    registra_conferencia(1, "Evento_1", "Marco_1", 3, 20,[]),
    registra_conferencia(2, "Evento_2", "Marco_2", 3, 20,[]),
    registra_conferencia(3, "Evento_3", "Marco_3", 3, 20,[]),
    registra_conferencia(4, "Evento_4", "Marco_4", 3, 20,[]),
    registra_conferencia(5, "Evento_5", "Marco_5", 3, 20,[]).


print_attendee({Uniq_ID, Name, Num_Of_Conf}) ->
    io:format("ID: ~p Nombre: ~p Conferencias Restantes: ~p ~n", [Uniq_ID, Name, Num_Of_Conf]).

print_conference({Uniq_ID, Name, Lecturer, Hour, Attendee_Limit, Attendees_List}) ->
    io:format("ID: ~p Nombre: ~p Conferencista: ~p Hora: ~p Limite de asistentes: ~p Lista de asistentes: ~p ~n", [Uniq_ID, Name, Lecturer, Hour, Attendee_Limit, Attendees_List]).

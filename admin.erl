-module(admin).
-export([start_server/0, server/2, registra_asistente/2, 
         start_client/0, client_listens/1, start/0, print_attendee/1,
         imprimir_asistentes/0]).

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
        print_attendees ->
            io:format("~p~n", [Attendee_List]),
            lists:foreach(fun print_attendee/1, Attendee_List),
            server(Attendee_List, Conference_List)
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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

registra_asistente(Uniq_ID, Name) ->
    admin_client ! {register, Uniq_ID, Name}.

elimina_asistente(Uniq_ID)->
    admin_client ! {delete, Uniq_ID}.

imprimir_asistentes() ->
    admin_client ! print_attendees.

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
        {delete, Uniq_ID} ->
            {admin_server, Server_Node} ! {self(), delete_attendee, Uniq_ID},
            await_result();
        print_attendees ->
            {admin_server, Server_Node} ! print_attendees
    end,
    client_listens(Server_Node).

await_result() ->
    receive
        {admin, registered, Who} ->
            io:format("Se registro a ~p~n", [Who]);
        {admin, stop, Why_Man} ->
            io:format("Stopped, reason: ~p~n", [Why_Man]);
        {admin, deleted, What, Identifer} ->
            io: format("Se borro a un ~p con Identificador ~p~n");
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
    registra_asistente(16, "Luis_16").

print_attendee({Uniq_ID, Name, Num_Of_Conf}) ->
    io:format("~-15p ~-10p Especios:~p", [Uniq_ID, Name, Num_Of_Conf]).



-module(admin).
-export([start_server/0, server/2, registra_asistente/2, 
         start_client/0, client_listens/1, start/0, print_attendee/1,
         imprimir_asistentes/0]).

%%% FORMATS:
%%% Attendee   -> {Uniq_ID, Name, Num_Of_Conf}
%%% Conference -> {Uniq_ID, Title, Spoke_Person, Hour, Limit, [Attendee]}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> start_server(), start_client().

print_attendee({Uniq_ID, Name, Num_Of_Conf}) ->
    io:format("~-15p ~-10p Espacios:~p", [Uniq_ID, Name, Num_Of_Conf]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

registra_asistente(Uniq_ID, Name) ->
    admin_client ! {register, Uniq_ID, Name}.

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
        print_attendees ->
            {admin_server, Server_Node} ! print_attendees
    end,
    client_listens(Server_Node).

await_result() ->
    receive
        {admin, registered, Who} ->
            io:format("Se registro a ~p~n", [Who]);
        {admin, stop, Why_Man} ->
            io:format("Stopped, reason: ~p~n", [Why_Man])
    end.

%%% admin:registra_asistente(a01191158, "Luis").
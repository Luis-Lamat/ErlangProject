-module(messenger).
-export([start_server/0, server/1, logon/1, logoff/0, message/2, client/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% The user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
%%%

% starts the server, registers the name to "server"
start_server() -> register(messenger, spawn(messenger, server, [[]])).

% Denotes the node's name
server_node() -> 'server@Luiss-MBP-2'.

% Main request handling block
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("List is now: ~p~n", [User_List]),
            server(User_List)
    end.

% (server_logon) -> Logging on function
% If name of user already exists, rejects...
% Else it send a message (!) to the user: 'Successfully logged on'
server_logon(From, Name, User_List) ->
    case lists:keymember(Name, 2, User_List) of
        true -> % if user is found, reject login
            From ! {messenger, stop, user_exists_in_node},
            User_List; % Just return the original user list
        false ->
            From ! {messenger, logged_on},
            [{From, Name} | User_List]
    end.

% (server_logoff) -> Logging off function
% Deletes the user record from the current User_List
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

% (server_transfer) -> First validation of sending message
% Checks if user that is trying to send message exists.
% If false return error
% Else pass the 'stick' to next validation function
server_transfer(From, To, Message, User_List) ->
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, youre_not_logged_of};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

server_transfer(From, Name, To, Message, User_List) ->
    case lists:keysearch(To, 2, User_List) of
        {value, {ToPid, _}} ->
            ToPid ! {message_from, Name, Message},
            From  ! {messenger, message_sent};
        _ ->
            From ! {messenger, stop, receiver_not_found}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logon(Name) ->
    case whereis(mess_client) of
        undefined -> register(mess_client, 
                              spawn(messenger, client, [server_node(), Name]));
        _ -> already_loggend_in
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of
        undefined -> not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
    end.

% (client) -> This is the motherflippin' client process 
% referenced as 'mess_client' in the register function
% Passes the 'stick' to next (client) func where the rest of the messages land.
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("Stopped: ~p~n", [Why]);
        {messenger, What} ->
            io:format("~p~n", [What])
    end.
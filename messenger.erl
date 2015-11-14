-module(messenger).
-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% starts the server, registers the name to "server"
start_server() -> register(messenger, spawn(messenger, server, [[]])).

% Denotes the node's name
server_node() -> messenger@main.

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
            From ! {messenger, successfully_logged_on},
            [{From, Name} | User_List];
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
        false ->
            From ! {messenger, stop, receiver_not_found};
        {value, {ToPid, Name}} ->
            ToPid ! {message_from, Name, Message},
            From  ! {messenger, message_sent}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
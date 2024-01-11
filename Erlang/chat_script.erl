

%
% Description & Instructions
%
% WARNING: The functionality to remove clients from the client list is not implemented
% as it was not part of the assignment description. Therefore, if you would like to run
% this program multiple times, you will need to abort the server with 'ctrl + c' + `a`
% before starting a new instance. Not doing so will result in erratic behavior!

% Compile this program by typing 'erl' into the console and then 
% typing 'c(chat_script), c(chat_server), c(chat_client).'

% To run the program, type chat_script:start(). into the erlang console.  

-module(chat_script).
-export([start/0]).

%% Start the server with multiple clients
start() -> 
	% Get server PID
	chat_server:start_link(),
	ChatServer = whereis(chat_server),

	% Initialize clients
	Client1 = spawn(fun() -> chat_client:start_link() end),
	Client2 = spawn(fun() -> chat_client:start_link() end),
	Client3 = spawn(fun() -> chat_client:start_link() end),

	% Connect the clients to the server
	chat_server:connect(Client1),
	chat_server:connect(Client2),
	chat_server:connect(Client3),
	% Provide delay for clients to connect
	timer:sleep(1000),

	% Confirming unique PIDs
		io:format("~nServer  PID: ~p~n", [ChatServer]),
		io:format("Client1 PID: ~p~n", [Client1]),
		io:format("Client2 PID: ~p~n", [Client2]),
		io:format("Client3 PID: ~p~n~n", [Client3]),

	% Begin messaging between clients
	chat_client:send_message(Client1, "Hello from client 1"),
	chat_client:send_message(Client2, "Hello from client 2"),
	chat_client:send_message(Client3, "Hello from client 3").

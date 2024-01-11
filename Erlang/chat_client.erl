
%
% Description & Instructions
%
% Please see the description & instructions in chat_script.erl

-module(chat_client).
-export([start_link/0, send_message/2, loop/0]).

%% Start the chat client and return PIDs
start_link() -> 
	chat_server:start_link(),
	loop(),
	?MODULE.

%% Casts client messages to the gen_server; finds server, then casts
send_message(ClientPID, Message) ->
	PID = whereis(chat_server),
	gen_server:cast(PID, {message, Message, PID, ClientPID}).

%% Client loop to receive messages
loop() ->
	receive
		%% If message with `message` tag received, ack to io
		{message, Message} ->
			io:format("~p received ~p~n", [self(), Message]),
			loop();

		%% Restart loop if any other msg received
		_ ->
			loop()
	end.



%
% Description & Instructions
%
% Please see the description & instructions in chat_script.erl

-module(chat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Export the necessary functions for operation
-export([start_link/0, 
	connect/1, 
	broadcast/2, 
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2]).


%% Stores details on server state
-record(state, {clients = []}).

%% Starts the chat server and return PID
start_link() -> 
	gen_server:start_link({local, chat_server}, chat_server, [], []).

%% Provide connection to chat server
connect(Client) -> 
	gen_server:cast(chat_server, {connect, Client}).

%% Provide broadcast functionality for clients
broadcast(Message, Sender) -> 
	gen_server:cast(?MODULE, {broadcast, Message, Sender}).

%% Initialize the server with list of connected clients
init([]) ->
	{ok, #state{clients = []}}.


%% Handle connections for client
handle_call(get_clients, _From, State) ->
	{reply, State#state.clients, State}.

%% Handle connect message for client
handle_cast({connect, Client}, State) ->
	io:format("Adding Client ~p to server~n", [Client]),
	NewClients = State#state{clients = [Client | State#state.clients]},
	{noreply, NewClients};

%%Handle messages sent to server under send_message/2
handle_cast({message, Message, ServerPID, ClientPID}, State) ->
	io:format("~p server received ~p's msg: ~p~n", [ServerPID, ClientPID, Message]),

	%% Elicit a broadcast message	
	gen_server:cast(?MODULE,{broadcast, ClientPID, Message}),
	{noreply, State};

%%Handle broadcast messages
handle_cast({broadcast, ClientPID, Message}, State) ->
	io:format("~nServer broadcasting ~p's message: ~p~n", [ClientPID, Message]),
	lists:foreach(fun(Client) -> 
		io:format("    to ~p~n", [Client]),
		Client ! {message, Message} end, State#state.clients),
	{noreply, State};

%% Handle invalid messages
handle_cast(_Msg, State) ->
	io:format("Server received invalid msg"),
	{noreply, State}.

%% Handle info messages
handle_info(_Info, State) ->
	{noreply, State}.

% Terminate connections
terminate(_Reason, _State) -> ok.

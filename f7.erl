%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(f7).
-export([start_sim/0]).
-export([start_router/0,init_router/0,router_loop/1]).
-export([client_allocate/0,client_deallocate/1,client_stop/0]).
-export([init_server/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Testing/simulation functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_sim() ->
    %observer:start(), % Fed up of always entering this, so do it here
    %timer:sleep(2000),
	start_router(),
    timer:sleep(1000),
    client_allocate(),
	client_allocate(),
	client_allocate(),
	client_allocate(),
	client_allocate(),
	client_allocate(),
	client_allocate(),
	client_allocate(),
	client_deallocate(21),
	client_deallocate(11),
	client_allocate().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Router functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_router() ->
    register(router, spawn(f7, init_router, [])).

init_router() ->
	io:format("[~w] Router starting servers... ~n",[self()]),
    register(server1, spawn_link(f7, init_server, [[10,11,12,13,14,15]])),
	register(server2, spawn_link(f7, init_server, [[20,21,22,23,24,25]])),
    router_loop([server1,server2]).

router_loop([Server|ServerList]) ->
    receive
        {request, Pid, stop} ->
            io:format("[~w] Router stopping servers at request of client (~w)... ~n",[self(),Pid]),
            router_stopservers(Server ++ ServerList),
		    io:format("[~w] Router stopping~n", [self()]);
		{request, Pid , {deallocate, Freq}} ->
			case server_for_freq(Freq) of
				unknown_frequency ->
					unknown_frequency;
				FreqServer ->
					router_forwardmessage(FreqServer, {request, Pid , {deallocate, Freq}}),
					router_loop([Server] ++ ServerList)
			end;
        Msg ->
            router_forwardmessage(Server, Msg),
			router_loop(ServerList ++ [Server])
    end.

server_for_freq(Freq) when Freq >= 10, Freq =< 15 -> server1;
server_for_freq(Freq) when Freq >= 20, Freq =< 25 -> server2;
server_for_freq(_) -> unknown_frequency.

router_forwardmessage(Server, Msg) ->
	io:format("[~w] Router forwarding message to server ~w~n",[self(), Server]),
	Server ! Msg.

router_stopservers([]) -> {reply, stopped};
router_stopservers([Server|ServerList]) ->
	io:format("[~w] Router stopping server ~w~n",[self(), Server]),
	Server ! {request, self(), stop},
	router_stopservers(ServerList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_server(AvailableFrequences) ->
    Frequencies = {AvailableFrequences, []},
    server_loop(Frequencies).

%% The Main Loop
server_loop(Frequencies) ->
    receive
		{request, Pid, allocate} ->
			io:format("[~w] Server allocating frequency to ~w~n",[self(), Pid]),
			{NewFrequencies, Reply} = server_allocate(Frequencies, Pid),
			Pid ! {reply, Reply},
			server_loop(NewFrequencies);
		{request, Pid , {deallocate, Freq}} ->
			io:format("[~w] Server deallocating frequency ~w from client ~w~n",[self(),Freq, Pid]),
			NewFrequencies = server_deallocate(Frequencies, Freq),
			Pid ! {reply, ok},
			server_loop(NewFrequencies);
		{request, Pid, stop} ->
			io:format("[~w] Server stopping~n",[self()]),
			Pid ! {reply, stopped}
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

server_allocate({[], Allocated}, _Pid) ->
  	{{[], Allocated}, {error, no_frequency}};
server_allocate({[Freq|Free], Allocated}, Pid) ->
  	{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

server_deallocate({Free, Allocated}, Freq) ->
	NewAllocated=lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free],  NewAllocated}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Functional interface
client_allocate() -> 
    router ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

client_deallocate(Freq) -> 
    router ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

client_stop() -> 
    router ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.

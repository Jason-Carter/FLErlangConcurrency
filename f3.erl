%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(f3).
-export([start_simulation/0,start_server/0,start_client/2,allocate/0,deallocate/1,stop/0]).
-export([init_server/0,init_client/1]).

%% These are the start functions used to create and
%% initialize the server.

start_simulation() ->
  start_server(),
  start_client(client1, [allocate, wait, deallocate,wait]),
  %start_client(client1, [allocate,allocate,wait,allocate,wait,deallocate,wait,deallocate,wait,deallocate]),
  start_client(client2, [allocate, wait, deallocate,wait]).

start_client(ClientName, Commands) ->
    register(ClientName, spawn(f3, init_client, [Commands])).

init_client(Commands) ->
  process_flag(trap_exit, true),
  client_loop(Commands).

client_loop(Commands) -> client_loop(Commands, [], []).

client_loop([], Commands, Frequencies) -> client_loop(lists:reverse(Commands), [], Frequencies); % Start the loop agin, and loop forever (until exit signal)
client_loop([Command |CommandsRemaining], CommandsProcessed, Frequencies) ->
  receive
    {'EXIT', _Pid, normal} -> io:format("[~w] Exited", [self()]);
    {'EXIT', _Pid, Reason} -> io:format("[~w] Server killed: ~w~n",[self(), Reason]);
    stop -> io:format("[~w] Stopped~n", [self()]);
    UnknownMsg -> io:format("[~w] Dropping unknown message: ~w~n",[self(), UnknownMsg])
  after 0 ->
    case Command of
      allocate ->
        {ok, Freq} = allocate(),
        io:format("[~w] Allocated frequency: ~w~n",[self(),Freq]),
        client_loop(CommandsRemaining, [Command | CommandsProcessed], [Freq | Frequencies]);
      deallocate ->
        case Frequencies of
          [] ->
            io:format("[~w] No frequencies to deallocate!~n", [self()]),
            client_loop(CommandsRemaining, [Command | CommandsProcessed], Frequencies);
          [Freq | AllocatedFrequencies] ->
            io:format("[~w] Deallocating frequency: ~w~n",[self(), Freq]),
            deallocate(Freq),
            client_loop(CommandsRemaining, [Command | CommandsProcessed], AllocatedFrequencies)
        end;
      wait ->
        io:format("[~w] Sleeping~n", [self()]),
        timer:sleep(5000),
        client_loop(CommandsRemaining, [Command | CommandsProcessed], Frequencies)
    end
  end.


start_server() ->
    register(f3,
	     spawn(f3, init_server, [])).

init_server() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  server_loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

server_loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      server_loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      server_loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid), 
      server_loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
    f3 ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    f3 ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    f3 ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.


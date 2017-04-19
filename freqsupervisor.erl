%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(freqsupervisor).
-export([start_simulation/0]).
-export([init_supervisor/0,start_supervisor/0]).
-export([init_client/2,start_client/3]).
-export([init_server/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Testing/simulation functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% These are the start functions used to create and
%% initialize the server.

start_simulation() ->
    start_supervisor(),
    timer:sleep(2000),
    start_client(client1, frequency_server1, [allocate, wait, deallocate,wait]),
    start_client(client2, frequency_server1, [allocate, wait, deallocate,wait]),
    start_client(client3, frequency_server2, [allocate, wait, deallocate,wait]),
    start_client(client4, frequency_server2, [allocate, wait, deallocate,wait]).
%   start_server([10,11,12,13,14,15]),
%   start_client(client1, [allocate, wait, deallocate,wait]),
%   %start_client(client1, [allocate,allocate,wait,allocate,wait,deallocate,wait,deallocate,wait,deallocate]),
%   start_client(client2, [allocate, wait, deallocate,wait]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Supervisor functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_supervisor() ->
    register(frequency_supervisor, spawn(freqsupervisor, init_supervisor, [])).

init_supervisor() ->
    process_flag(trap_exit, true),
    register(frequency_server1, spawn_link(freqsupervisor, init_server, [[10,11,12,13,14,15]])),
    register(frequency_server2, spawn_link(freqsupervisor, init_server, [[20,21,22,23,24,25]])),
    supervisor_loop().

supervisor_loop() ->
    % To do...
    receive
        Msg ->
            io:format("[~w] Supervisor message: ~w~n", [self(), Msg])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start_server(FrequencyList) ->
%     register(freqserver,
% 	     spawn(freqsupervisor, init_server, [FrequencyList])).

init_server(FrequencyList) ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {FrequencyList, []},
  server_loop(Frequencies).

%% The Main Loop
server_loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = server_allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      server_loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = server_deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      server_loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, Reason} ->                   %%% CLAUSE ADDED
      _NewFrequencies = exited(Frequencies, Pid),
      io:format("[~w] Killed: ~w~n",[self(), Reason])
      %server_loop(NewFrequencies)
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

server_allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
server_allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

server_deallocate({Free, Allocated}, Freq) ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_client(ClientName, ServerName, Commands) ->
    register(ClientName, spawn(freqsupervisor, init_client, [ServerName, Commands])).

init_client(ServerName, Commands) ->
  process_flag(trap_exit, true),
  client_loop(ServerName, Commands).

client_loop(ServerName, Commands) -> client_loop(ServerName, Commands, [], []).

client_loop(ServerName, [], Commands, Frequencies) ->
  % Start the loop agin, and loop forever (until exit signal)
  client_loop(ServerName, lists:reverse(Commands), [], Frequencies);

client_loop(ServerName, [Command |CommandsRemaining], CommandsProcessed, Frequencies) ->
  receive
    {'EXIT', _Pid, normal} -> io:format("[~w] Exited", [self()]);
    {'EXIT', _Pid, Reason} -> io:format("[~w] Killed: ~w~n",[self(), Reason]);
    stop -> io:format("[~w] Stopped~n", [self()]);
    UnknownMsg -> io:format("[~w] Dropping unknown message: ~w~n",[self(), UnknownMsg])
  after 0 ->
    case Command of
      allocate ->
        {ok, Freq} = client_allocate(ServerName),
        io:format("[~w] Allocated frequency: ~w~n",[self(),Freq]),
        client_loop(ServerName, CommandsRemaining, [Command | CommandsProcessed], [Freq | Frequencies]);
      deallocate ->
        case Frequencies of
          [] ->
            io:format("[~w] No frequencies to deallocate!~n", [self()]),
            client_loop(ServerName, CommandsRemaining, [Command | CommandsProcessed], Frequencies);
          [Freq | AllocatedFrequencies] ->
            io:format("[~w] Deallocating frequency: ~w~n",[self(), Freq]),
            client_deallocate(ServerName, Freq),
            client_loop(ServerName, CommandsRemaining, [Command | CommandsProcessed], AllocatedFrequencies)
        end;
      wait ->
        io:format("[~w] Sleeping~n", [self()]),
        timer:sleep(5000),
        client_loop(ServerName, CommandsRemaining, [Command | CommandsProcessed], Frequencies)
    end
  end.

%% Functional interfaces
client_allocate(ServerName) -> 
    ServerName ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

client_deallocate(ServerName, Freq) -> 
    ServerName ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

% Don't want the client stopping the server
%
% stop() -> 
%     freqserver ! {request, self(), stop},
%     receive 
% 	    {reply, Reply} -> Reply
%     end.

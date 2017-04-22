%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(f5).
-export([start_sim/0]).
-export([init_supervisor/0,start_supervisor/0]).
-export([init_client/2,start_client/3]).
-export([init_server/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Testing/simulation functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_sim() ->
    observer:start(), % Fed up of always entering this, so do it here
    timer:sleep(2000),
    start_supervisor(),
    timer:sleep(2000),
    start_client(client1, server1, [allocate, wait, deallocate, wait]),
    start_client(client2, server1, [allocate, wait, deallocate, deallocate, wait]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Supervisor functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_supervisor() ->
    register(supervisor, spawn(f5, init_supervisor, [])).

init_supervisor() ->
    process_flag(trap_exit, true),
    register(server1, spawn_link(f5, init_server, [[10,11,12,13,14,15]])),
    supervisor_loop().

supervisor_loop() ->
    receive
        {'EXIT', _Pid, Reason} ->
            io:format("[~w] Supervisor detected server killed (reason: ~w), attempting restart...~n",[self(), Reason]),
            register(server1, spawn_link(f5, init_server, [[10,11,12,13,14,15]])),
            supervisor_loop();
        stop ->
            io:format("[~w] Supervisor stopping~n", [self()]);
        UnknownMsg ->
            io:format("[~w] Supervisor dropping unknown message: ~w~n",[self(), UnknownMsg]),
            supervisor_loop()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% should only be started by the supervisor
init_server(AvailableFrequences) ->
    process_flag(trap_exit, true),
    Frequencies = {AvailableFrequences, []},
    server_loop(Frequencies).

server_loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = server_allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            server_loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            try server_deallocate(Frequencies, Freq) of
                {FreeFrequencies, AllocatedFrequencies} ->
                    Pid ! {reply, ok},
                    server_loop({FreeFrequencies, AllocatedFrequencies})
            catch
            % Not sure what to do, so log and carry on...
                throw:frequency_not_allocated -> 
                    io:format("[~w] Server caught error attempting to deallocate frequency ~w ~n",[self(), Freq]),
                    Pid ! stop, % Naughty client, stop it...
                    server_loop(Frequencies)
            end;
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', Pid, Reason} ->
            io:format("[~w] Server detected client ~w died (reason: ~w), deallocating frequencies~n",[self(), Pid, Reason]),
            NewFrequencies = exited(Frequencies, Pid),
            server_loop(NewFrequencies);
        UnknownMsg ->
            io:format("[~w] Server encountered unknown message: ~w~n",[self(), UnknownMsg]),
            throw(unknown_message)
    end.

%% The Internal Help Functions used to allocate and deallocate frequencies.

server_allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
server_allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

server_deallocate({Free, Allocated}, Freq) ->
    case lists:keysearch(Freq,1,Allocated) of
       {value,{Freq,Pid}} ->
            unlink(Pid),
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {[Freq|Free], NewAllocated};
        false ->
            throw(frequency_not_allocated)
    end.

exited({Free, Allocated}, Pid) ->
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
    register(ClientName, spawn(f5, init_client, [ServerName, Commands])).

init_client(ServerName, Commands) ->
    process_flag(trap_exit, true),
    client_loop(ServerName, Commands).

client_loop(ServerName, Commands) -> client_loop(ServerName, Commands, [], []).

client_loop(ServerName, [], Commands, Frequencies) ->
    % Start the loop agin, and loop forever (until exit signal)
    client_loop(ServerName, lists:reverse(Commands), [], Frequencies);

client_loop(ServerName, [Command |CommandsRemaining], CommandsProcessed, Frequencies) ->
    receive
        {'EXIT', Pid, normal} -> io:format("[~w] Client exited normally by ~w~n", [self(), Pid]);
        {'EXIT', Pid, Reason} -> io:format("[~w] Client killed by ~w, reason: ~w~n",[self(), Pid, Reason]);
        stop -> io:format("[~w] Client stopped~n", [self()]);
        UnknownMsg -> io:format("[~w] Client dropping unknown message: ~w~n",[self(), UnknownMsg])
    after 0 ->
        case Command of
            allocate ->
                {ok, Freq} = client_allocate(ServerName),
                io:format("[~w] Allocated frequency: ~w~n",[self(),Freq]),
                client_loop(ServerName, CommandsRemaining, [Command | CommandsProcessed], [Freq | Frequencies]);
            deallocate ->
                case Frequencies of
                    [] ->
                        %io:format("[~w] No frequencies to deallocate!~n", [self()]),
                        io:format("[~w] Testing frequency deallocation when nothing allocated!~n", [self()]),
                        client_deallocate(ServerName, 21),
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
	      {reply, Reply} -> Reply;
          stop -> io:format("[~w] Wasn't me, I didn't do it!~n", [self()])
    end.

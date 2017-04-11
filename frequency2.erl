%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,stop/0,clear/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

% To test from the shell (before server timouts added):
%
%  frequency2:start().
%  frequency2:allocate().
%  frequency2:allocate().
%  frequency2:deallocate(11).
%  frequency2:clear().
%  frequency2:stop().


start() ->
    register(frequency,
	     spawn(frequency2, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop - Server side

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      timer:sleep(5000),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      timer:sleep(5000),
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface - client side

allocate() -> 
    % Clear the message queue prior to calling the server
    clear(), 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
      after 3000 ->
        time_out
    end.

deallocate(Freq) -> 
    % Clear the message queue prior to calling the server
    clear(), 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
      after 3000 ->
        time_out
    end.

stop() ->
    clear(), % Clears the client's message queue prior to stopping 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.

clear() ->
  receive
      _Msg -> clear()
  after 0 ->
      ok
  end.



%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.


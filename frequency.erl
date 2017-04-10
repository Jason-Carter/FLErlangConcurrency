%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

% TO use/test from the shell:
%
%  register(frequencies, spawn(frequency, init, [])). 
%  frequencies ! {request, self(), allocate}.
%  receive {reply, Freq} -> Freq end.
%  frequencies ! {request, self(), {deallocate, Freq}}.
%  receive {reply, ok} -> worked end.
%  frequencies ! {request, self(), stop}.
%  receive {reply, stopped} -> stopped end.
 

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    false -> {{[Freq|Free], Allocated}, {error, pid_already_allocated}};
    _ -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

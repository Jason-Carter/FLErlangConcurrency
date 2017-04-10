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
%  receive {reply, Msg} -> Msg end.
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
      {NewFrequencies, Reply} = deallocate(Frequencies, Pid, Freq),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keymember(Pid, 2, Allocated) of
    true  -> {{[Freq|Free], Allocated}, {error, pid_already_assigned}};
    false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Pid, Freq) ->
  case pid_assigned_freq({Freq, Pid}, Allocated) of
    false ->
      {{Free, Allocated}, {error, pid_not_assigned_freq}};
    true ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free],  NewAllocated}, {ok, Freq}}
  end.


pid_assigned_freq({_Freq, _Pid}, []) -> false;
pid_assigned_freq({_Freq, _Pid}, [{_Freq, _Pid} | _Allocated]) -> true;
pid_assigned_freq({Freq, Pid}, [{_, _} | Allocated]) -> pid_assigned_freq({Freq, Pid}, Allocated).

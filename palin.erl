-module(palin).
-export([palin/1,nopunct/1,palindrome/1]).
-export([server/1]).


% palindrome server
%
% To Call:
%
%   IsPalin = spawn(palin, server, [self()]).
%   IsPalin ! {check, "Pop,"}.
%
server(Pid) ->
    receive
        stop ->
            Pid ! io:format("stopped~n");
        {check, Msg} ->
            case palindrome(Msg) of
                true  -> Pid ! io:format("{result, ~s is a palindrome}~n", [Msg]);
                false -> Pid ! io:format("{result, ~s is NOT a palindrome}~n", [Msg])
            end,
            server(Pid)
    end.


% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

 
	



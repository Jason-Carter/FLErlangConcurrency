-module(mailbox).
-export([receiver/0, receiver2/0, receiver_order/0]).

% receiver mailbox test
%
% Pid = spawn(mailbox, receiver, []).
%   or
% Pid = spawn(mailbox, receiver2, []).
%
% Test with:
% Pid ! "hello1", Pid ! "hello2", Pid ! "hello3", Pid ! "hello4", Pid ! "hello5".
% Pid ! stop.

receiver() ->
    % Matches the message in a case statement after receiving in a variable
    receive
        Msg ->
            case Msg of
                stop -> io:format("stopping:~w~n",[{ok,42}]);
                _    -> io:format("message: ~s ~w~n",[Msg, {ok,42}]),
                        timer:sleep(5000),
                        receiver()
            end
    end.

receiver2() ->
    % Matches the message at the top level
    receive
        stop -> 
            io:format("stopping:~w~n",[{ok,42}]);
        Msg -> 
            io:format("message: ~s ~w~n",[Msg, {ok,42}]),
            timer:sleep(5000),
            receiver2()
    end.

% Pid = spawn(mailbox, receiver_order, []).
%
% Test with:
% Pid ! {second, "Hello2"}, Pid ! {first, "Hello1"}.
% Pid ! {first, "Hello1"}, Pid ! {second, "Hello2"}.

receiver_order() ->
    % Matches the message at the top level
    receive
        stop -> 
            io:format("stopping:~w~n",[{ok,42}]);
        {second, SecondString} -> 
            receive
                {first, FirstString} -> io:format("message: ~s ~w~n",[FirstString, {ok,42}])
            end,
            io:format("message: ~s ~w~n",[SecondString, {ok,42}]),
            timer:sleep(5000),
            receiver_order()
    end.

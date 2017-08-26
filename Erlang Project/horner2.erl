-module(horner2).
-export([server/1, client/2,  createProcess/2,loop/2, calcProcess/1]).

%Given Coeffs list [C[N], C[N-1], ..., C[1], C[0]], set up a server
%to evaluate the polynomial
%
%   C[N]*X^(N) + C[N-1]]*X^(N-1) + ... + C[1]*X^(1) + C[0]
%
%at some (yet unspecified) X.
%
%The server must consist of multiple Erlang processes with one Erlang 
%process per polynomial coefficient (there may be additional processes too).
%The processes for the coefficients should store their respective
%coefficients and communicate among themselves to evaluate the
%polynomial at a specified X using Horner's rule.
%
%The return value of this function should be the PID of the server.
%
%The details of the messages exchanged between the processes for
%evaluating the polynomial can be chosen by the implementation.
%However, the server must respond to a 'stop' message by stopping
%all the processes as well as itself.
%
%When a process for a coefficient is assisting in evaluating the
%polynomial, it must log to standard error it's stored coeffient
%and the value accumulated so far in the Horner evaluation.
%When a process for a coefficient is being stopped, it must
%log to standard error it's stored coefficient and the 'stop'
%message.  
server(Coeffs) ->
    Pids =createProcess(Coeffs,[]),
    Pid  = spawn(horner2,loop,[Pids,Pids]),
    Pid.

% Below function creates all the required processes
createProcess([],Pids) ->
    Pids;
createProcess([H|T],Pids) ->
    Pid = spawn(horner2,calcProcess,[H]),
    createProcess(T,Pids ++ [Pid]).

% Sending message to client will call below function
loop([],Pids) ->
    receive 
        {ClientPid,Result,_} ->
            ClientPid ! {Result},
            loop(Pids,Pids);
    stop ->
            true
    end;
loop([H|T],Pids) ->
    receive
        {ClientPid,X} ->
            H ! {self(),X,0,ClientPid},
            loop(T,Pids);
        {ClientPid,Result,X} ->
            H ! {self(),X,Result,ClientPid},
            loop(T,Pids);
        stop ->
            H ! {self(),stop},
            loop(T,Pids)
    end.

% Below callback function is the actual calculation function.
% This function calculates the new accumlation and returns it back.
calcProcess(CurrentCoeff) ->
    receive
        {From,X,Acc,ClientPid} ->
            Result = (X*Acc) + CurrentCoeff,
            do_log(CurrentCoeff,Result),
            From ! {ClientPid,Result,X},
            calcProcess(CurrentCoeff);
        {From,stop} ->
            do_log(CurrentCoeff,stop),
            From ! stop
    end.

%Return the value at X of the polynomial stored within the polynomial server
%specified by PID.
client(Pid, X) ->
  Pid ! {self(),X},
  receive
      {Result} -> 
          Result
  end.

do_log(Coeff, Msg) ->
    io:format(standard_error, "coeff = ~w; ~w~n", [Coeff, Msg]).

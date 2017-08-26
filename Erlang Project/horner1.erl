-module(horner1).
-export([poly_eval/1]).

%Given Coeffs list [C[N], C[N-1], ..., C[1], C[0]], specifying 
%the polynomial
%
%   C[N]*X^(N) + C[N-1]]*X^(N-1) + ... + C[1]*X^(1) + C[0]
%
%return a function of one variable X which when called returns the
%value of the polynomial at X.
poly_eval(Coeffs) ->
    fun(X) ->                           % Create a anonymous function as part of closure
          poly_eval_aux(Coeffs,X,0)     % Calls aux function with a accumlator
    end.
  
poly_eval_aux([H|T],X,Acc)->
    Acc1 = (X*Acc)+H,                   % Calcualte next accumlator
    poly_eval_aux(T,X,Acc1);
poly_eval_aux([],_,Acc)->               % In case coefficientsis empty the result is computed.
    Acc.

  

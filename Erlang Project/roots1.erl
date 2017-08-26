-module(roots1).
-export([roots/3]).
%Return a 2-element tuple containing the roots of the quadratic
%equation A*X^2 + B*X + C.  You may assume that the discriminant >= 0.
%The first element of the return'd tuple should use the positive
%square-root of the discriminant and the second element of the
%return'd tuple should use the negative square-root of the discriminant.
roots(A, B, C)->
    Discriminant = math:sqrt((B*B)-(4*A*C)), % Calculate Discrminant
    Roots1 = ((-B + Discriminant) / (2*A)),  % Calc postive root
    Roots2 = ((-B - Discriminant) / (2*A)),  % Calc negative root
    {Roots1,Roots2}.                         % Return tuple of positive and negative root

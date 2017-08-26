-module(roots2).
-export([server/0, client/2,loop/0]).

-import(roots1, [roots/3]).

%Return the PID of a quadratic-solver Erlang process which when given
%a message containing the coefficients A, B, C responds with a message
%giving the roots of the quadratic equation A*X^2 + B*X + C.  It is
%assumed that the return'd roots are not complex.
server() ->
    Pid2 = spawn(roots2,loop,[]), % Create a new process which would be called by client
    Pid2. % Return PID of the newly created process.

loop() ->
    receive                                             % Receive message from client
        {RequestFrom, {A,B,C}} ->
            {Root1,Root2} = roots1:roots(A,B,C),        % Calculate root
            RequestFrom ! {Root1,Root2},                % send calcualted root back to client
            loop(); 
        stop ->                                         % if received stop message the stop the server
            true
        end.


%Given the PID of a quadratic-solver and Coeffs a 3-tuple {A, B, C}, 
%this function uses that quadratic-solver to return the roots of the
%quadratic equation A*X^2 + B*X + C.  The roots are return'd as a
%2-tuple, with the first element of the return'd tuple using the
%positive square-root of the discriminant and the second element of
%the return'd tuple using the negative square-root of the
%discriminant.  It is assumed that the return'd roots are not complex.
client(PID, Coeffs) ->
    PID ! {self(),Coeffs},                  % Send request to server to calculate roots of quadratic equation
    receive
        {Roots1,Roots2} ->                  % Receive roots and print them
            {Roots1,Roots2}
    end.


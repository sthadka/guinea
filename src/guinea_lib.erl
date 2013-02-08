-module(guinea_lib).
-export([gauss/2, gauss_int/2]).

gauss_int(Mu, Sigma) ->
    erlang:trunc(gauss(Mu, Sigma)).

% We ignore the second value for now
gauss(Mu, Sigma) ->
    Mu + normal() * Sigma.


% When x and y are two variables from [0, 1), uniformly
% distributed, then
%
%    cos(2*pi*x)*sqrt(-2*log(1-y))
%    sin(2*pi*x)*sqrt(-2*log(1-y))
%
% are two *independent* variables with normal distribution
% (mu = 0, sigma = 1).
normal() ->
    math:cos(2 * math:pi() * random:uniform()) *
    math:sqrt((-2.0 * math:log(1 - random:uniform()))).

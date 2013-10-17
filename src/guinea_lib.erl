-module(guinea_lib).
-export([gauss/2, gauss_int/2,
         to_integer/1, to_binary/1, to_list/1]).

-include_lib("guinea.hrl").

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

-spec to_integer(any()) -> integer().
to_integer(I) when is_integer(I) ->
        I;
to_integer(B) when is_binary(B) ->
        to_integer(?b2l(B));
to_integer(L) when is_list(L) ->
        case catch ?l2i(L) of
                    {'EXIT', _} -> throw({error, {not_a_valid_integer, L}});
                    Int         -> Int
                    end.

-spec to_binary(any()) -> binary().
to_binary(B) when is_binary(B) ->
        B;
to_binary(L) when is_list(L) ->
        ?l2b(L);
to_binary(A) when is_atom(A) ->
        ?a2b(A);
to_binary(I) when is_integer(I) ->
        ?i2b(I).

-spec to_list(any()) -> list().
to_list(B) when is_binary(B) ->
        ?b2l(B);
to_list(A) when is_atom(A) ->
        ?a2l(A);
to_list(I) when is_integer(I) ->
        ?i2l(I);
to_list(L) when is_list(L) ->
        L.

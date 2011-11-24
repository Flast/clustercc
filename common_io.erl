-module(common_io).
-export([prefixed/1, prefixed/2]).

prefixed(Format) -> prefixed(Format, []).
prefixed(Format, Data) when length(Format) /= 0 ->
  F = lists:concat(["clusterccd[~w]: ", Format, "~n"]),
  D = [self() | Data],
  io:format(F, D).

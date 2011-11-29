-module(manage).
-export([main/1]).

main([Target]) when is_atom(Target) ->
  true = ccc_manage:connect(Target).

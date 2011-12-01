% fake process
-module(distccd).
-export([main/1]).

main([User]) when is_atom(User) ->
  Args = init:get_plain_arguments(),
  try true = lists:member("--inetd", Args)
  catch
    error:badmatch ->
      % distccd requires --inetd flag
      exit(normal)
  end,

  true = ccc_manage:connect(daemon@localhost),
  true = register(distccd, self()),
  process_flag(trap_exit, true),

  case Node = node_manager:get() of
    undefined -> exit(enoent);
    _         -> undefined
  end,
  {ok, Socket} = gen_tcp:connect(Node, 3632, []),

  TX = spawn_link(
    fun() -> redirecting(
          fun() -> file:read(standard_io, 1) end,
          fun(Data) -> gen_tcp:send(Socket, Data) end)
    end),
  RX = spawn_link(
    fun() -> redirecting(
          fun() -> gen_tcp:recv(Socket, 0) end,
          fun(Data) -> io:format(standard_io, "~w", [Data]) end)
    end),

  erlang:yield(),
  loop(TX, RX),
  gen_tcp:close(Socket).

loop(undefined, undefined) -> undefined;
loop(TX, RX) ->
  receive
    {'EXIT', TX, Reason} ->
      io:format("TX: ~w~n", [Reason]),
      loop(undefined, RX);
    {'EXIT', RX, Reason} ->
      io:format("RX: ~w~n", [Reason]),
      loop(TX, undefined)
  end.

redirecting(Receiver, Sender) ->
  case Receiver() of
    {ok, Data}      -> ok = Sender(Data);
    eof             -> exit(normal);
    {error, Reason} ->
      io:format("~s~n", [file:format_error(Reason)]),
      exit(Reason)
  end,
  redirecting(Receiver, Sender).

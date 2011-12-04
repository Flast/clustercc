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
  process_flag(trap_exit, true),

  case Node = node_manager:get() of
    undefined -> exit(enoent);
    _         -> undefined
  end,
  {ok, Socket} = gen_tcp:connect(Node, 3632, [{active, false}]),

  TX = spawn_link(
    fun() -> redirecting(
          fun()     -> io:get_chars([], 1) end,
          %fun()     -> file:read(standard_io, 1) end,
          fun(Data) -> gen_tcp:send(Socket, Data) end)
    end),
  RX = spawn_link(
    fun() -> redirecting(
          fun()     -> gen_tcp:recv(Socket, 0) end,
          fun(Data) -> io:format(standard_io, "~w", [Data]) end)
    end),

  erlang:yield(),
  loop(TX, RX),
  gen_tcp:close(Socket).

loop(undefined, undefined) -> undefined;
loop(TX, RX) ->
  receive
    {'EXIT', TX, {Reason, RS}} ->
      io:format("TX: ~w(~s)~n", [Reason, RS]),
      loop(undefined, RX);
    {'EXIT', RX, {Reason, RS}} ->
      io:format("RX: ~w(~s)~n", [Reason, RS]),
      loop(TX, undefined);
    {'EXIT', Pid, Why} ->
      io:format("unexpected process termination (~w): ~w~n", [Pid, Why]),
      loop(TX, RX)
  end.

redirecting(Receiver, Sender) ->
  case Receiver() of
    eof             -> exit({normal, "Success"});
    {error, Reason} -> exit({Reason, file:format_error(Reason)});
    {ok, Data}      -> ok = Sender(Data);
    Data            -> ok = Sender(Data)
  end,
  redirecting(Receiver, Sender).

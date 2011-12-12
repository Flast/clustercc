% fake process
-module(distccd).
-export([main/1]).
-import(string, [to_integer/1, substr/2, substr/3]).

io_write(Data) when is_list(Data) ->
  io_write(list_to_binary(Data));
io_write(Data) when is_binary(Data) ->
  case file:write(group_leader(), Data) of
    ok              -> ok;
    {error, Reason} -> exit(file:format_error(Reason))
  end.

io_read(Number) ->
  case file:read(group_leader(), Number) of
    {ok, Data} when is_list(Data)   -> list_to_binary(Data);
    {ok, Data} when is_binary(Data) -> Data;
    eof                             -> exit(normal);
    {error, Reason}                 -> exit(file:format_error(Reason))
  end.

format_log(Format) -> format_log(Format, []).
format_log(Format, Datas) when is_list(Datas) ->
  F = lists:concat([Format, "~n"]),
  io:format(standard_error, F, Datas).

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

  TX = spawn_link(fun() -> tx(Socket) end),
  RX = spawn_link(fun() -> rx(Socket) end),

  erlang:yield(),
  loop(TX, RX),
  gen_tcp:close(Socket),
  format_log("terminated").

loop(undefined, undefined) -> undefined;
loop(TX, RX) ->
  receive
    {'EXIT', TX, normal} -> loop(undefined, RX);
    {'EXIT', TX, Why} ->
      format_log("TX: ~w", [Why]),
      loop(undefined, RX);

    {'EXIT', RX, normal} -> loop(TX, undefined);
    {'EXIT', RX, Why} ->
      format_log("RX: ~w", [Why]),
      loop(TX, undefined);

    {'EXIT', Pid, Why} ->
      format_log("unexpected process termination (~w): ~w", [Pid, Why]),
      loop(TX, RX)
  end.

rx(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {error, closed} -> exit(normal);
    {error, Reason} -> exit(file:format_error(Reason));
    {ok, Data}      -> ok = io_write(Data)
  end,
  rx(Socket).

read_prefix() ->
  <<Req:4/binary, BN/binary>> = BP = io_read(12),
  N = list_to_integer(binary_to_list(BN), 16),
  io:format(standard_error, "Req: ~s, Len: ~w, prefix: ~s -> ", [Req, N, BP]),
  {BP, N}.

transfar_request(3, Sender, R = {Orig, Len}) ->
  ok = case Orig of
    <<"NFIL", _/binary>> ->
      Sender(Orig),
      format_log("done");
    <<Req:4/binary, _/binary>>
    when Req == "CDIR"; Req == "NAME";
         Req == "FILE"; Req == "LINK" ->
      Data = io_read(Len),
      Sender(list_to_binary([Orig, Data])),
      format_log("done");
    _ -> transfar_request(2, Sender, R)
  end;

transfar_request(2, Sender, R) ->
  transfar_request(1, Sender, R);

transfar_request(1, Sender, {Orig, Len}) ->
  Data = case Orig of
    <<"ARGC", _/binary>> -> <<>>;
    <<Req:4/binary, _/binary>>
    when Req == "ARGV";
         Req == "DOTI" -> io_read(Len)
  end,
  ok = Sender(list_to_binary([Orig, Data])),
  format_log("done").

tx_loop(Version, Sender) ->
  Prefix = read_prefix(),
  transfar_request(Version, Sender, Prefix),
  tx_loop(Version, Sender).

tx(Socket) ->
  Sender = fun(Data) -> gen_tcp:send(Socket, Data) end,

  case read_prefix() of
    {S= <<"DIST", _/binary>>, V} when V == 1; V == 2; V == 3 ->
      ok = Sender(S),
      format_log("distcc protocol version: ~w", [V])
  end,

  tx_loop(V, Sender).

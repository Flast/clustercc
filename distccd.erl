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

  In = spawn_link(fun() -> buffer([]) end),
  spawn_link(fun() -> receiver(standard_io, In) end),

  %Out = spawn_link(fun() -> buffer([]) end),
  %spawn_link(fun() -> receiver(socket, Out) end),

  spawn_link(fun() -> redirecting(In, standard_io) end),

  erlang:yield(),
  receive
    {buffer, eof, Pid} ->
      io:format("~n~n~w encountered EOF: system will down~n", [Pid])
  end.

redirecting(Buffer, IoDevice) when is_pid(Buffer) ->
  Buffer ! {buffer, read, self()},
  receive
    {buffer, Data} -> io:format("~s", [lists:reverse(Data)])
  end,
  redirecting(Buffer, IoDevice).

buffer(Data) ->
  receive
    {buffer, append, Char} ->
      buffer([Char | Data]);
    {buffer, read, Pid} when length(Data) /= 0 ->
      Pid ! {buffer, Data},
      buffer([])
  end.

receiver(IoDevice, BPid) ->
  case file:read(IoDevice, 1) of
    {ok, [Term]} ->
      BPid ! {buffer, append, Term},
      receiver(IoDevice, BPid);
    eof -> distccd ! {buffer, eof, self()};
    {error, Reason} -> exit(Reason)
  end.

-module(worker).
-export([init/1, terminate/2, handle_ssh_msg/2, handle_msg/2]).

prefixed(Format) -> prefixed(Format, []).
prefixed(Format, Data) when length(Format) /= 0 ->
  F = lists:concat(["worker[~w]: ", Format, "~n"]),
  D = [self() | Data],
  io:format(F, D).

init(_) ->
  Pid = spawn(fun clustercc_main/0),
  clusterccd ! {node, self(), Pid},
  Node = dict:store(node, Pid, dict:new()),
  {ok, Node}.

terminate(Reason, State) ->
  Node = dict:fetch(node, State),
  Node ! {ssh_closed, Reason},
  prefixed("terminate ssh session with [~w]", [Reason]).

handle_msg({ssh_channel_up, ID, Ref}, State) ->
  Node = dict:fetch(node, State),
  Node ! {clustercc, Ref, ID},
  {ok, State};
% do nothing
handle_msg(_, State) -> {ok, State}.

handle_ssh_msg({ssh_cm, Ref, Msg}, State) ->
  case Msg of
    {closed, ID}   -> {stop, ID, State};
    {shell, ID, _} ->
      Send = fun(M) when is_list(M) ->
          BM = list_to_binary(M),
          ssh_connection:send(Ref, ID, BM)
      end,
      Send("You've successfully authenticated.\r\n"),
      Send("  This server does not provide shell access, and close this session.\r\n\r\n"),
      {stop, ID, State};

    {env, _, _, Var, Value} ->
      prefixed("env => ~s = ~s", [binary_to_list(Var), binary_to_list(Value)]),
      {ok, State};

    {exec, ID, true, Cmd} ->
      Node = dict:fetch(node, State),
      WorkCmd = case string:tokens(Cmd, " \t") of
        ["distccd", "--inetd"] -> exec;
        _ ->
          prefixed("warning: unexpected commands [~s]", [Cmd]),
          close
      end,
      Node ! {clustercc, WorkCmd},
      case WorkCmd of
        exec  -> {ok, State};
        close -> {stop, ID, State}
      end;
    {data, ID, _, Data} when is_binary(Data) ->
      Node = dict:fetch(node, State),
      Node ! {ssh, Ref, ID, Data},
      {ok, State};

    % don't care following msg
    {pty, _, _, _}                -> {ok, State};
    {window_chage, _, _, _, _, _} -> {ok, State};

    _ ->
      prefixed("unhandled ssh message: ~w~n", [Msg]),
      {ok, State}
  end.

%% clustercc

clustercc_main() ->
  {clustercc, Ref, ID} = receive V1 -> V1 end,
  {clustercc, exec}    = receive V2 -> V2 end,
  R  = (catch clustercc_main(Ref, ID, node_manager:get())),
  ok = ssh_connection:close(Ref, ID),
  case R of
    no_entry -> prefixed("no worker node");
    closed   -> undefined;

    {'EXIT', {Reason, _}} -> error(Reason);
    {'EXIT', Reason}      -> exit(Reason);
    T                     -> throw(T)
  end.

clustercc_main(_, _, undefined) -> no_entry;
clustercc_main(Ref, ID, Node)   ->
  {ok, Socket} = gen_tcp:connect(Node, 3632, []),

  TX = fun(Data) ->
      gen_tcp:send(Socket, Data)
  end,
  RX = fun
    (Data) when is_binary(Data) ->
      ssh_connection:send(Ref, ID, Data);
    (Data) when is_list(Data) ->
      ssh_connection:send(Ref, ID, list_to_binary(Data))
  end,

  loop(TX, RX),

  gen_tcp:close(Socket).

loop(TX, RX) ->
  receive
    {tcp, _, Data}    -> RX(Data), loop(TX, RX);
    {ssh, _, _, Data} -> TX(Data), loop(TX, RX);

    {tcp_closed, _} -> closed;
    {ssh_closed, _} -> closed;

    {tcp_error, _, _} -> closed
  end.

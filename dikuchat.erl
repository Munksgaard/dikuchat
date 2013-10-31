-module(dikuchat).

-export([start_server/1]).
%-compile(export_all).

reply(From, Msg) ->
    From ! {self(), Msg}.

who([]) -> [];
who([{Name, _}|XS]) -> [Name | who(XS)].

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

broadcast([], _, _) ->
    ok;
broadcast([{_, Pid} | Userlist], FromName, Msg) ->
    reply(Pid, {broadcast, FromName, Msg}),
    broadcast(Userlist, FromName, Msg).

nameexists(_, []) -> false;
nameexists(Name, [{Name, _} | _]) -> true;
nameexists(Name, [_ | Names]) -> nameexists(Name, Names).

pidlookup([], _) -> error;
pidlookup([{Name, Pid} | _], Pid) -> {ok, Name};
pidlookup([_ | Userlist], Pid) -> pidlookup(Userlist, Pid).

server_loop(Userlist) ->
    receive
        {From, who} ->
            reply(From, who(Userlist)),
            server_loop(Userlist);
        {From, {name, Name}} ->
            case nameexists(Name, Userlist) of
                true -> reply(From, exists),
                        server_loop(Userlist);
                false -> reply(From, ok),
                         server_loop([{Name, From} | Userlist])
            end;
        {From, {broadcast, Msg}} ->
            case pidlookup(Userlist, From) of
                {ok, SenderName} -> reply(From, ok),
                                    broadcast(Userlist, SenderName, Msg);
                _ -> reply(From, noname)
            end,
            server_loop(Userlist);
        {From, quit} ->
            case pidlookup(Userlist, From) of
                {ok, Name} -> server_loop(lists:delete({Name, From}, Userlist));
                _ -> server_loop(Userlist)
            end;
        Undefined ->
            io:format("Error in server_loop! ~p~n", [Undefined]),
            server_loop(Userlist)
    end.

start_server(Port) ->
    SPid = spawn_link(fun() -> server_loop([]) end),
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(SPid, Listen) end),
        timer:sleep(infinity)
    end),
    {ok, SPid, Pid}.

acceptor(Server, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(Server, ListenSocket) end),
    handle(Server, Socket).

prettifyWho([]) -> [];
prettifyWho([User, User2 | Userlist]) ->
    User ++ ", " ++ prettifyWho([User2 | Userlist]);
prettifyWho([User | _]) -> User ++ "\n".

strip(String) ->
    string:left(String, string:cspan(String, "\r\n")).

%% Echoing back whatever was obtained
handle(Server, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {Server, {broadcast, Name, Msg}} ->
            gen_tcp:send(Socket, "broadcast " ++ Name ++ " " ++ Msg ++ "\n"),
            handle(Server, Socket);
        {tcp, Socket, <<"quit", _/binary>>} ->
            Server ! {self(), quit},
            gen_tcp:close(Socket);
        {tcp, Socket, <<"who", _/binary>>} ->
            WhoList = rpc(Server, who),
            gen_tcp:send(Socket, "who " ++ prettifyWho(WhoList)),
            handle(Server, Socket);
        {tcp, Socket, <<"name ", Name/binary>>} ->
            SName = strip(binary:bin_to_list(Name)),
            case rpc(Server, {name, SName}) of
                ok -> gen_tcp:send(Socket, "name " ++ SName ++ "\n"),
                      handle(Server, Socket);
                exists -> gen_tcp:send(Socket, "exists\n"),
                          handle(Server, Socket)
            end;
        {tcp, Socket, <<"broadcast ", Msg/binary>>} ->
            case rpc(Server, {broadcast, strip(binary:bin_to_list(Msg))}) of
                ok -> handle(Server, Socket);
                _ -> gen_tcp:send(Socket, "noname\n"),
                     handle(Server, Socket)
            end;
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, "error " ++ binary:bin_to_list(Msg)),
            handle(Server, Socket)
    end.

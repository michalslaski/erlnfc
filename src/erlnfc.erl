-module(erlnfc).
-export([start/0]).

start() ->
    os:cmd("sudo killall readNFC"),
    Cmd = "sudo ./priv/readNFC",
    Port = erlang:open_port({spawn, Cmd}, [binary]),
    loop(Port).

loop(Port) ->
    receive
        {Port, {data, Data}} ->
            case parse(Data) of
                {nfctag, NfcTag} ->
                    io:format("NFC tag: ~p~n", [NfcTag]),
                    loop(Port);
                not_found ->
                    loop(Port)
            end;
        _ ->
            loop(Port)
    end.

parse(Data) ->
    parse(Data, []).

parse(<<"\n", _/binary>>, Acc) ->
    {nfctag, list_to_integer(lists:reverse(Acc))};
parse(<<"">>, _Acc) ->
    not_found;
parse(<<Digit, Bin/binary>>, Acc) ->
    parse(Bin, [Digit|Acc]).

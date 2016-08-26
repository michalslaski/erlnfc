%%%-------------------------------------------------------------------
%%% @author Michal Slaski
%%% @doc
%%% API module for the erlnfc library. Use add_handler/1 to add your
%%% event handlers.
%%% @end
%%% Created : 26 Aug 2016 by Michal Slaski
%%%-------------------------------------------------------------------
-module(erlnfc).
-export([add_handler/1]).
-export([start/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler.
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Module) ->
    gen_event:add_handler(erlnfc_event, Module, []).


%%%===================================================================
%%% Internal functions
%%%===================================================================

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

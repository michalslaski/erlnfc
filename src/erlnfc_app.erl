%%%-------------------------------------------------------------------
%%% @author Michal Slaski
%%% @doc
%%% Erlang library for ITEAD PN532 NFC module
%%% @end
%%% Created : 26 Aug 2016 by Michal Slaski
%%%-------------------------------------------------------------------
-module(erlnfc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case erlnfc_sup:start_link() of
        {ok, Pid} ->
            erlnfc:add_handler(erlnfc_event_handler),
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

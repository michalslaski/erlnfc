%%%-------------------------------------------------------------------
%%% @author Michal Slaski <michalslaski@macbook-2.local>
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2016 by Michal Slaski
%%%-------------------------------------------------------------------
-module(erlnfc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    ErlNFCevent = #{id => 'erlnfc_event',
                     start => {gen_event, start_link, [{local, erlnfc_event}]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => dynamic},

    ErlNFCreader = #{id => 'erlnfc_reader',
                     start => {erlnfc_reader, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [erlnfc_reader]},

    {ok, {SupFlags, [ErlNFCevent, ErlNFCreader]}}.

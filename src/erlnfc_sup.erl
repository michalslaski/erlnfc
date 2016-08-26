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
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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

    SupFlags = {one_for_one,
                1,
                5},

    ErlNFCevent = {'erlnfc_event',
                   {gen_event, start_link, [{local, erlnfc_event}]},
                   permanent,
                   5000,
                   worker,
                   dynamic},

    ErlNFCreader = {'erlnfc_reader',
                    {erlnfc_reader, start_link, []},
                    permanent,
                    5000,
                    worker,
                    [erlnfc_reader]},

    {ok, {SupFlags, [ErlNFCevent, ErlNFCreader]}}.

%% -*- mode: erlang; indent-tabs-mode: nil -*-
% Copyright (c) 2012-2015 Christopher Vance.
-module(uuid_app).
-author('Christopher Vance.').

-behaviour(application).
-export([start/2,stop/1]). % application required
%-export([start_phase/3,prep_stop/1,config_change/3]). % application optional
-export([start/0]). % application api

%%% application required callbacks

%% invoked by
%% application:start
%% (application_master:start_it_old,application_master:start_supervisor)
%%
%% @doc Standard application callback. Start the application's supervisor.
%%
-spec start(StartType :: normal |
                         {takeover, Node :: node()} |
                         {failover, Node :: node()},
            StartArgs :: term()) ->
                   {ok, pid()} |
                   {ok, pid(), State :: term()} |
                   {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    uuid_sup:start_link().

%% invoked by
%% application:stop
%% (application_master:loop_it)
%%
%% @doc Standard application callback. Stop the application.
%%
-spec stop(State :: term()) ->
                  term().
stop(_State) ->
    ok.

%%% application optional callbacks

%% (removed)

%%% application api

%%
%% @doc API to start application.
%%
-spec start() ->
                   ok |
                   {error, Reason :: term()}.
start() ->
    application:start(?MODULE).

%%% functions internal to uuid implementation

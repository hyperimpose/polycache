%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 17 Aug 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(polycache).

-behaviour(gen_server).


-include_lib("kernel/include/logger.hrl").


%% API
-export([start_link/0, new/1, get/2, set/3, set/4]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).


%% Macros
-define(NAME, ?MODULE).

-record(state, {cache=[]}).
-record(entry, {value, ttl}).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
          {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Create a new cache.
%%
%% @param Size The total number of entries the table can hold.
%% @returns A reference to the table.
%% @end
%%--------------------------------------------------------------------

new(Size) ->
    gen_server:call(?NAME, {new, Size}).


%%--------------------------------------------------------------------
%% @doc Get the value associated with the Key from a Cache.
%% @end
%%--------------------------------------------------------------------

get({Table, Size}, Key) ->
    case ets:lookup(Table, erlang:phash2(Key, Size)) of
        [{_Hash, Key, Hit}] -> get1(Hit);
        _Else               -> not_found
    end.

get1(#entry{value = Value, ttl = undefined}) ->
    {ok, Value};
get1(#entry{value = Value, ttl = Ttl})       ->
    Now = erlang:system_time(seconds),
    if
        Ttl > Now -> {ok, Value};
        true      -> not_found
    end.


%%--------------------------------------------------------------------
%% @doc Add or update a Cache entry.
%%
%% @param Cache The cache reference as returned by new/1
%% @param Key The key to use for retrieval of the cache entry
%% @param Value The value of the cache entry
%% @param Opts A map of options for the cache entry. Available options:
%%
%% ttl: ttl is a timestamp in the future that marks the expiration of
%%      the cache entry. It is compared to erlang:system_time(seconds).
%%      If it is not given the cache entry will never expire.
%% @end
%%--------------------------------------------------------------------

set(Cache, Key, Value) -> set(Cache, Key, Value, #{}).

set({Table, Size}, Key, Value, Opts) ->
    Ttl = maps:get(ttl, Opts, undefined),

    Entry = #entry{value = Value, ttl = Ttl},
    true = ets:insert(Table, {erlang:phash2(Key, Size), Key, Entry}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% --------------------------------------------------------------------

-spec init(term()) -> {ok, State :: #state{}}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages.
%% @end
%%--------------------------------------------------------------------

handle_call({new, Size}, _From, #state{cache = Cache} = State) ->
    Table = ets:new(minutia_cache, [set, public, {keypos, 1}]),
    S1 = State#state{cache = [{Table, Size} | Cache]},
    {reply, {ok, {Table, Size}}, S1};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages.
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
% @end
%%--------------------------------------------------------------------

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().

terminate(_Reason, _State) ->
    % TODO: maybe send a close message to the port
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------

-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().

format_status(_Opt, Status) ->
    Status.

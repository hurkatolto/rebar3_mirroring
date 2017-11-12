%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%% @doc
%%
%% Mirroring module for pkg resources.
%%
%% @end

-module(rebar3_pkg_mirroring).

-behaviour(rebar_resource).

-include("rebar3_mirroring.hrl").
-include_lib("rebar3/src/rebar.hrl").

-export([lock/2,
         download/3,
         needs_update/2,
         make_vsn/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec lock(Dir, Source) -> Res when
    Dir :: file:filename_all(),
    Source :: tuple(),
    Res :: rebar_resource:resource().
lock(Dir, Source) ->
    rebar_pkg_resource:lock(Dir, Source).

-spec download(Dir, Source, State) -> Res when
    Dir :: file:filename_all(),
    Source :: tuple(),
    State :: rebar_state:t(),
    Res :: {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
download(Dir, Source, State) ->
    io:format("~p ~p Source: '~p' ~n", [?MODULE, ?LINE, Source]),
    rebar_pkg_resource:download(Dir, Source, State).

-spec needs_update(Dir, Source) -> Res when
    Dir :: file:filaname_all(),
    Source :: tuple(),
    Res :: boolean().
needs_update(Dir, Source) ->
    rebar_pkg_resource:make_vsn(Dir, Source).

-spec make_vsn(Dir) -> Res when
    Dir :: file:filename_all(),
    Res :: {plain, string()} | {error, string()}.
make_vsn(Dir) ->
    rebar_pkg_resource:make_vsn(Dir).

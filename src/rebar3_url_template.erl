%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2017 Laszlo Toth
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% @doc
%% Implement basic converting for urls using variable substitutions.
%% @TODO add doc
%% @end

-module(rebar3_url_template).

-include("rebar3_mirroring.hrl").

-export([convert/2,
         convert_to_internal/2,
         convert_url/2,
         tokenize/1,
         tokenize_url/2,
         substitute_url/2]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @TODO reformat lines... they should not be longer than 80 characters
-spec convert_to_internal(Url, Routings) -> Res when
      Url :: string(),
      Routings :: list(routing_info()),
      Res :: error | {ok, {proplists:proplist(), proplists:proplist()}}.
convert_to_internal(_Url, []) ->
    error;
convert_to_internal(Url, [Routing | T]) ->
    case convert_url(Url, Routing) of
        error ->
            convert_to_internal(Url, T);
        {ok, Res} ->
            {ok, Res}
    end.

-spec substitute_url(Url, Templates) -> Res when
      Url :: string(),
      Templates :: list({MaybeMatching, Template}),
      MaybeMatching :: string(),
      Template :: string(),
      Res :: string().
substitute_url(Url, []) ->
    Url;
substitute_url(Url, [{MaybeMatchingUrl, Template} | T]) ->
    MaybeMatchingToks = tokenize(MaybeMatchingUrl),
    case tokenize_url(MaybeMatchingToks, Url) of
        error ->
            substitute_url(Url, T);
        UrlToks ->
            convert(UrlToks, tokenize(Template))
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec convert_url(Url, RoutingInfo) -> Res when
      Url :: string(),
      RoutingInfo :: routing_info(),
      Res :: error | {ok, {proplists:proplist(), proplists:proplist()}}.
convert_url(Url, {MaybeMatching, ExtUrlTemplates, IntUrlTemplates}) ->
    MaybeMatchingToks = tokenize(MaybeMatching),
    case tokenize_url(MaybeMatchingToks, Url) of
        error ->
            error;
        UrlToks ->
            ExtUrlData = [{Key, convert(UrlToks, tokenize(Toks))}
                          || {Key, Toks} <- ExtUrlTemplates],
            IntUrlData = [{Key, convert(UrlToks, tokenize(Toks))}
                          || {Key, Toks} <- IntUrlTemplates],
            {ok, {ExtUrlData, IntUrlData}}
    end.

-spec tokenize_url(Tokens, Url) -> Res when
      Tokens :: routing_tokens(),
      Url :: string(),
      Res :: error | url_tokens().
tokenize_url(Tokens, Url) ->
    tokenize_url(Tokens, Url, [], undefined).

-spec tokenize_url(Tokens, Url, Acc, LastVar) -> Res when
      Tokens :: routing_tokens(),
      Url :: string(),
      Acc :: url_tokens(),
      LastVar :: undefined | string(),
      Res :: error | url_tokens().
tokenize_url([], _Url, Res, undefined = _LastVar) ->
    lists:reverse(Res);
tokenize_url([], Url, Res, LastVar) ->
    lists:reverse([{var, LastVar, Url} | Res]);
tokenize_url([{var, Name} | Toks], Url, Res, undefined) ->
    tokenize_url(Toks, Url, Res, Name);
tokenize_url([{string, SubStr} | Toks], Url, Res, LastVar) ->
    case string:str(Url, SubStr) of
        0 ->
            error;
        Pos ->
            VarValue = string:sub_string(Url, 1, Pos - 1),
            UrlRemaining = string:sub_string(Url, length(SubStr) + Pos),
            case LastVar of
                undefined ->
                    tokenize_url(Toks, UrlRemaining, [{string, SubStr} | Res], undefined);
                _Var ->
                    tokenize_url(Toks, UrlRemaining, [{string, SubStr}, {var, LastVar, VarValue} | Res], undefined)
            end
    end.

-spec convert(UrlToks, RoutingToks) -> Res when
      UrlToks :: url_tokens(),
      RoutingToks :: routing_tokens(),
      Res :: string().
convert(UrlToks, RoutingToks) ->
    lists:flatten(lists:reverse(convert(UrlToks, RoutingToks, []))).

-spec convert(UrlToks, RoutingToks, Acc) -> Res when
      UrlToks :: url_tokens(),
      RoutingToks :: routing_tokens(),
      Acc :: list(string()),
      Res :: list(string()).
convert(_UrlToks, [], Res) ->
    Res;
convert(UrlToks, [{string, String} | RoutingToks], Res) ->
    convert(UrlToks, RoutingToks, [String | Res]);
convert(UrlToks, [{var, Name} | RoutingToks], Res) ->
    Value = get_variable(UrlToks, Name),
    convert(UrlToks, RoutingToks, [Value | Res]).

-spec tokenize(Url) -> Res when
      Url :: string(),
      Res :: error | routing_tokens().
tokenize(Url) ->
    case tokenize(Url, false, [], []) of
        error ->
            error;
        Toks ->
            lists:filter(fun not_empty_token/1, Toks)
    end.

-spec tokenize(Url, InVar, Tokens, Acc) -> Res when
      Url :: string(),
      InVar :: boolean(),
      Tokens :: string(),
      Acc :: routing_tokens(),
      Res :: error | routing_tokens().
tokenize([], true = _InVar, _CurrentToken, _Res) ->
    error;
tokenize([], _InVar, CurrentToken, Res) ->
    LastToken = {string, lists:reverse(CurrentToken)},
    lists:reverse([LastToken | Res]);
tokenize([$$, ${ | _Url], true, _CurrentToken, _Res) ->
    error;
tokenize([$$, ${ | Url], false, CurrentToken, Res) ->
    LastToken = {string, lists:reverse(CurrentToken)},
    tokenize(Url, true, [], [LastToken | Res]);
tokenize([$} | _Url], false, _CurrentToken, _Res) ->
    error;
tokenize([$} | Url], true, CurrentToken, Res) ->
    LastToken = {var, lists:reverse(CurrentToken)},
    tokenize(Url, false, [], [LastToken | Res]);
tokenize([C | Url], InVar, CurrentToken, Res) ->
    tokenize(Url, InVar, [C | CurrentToken], Res).

-spec not_empty_token(Token) -> Res when
      Token :: {string(), string()},
      Res :: boolean().
not_empty_token({_Type, []}) ->
    false;
not_empty_token({_Type, _Token}) ->
    true.

-spec get_variable(Tokens, Var) -> Res when
      Tokens :: url_tokens(),
      Var :: string(),
      Res :: string().
get_variable(Tokens, Var) ->
    Vars = lists:filter(fun({var, _, _}) -> true;
                           (_) -> false
                        end,
                        Tokens),
    {var, Var, Value} = lists:keyfind(Var, 2, Vars),
    Value.

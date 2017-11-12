%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------

-type routing_info()       :: {Url :: string(), proplists:proplist(),
                               proplists:proplist()}.
-type routing_token()      :: {string, string()} | {var, string()}.
-type routing_tokens()     :: list(routing_token()).
-type url_token()          :: {string, string()} | {var, string(), string()}.
-type url_tokens()         :: list(url_token()).
-type rebar_source()       ::
    {git, string(), term()} | {hg, string(), term()} |
                              {pkg, string(), string(), term()}.
-type rebar_resource_mod() :: rebar_pkg_resource | rebar_git_resource |
                              rebar_hg_resource.

-define(WAIT_AFTER_CREATE_REPO, 5000).
-define(FAILING_COMMANDS_LOG, "/tmp/rebar_routing_failures.log").

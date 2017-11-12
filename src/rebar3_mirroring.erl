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
%%
%% Module for  cloning remote repositories into our EXTERNAL/INTERNAL repos.
%% If the url template stored in the routing section of the configuration
%% matches the address of the remote repository the routing will be executed
%% and those EXTERNAL/INTERNAL repositories will be cloned and automatically
%% updated with the new commits in the case of an upgrade command.
%%
%% The configuration for routing works as follows:
%%
%% The code tries to read the contents of rebar_routing.lock file.
%%
%% If it is able to read the contents of the file the configuration will be
%% what is stored in the file, otherwise try to find the
%% key `remote_url` in the routing section of the rebar configration.
%%
%% If remote_url is present, download the erlang term from URL,
%% and return the contents.
%%
%% If remote_url is not present simply return the contents of the routing
%% section of the configuration.
%%
%% EXTERNAL and INTERNAL repositories have the following meaning:
%% Projects downloaded from a remote repository (like github) will be downloaded
%% into the EXTERNAL repository.
%% The purpose of the INTERNAL repository is similar but it contains our
%% changes so its contents will be automatically rebased by this code against
%% the remote changes.
%%
%% An other possibility is to use the substitute_urls option which will only
%% specify that the url on the left side will be substituted and the value
%% on the right side will be used during a download. Using this option the
%% contents of the external repositories won't be cloned into the local repos.
%%
%% For routing the route_urls option can be used. A list of patterns can
%% be specified. For example:
%%
%% <xmp>
%%
%%  {route_urls, [
%%    {"https://github.com/${user}/${project}.git",
%%      [{url, "https://bitbucket.erfk.kozpont.otp/scm/ext/${project}.git"},
%%       {ssh_url, "ssh://git@bitbucket.erfk.kozpont.otp:7999/ext/${project}.git"},
%%       {host, "https://bitbucket.erfk.kozpont.otp"},
%%       {project, "${project}"}],
%%      [{url, "https://bitbucket.erfk.kozpont.otp/scm/int/${project}.git"},
%%       {ssh_url, "ssh://git@bitbucket.erfk.kozpont.otp:7999/int/${project}.git"},
%%       {host, "https://bitbucket.erfk.kozpont.otp"},
%%       {project, "${project}"}]
%%    }
%%  ]}
%% </xmp>
%%
%% There are three elements in this tuple. The first one contains a pattern
%% for a remote repository url. If the current URL being processed matches this
%% pattern the URL will be routed, meaning it will be cloned into our
%% EXTERNAL and INTERNAL repositories. The url and other required information
%% for these mirrored repositores are described in the next two elements of
%% the tuple. As the example shows both of these two elmements
%% are proplists.
%%
%% Variable names can be used in the patterns with ${VAR_NAME} syntax.
%% The current values coming from the remote URL will be injected into those
%% proplists specifying the EXTERNAL and INTERNAL repositories.
%%
%% Initially if during a rebar compile the module faces a new dependency which
%% has not been downloaded into these EXTERNAL/INTERNAL repos, the contents of
%% this remote repo will be cloned into these two repositories.
%%
%% The difference between EXTERNAL and INTERNAL will be important during an
%% upgrade. If there are newer commits on the remote (e.g. github) repo
%% than on our EXTERNAL repository we call the upgrade function, which will
%% simply copy all branches from the remote into the EXTERNAL branches.
%%
%% As INTERNAL contains our local changes to the dependency the contents of the
%% branch being updated must be rebased against the remote changes.
%%
%% If the rebase is successfull the contents of the rebased branch
%% will be pushed into the INTERNAL repository.
%%
%% In case of a failing rebase a PR will be created containing the remote
%% changes, and a pull request will be created containing all the merge conflicts
%% against the INTERNAL branch.
%%
%% Example rebar config:
%%
%% <xmp>
%% {routing, [
%%    %% Os commands are executed along the way. If one of them will fail, the
%%    %% command and the error message coming back from the command will be logged
%%    %% into this file.
%%    {failing_commands_log, "/tmp/rebar_routing_failures.log"},
%%    %% If hexpm packages are used, the user executing the upgrade command
%%    %% should be able to ssh into the server with this id.
%%    {hexpm_user, "hexpm@hexpm"},
%%    %% The path where the hexpm packages are mirrored
%%    {hexpm_path, "/home/reposerver/images/hexpm"},
%%    %% Before doing the routing the URL's on the left side of the following
%%    %% tuples will be transformed into the form specified on the right side.
%%    {substitute_urls, [
%%            {"git://github.com/${user}/${project}.git", "https://github.com/${user}/${project}.git"},
%%            {"git://github.com/${user}/${project}", "https://github.com/${user}/${project}.git"}
%%        ]},
%%    %% Used when there is a merge conflict during an upgrade.
%%    %% In that case the conflict will have to be manually solved.
%%    %% A PR will be created containing the external changes towards the internal
%%    %% repository. A notification about the created PR will be sent to the
%%    %% list of 'reviewers' specified bellow.
%%    {pr_reviewers, ["TothLaszlo4@irfi.OTP"]},
%%    %% Routing infos. The fields [url, ssh_url, host, project] are mandatory
%%    %% for the INTERNAL and EXTERNAL repositories as they are used during
%%    %% cloning/upgrading.
%%    {route_urls, [
%%    {route_urls, [
%%       {"https://github.com/${user}/${project}.git",
%%        [{url, "https://bitbucket.erfk.kozpont.otp/scm/ext/${user}_${project}.git"},
%%         {ssh_url, "ssh://git@bitbucket.erfk.kozpont.otp:7999/ext/${user}_${project}.git"},
%%         {host, "https://bitbucket.erfk.kozpont.otp"},
%%         {project, "${user}_${project}"}],
%%        [{url, "https://bitbucket.erfk.kozpont.otp/scm/int/${user}_${project}.git"},
%%         {ssh_url, "https://bitbucket.erfk.kozpont.otp/scm/int/${user}_${project}.git"},
%%         {host, "https://bitbucket.erfk.kozpont.otp"},
%%         {project, "${user}_${project}"}]},
%%       {"https://repo.hex.pm:${port}/tarballs/${project}?",
%%        [{url, "https://repo.erfk.kozpont.otp:8081/repo/hexpm/${project}?"},
%%         {project, "${project}"},
%%         {ssh_url, "https://repo.erfk.kozpont.otp:8081/repo/hexpm/${project}?"}],
%%        [{url, "https://repo.erfk.kozpont.otp:8081/repo/hexpm/${project}?"},
%%         {project, "${project}"},
%%         {ssh_url, "https://repo.erfk.kozpont.otp:8081/repo/hexpm/${project}?"}]}
%%    ]}]}.
%% </xmp>
%%
%% In order to be able to commit into the EXTERNAL/INTERNAL repositories
%% user credentials have to be stored in the global rebar.config
%% (~/.config/rebar3/rebar.config").
%%
%% These credentials are required only during creating the external/internal
%% repositories. If this is not stored in the configuration the module
%% won't be able to create the repositories so the module will try to download
%% from the original URL (without routing).
%%
%% For example:
%%
%% <xmp>
%%  {routing_credentials, [
%%      {user, ""}
%%    ]}.
%% </xmp>
%%
%% @end


-module(rebar3_mirroring).

-export([init/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    NewState =
        lists:foldl(fun({Resource, Module}, StateAcc) ->
                        rebar_state:add_resource(StateAcc, {Resource, Module})
                    end, State,
                    [{git, rebar3_git_mirroring},
                     {pkg, rebar3_pkg_mirroring},
                     {hg, rebar3_hg_mirroring}]),
    {ok, NewState}.

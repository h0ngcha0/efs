%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc entry point of the efs backend application
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(efs_be_app).
-behaviour(application).

%%%_* Exports ==========================================================
%% Application callbacks
-export([ start/2
        , stop/1]).

%%%_* Code =============================================================
start(_StartType, _StartArgs) ->
  efs_be_sup:start_link().

stop(_State) ->
  ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc efs core supervisor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(efs_be_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([start_link/0]).
-export([init/1]).

%%%_* Macros ===========================================================
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%_* Code =============================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Ip     = efs_be:get_app_env(web_ip, "0.0.0.0"),
  Port   = efs_be:get_app_env(web_port, 8001),
  LogDir = efs_be:get_app_env(log_dir, "priv/log"),

  lager:info("efs_be port:~p", [Port]),

  {ok, Dispatch} = file:consult( filename:join( code:priv_dir(efs_be)
                                              , "dispatch.conf")),

  WebConfig = [ {ip       , Ip}
              , {port     , Port}
              , {log_dir  , LogDir}
              , {dispatch , Dispatch}],

  efs_be:read_mapred_js(),

  Child = { webmachine_mochiweb
          , {webmachine_mochiweb, start, [WebConfig]}
          ,  permanent, 5000, worker, [webmachine_mochiweb]},

  {ok, {{one_for_one, 5, 10}, [Child]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

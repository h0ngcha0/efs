%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc web machine resource for images
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_img_resource).

%%%_* Exports ==================================================================
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , to_json/2
        ]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Macros ===================================================================
%%%_* Records ==================================================================

%%%_* Code =====================================================================
init([]) -> {ok, []}.

allowed_methods(RD, Ctx) ->
  {['GET'], RD, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json} ],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  api_resource_lib:maybe_store_val(ReqData, Ctx, [version, id]).

to_json(ReqData, Ctx) ->
  Data = {array, []},
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal Functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

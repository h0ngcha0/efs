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
-define(B_IMG, <<"img">>).
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
  case api_resource_lib:maybe_store_val(ReqData, Ctx, [version, id]) of
    {true, _, _} ->
      api_resource_lib:maybe_store_val( ReqData, Ctx
                                      , [ version, nw, ne, sw
                                        , se, year, sample]);
    ReturnVal    -> ReturnVal
  end.

to_json(ReqData, Ctx) ->
  Query  = mk_query(ReqData),
  Result = wrc:run(Query),
  Data   = {array, Result},
  {mochijson2:encode(Data), ReqData, Ctx}.

mk_query(ReqData) ->
  Get  = fun(KeyStr) ->
             io:format("KeyStr:~p~n",[KeyStr]),
           list_to_float(wrq:get_qs_value(KeyStr, ReqData))
         end,
  Se   = Get("se"),
  Sw   = Get("sw"),
  Ne   = Get("ne"),
  Nw   = Get("nw"),
  Year = case wrq:get_qs_value("year", ReqData) of
           undefined -> 2013;
           YearStr   -> list_to_integer(YearStr)
         end,
  fun(Client) ->
      Results0 = wrc:mapred( Client
                           ,  ?B_IMG
                           , [make_map_spec( efs_img
                                           , efs_query
                                           , {Se, Sw, Ne, Nw, Year}
                                           , true)]),
      Results = case Results0 of
                  {ok, [{_, Res}]} -> Res;
                  {ok, []}         -> []
                end,
      lager:info("fetching imgs:~p", [Results]),
      Results
  end.

make_map_spec(Module, Func, Args, Show) ->
  { map
  , {modfun, Module, Func}
  , Args
  , Show}.

%%%_* Internal Functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

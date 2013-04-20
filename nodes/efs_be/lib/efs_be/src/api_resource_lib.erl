%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc API resource for querying the img datasets
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_resource_lib).

%%%_* Exports ==================================================================
-export([ get_user/1
        , get_key/1
        , get_value/2
        , is_authorized/1
        , make_error_rd/2
        , make_header/1
        , make_ok_rd/1
        , maybe_store_val/3
        ]).

%%%_* Code =====================================================================
get_user(RD) ->
  wrq:get_qs_value("uid", RD).

get_key(RD) ->
  get_value("id", RD).

get_value(Key, RD) ->
  Val = wrq:get_qs_value(Key, RD),
  case Val of
    undefined   -> undefined;
    Val         -> UnQuotedVal = mochiweb_util:unquote(Val),
                   list_to_binary(UnQuotedVal)
  end.

%% TODO: at least check if the user id exist in db.
is_authorized(_UserId) ->
  true.

make_ok_rd(RD) ->
  make_header(wrq:set_resp_body("ok", RD)).

make_error_rd(ErrorJson, RD) ->
  make_header(wrq:set_resp_body(ErrorJson, RD)).

make_header(RD) ->
  wrq:set_resp_header("Content-type", "application/json", RD).

maybe_store_val(ReqData0, Ctx0, Keys) ->
  F = fun(Key, {ReqDataInner, CtxInner}) ->
          Value = validate(Key, ReqDataInner),
          {ReqDataInner, orddict:store(Key, Value, CtxInner)}
      end,
  try
    {ReqData, Ctx} = lists:foldl(F, {ReqData0, Ctx0}, Keys),
    {false, ReqData, Ctx}
  catch throw:{error, Key} = E ->
      KeyString = case Key of
                    {_Type, KeyAtom} -> atom_to_list(KeyAtom);
                    KeyAtom          -> atom_to_list(KeyAtom)
                  end,
      lager:info( "Invalid Request, ~nKey: ~p~nValue: ~p"
                , [Key, wrq:path_info(Key, ReqData0)]),
      {true, ReqData0, orddict:store(error, E, Ctx0)}
  end.

validate(Key, ReqData) ->
  case (term_to_validate(Key))(ReqData) of
    {ok, Value}  -> Value;
    {error, Rsn} ->
      lager:debug( "API input validation failed. Key ~p, Rsn: ~p"
                 , [Key, Rsn]),
      throw({error, Key})
  end.

term_to_validate(version)   -> fun version_validate/1;
term_to_validate(id)        -> fun id_validate/1.

version_validate(ReqData) ->
  do_version_validate(wrq:path_info(version, ReqData)).

do_version_validate(undefined)  -> {ok, "V1"};
do_version_validate(VersionStr) ->
  try
    case lists:nth(1, VersionStr) of
      86  -> ok;  %% V
      118 -> ok;  %% v
      _   -> throw(no_v)
    end,
    is_pos_int_str(lists:nthtail(1,VersionStr))
  catch
    error:badarg    -> {error, version_invalid};
    throw:no_v      -> {error, version_invalid}
  end.

id_validate(ReqData) ->
  is_pos_int_str(wrq:path_info(id, ReqData)).

is_pos_int_str(IntStr) ->
  try
    case list_to_integer(IntStr) of
      Int when Int >=0 -> {ok, Int};
      _                -> throw(not_pos_int)
    end
  catch
    throw:not_pos_int -> {error, id_invalid};
    error:badarg      -> {error, id_invalid}
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

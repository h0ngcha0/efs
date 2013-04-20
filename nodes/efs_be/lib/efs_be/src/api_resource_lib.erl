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
      lager:info( "Invalid Request, ~nKey: ~p~nValue: ~p"
                , [Key, val(Key, ReqData0)]),
      {true, ReqData0, orddict:store(error, E, Ctx0)}
  end.

validate(Key, ReqData) ->
  case (term_to_validate(Key))(ReqData) of
    {ok, Value}  -> Value;
    {error, Rsn} ->
      throw({error, Key})
  end.

term_to_validate(version)   -> fun version_validate/1;
term_to_validate(id)        -> fun id_validate/1;
term_to_validate(nw)        -> mk_cord_validate("nw");
term_to_validate(ne)        -> mk_cord_validate("ne");
term_to_validate(sw)        -> mk_cord_validate("sw");
term_to_validate(se)        -> mk_cord_validate("se");
term_to_validate(year)      -> fun year_validate/1;
term_to_validate(sample)    -> fun sample_validate/1.

version_validate(ReqData) ->
  do_version_validate(wrq:path_info(version, ReqData)).

do_version_validate(undefined)  -> {ok, default_version()};
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

year_validate(ReqData) ->
  case wrq:get_qs_value("year", ReqData) of
    undefined -> {ok, default_year()};
    YearStr   -> is_pos_int_str(YearStr)
  end.

sample_validate(ReqData) ->
  case wrq:get_qs_value("sample", ReqData) of
    undefined -> {ok, default_sample()};
    SampleStr   -> is_pos_int_str(SampleStr)
  end.

mk_cord_validate(Orientation) ->
  fun(ReqData) ->
    cord_validate(Orientation, ReqData)
  end.

cord_validate(Orientation, ReqData) ->
  is_float_str(wrq:get_qs_value(Orientation, ReqData)).

is_pos_int_str(IntStr) ->
  try
    case list_to_integer(IntStr) of
      Int when Int >=0 -> {ok, Int};
      _                -> throw(not_pos_int)
    end
  catch
    throw:not_pos_int -> {error, not_pos_int};
    error:badarg      -> {error, not_pos_int}
  end.

is_float_str(FloatStr) ->
  try
    case list_to_float(FloatStr) of
      Float -> {ok, Float};
      _     -> throw(not_float)
    end
  catch
    throw:not_float -> {error, not_float};
    error:badarg    -> {error, not_float}
  end.

val(Key, ReqData) ->
  case lists:member(Key, path_val()) of
    false -> wrq:get_qs_value(atom_to_list(Key), ReqData);
    true  -> wrq:path_info(Key, ReqData)
  end.

path_val() ->
  [id, version].

default_version() ->
  "V1".

default_year() ->
  2013.

default_sample() ->
  100.
%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

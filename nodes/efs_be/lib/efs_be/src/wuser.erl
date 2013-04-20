%%%_* Module declaration ===============================================
-module(wuser).

%%%_* Exports ==========================================================
-export( [ create/2
         , fetch/1
         , get_email/1
         , get_name/1
         , set_login_time/2
         , store/1
         ]).

%%%_* Includes =========================================================
-include_lib("efs_be/include/efs_be.hrl").

%%%_* Macros ===========================================================
-define(F_EMAIL,      <<"email">>).
-define(F_NAME,       <<"name">>).
-define(F_LOGIN_TIME, <<"login_time">>).

%%%_* Code =============================================================

%% @doc fetch a user based on the user id @end
-spec fetch(UserId :: binary()) ->
               {ok, User :: binary()} | {error, term()}.
fetch(UserId) ->
  Fun = fun(Client) ->
            fetch(Client, UserId)
        end,
  wrc:run(Fun).

fetch(Client, UserId) when is_list(UserId) ->
  fetch(Client, list_to_binary(UserId));
fetch(Client, UserId) when is_binary(UserId) ->
  case wrc:get(Client, ?B_USER, UserId) of
    {ok, Wobj} ->
      lager:info("fetched user:~p", [Wobj]),
      {ok, wobj:to_json(<<"user">>, [Wobj])};
    Error      ->
      lager:error("error when fetching user:~p", [Error]),
      Error
  end.

%% @doc store a new user @end
-spec store(UserJson :: binary()) ->
               ok | {ok, tuple()} | {ok, binary()} | {error, term()}.
store(UserJson) ->
  Fun = fun(Client) ->
            store(Client, UserJson)
        end,
  wrc:run(Fun).

store(Client0, UserJson) ->
  WUser        = to_wobj(UserJson),
  {ok, Client} = wrc:set_client_id(Client0, wobj:key(WUser)),
  lager:debug("store user:~p", [UserJson]),
  wrc:put(Client, WUser).

create(Username, DecodedJsonStruct) ->
  wobj:create(?B_USER, Username, DecodedJsonStruct).

get_name(User) ->
  case wobj:get_json_field(User, ?F_NAME) of
    {struct, Name} ->
      wobj:get_json_field(Name, <<"formated">>);
    Name ->
      Name
  end.

get_email(User) ->
  wobj:get_json_field(User, ?F_EMAIL).

set_login_time(User, Ts) ->
  Utc = calendar:now_to_universal_time(Ts),
  Sec = calendar:datetime_to_gregorian_seconds(Utc),
  wobj:set_json_field(User, ?F_LOGIN_TIME, Sec).

to_wobj(UserJson) ->
  {struct, [{<<"user">>, {struct, UserFields}}]} = mochijson2:decode(UserJson),
  UserId = proplists:get_value(<<"key">>, UserFields),
  create(UserId, {struct, UserFields}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

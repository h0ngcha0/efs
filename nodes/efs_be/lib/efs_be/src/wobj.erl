%%%_* Module declaration ===============================================
-module(wobj).

%%%_* Exports ==========================================================
-export([ add_link/2
        , bucket/1
        , create/3
        , create_siblings/4
        , from_riakc_obj/1
        , get_content_type/1
        , get_json_field/2
        , get_links/1
        , get_siblings/1
        , get_value/1
        , get_vclock/1
        , has_siblings/1
        , key/1
        , remove_links/3
        , set_content_type/2
        , set_json_field/3
        , set_links/2
        , set_value/2
        , set_vclock/2
        , to_riakc_obj/1
        , to_json/2
        , to_json/3
        ]).

%%%_* Includes =========================================================
-include_lib("riakc/include/riakc.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
%%%_* Macros ===========================================================
-define(CTYPE_JSON, "application/json").

%%%_* Records ==========================================================
-record(wobj, { bucket
              , key
              , value
              , links=[]
              , ctype=?CTYPE_JSON
              , vclock}).

-record(wsib, { bucket
              , key
              , vclock
              , sibs}).

%%%_* Code =============================================================
create(Bucket, Key, Value) when is_binary(Bucket), is_binary(Key) ->
  #wobj{bucket=Bucket, key=Key, value=Value}.

create_siblings(Bucket, Key, Vclock, Siblings)
  when is_binary(Bucket), is_binary(Key),
       is_binary(Vclock), is_list(Siblings) ->
  #wsib{bucket=Bucket, key=Key, vclock=Vclock, sibs=Siblings}.

get_vclock(#wobj{vclock=Vclock}) ->
  Vclock;
get_vclock(#wsib{vclock=Vclock}) ->
  Vclock.

set_vclock(Obj, Vclock) when is_binary(Vclock); Vclock =:= undefined ->
  Obj#wobj{vclock=if Vclock =:= <<>> -> undefined;
                     true            -> Vclock
                  end};
set_vclock(Obj, Vclock) when is_list(Vclock)                         ->
  set_vclock(Obj, list_to_binary(Vclock)).

bucket(#wobj{bucket=Bucket}) ->
  Bucket;
bucket(#wsib{bucket=Bucket}) ->
  Bucket.

key(#wobj{key=Key}) ->
  Key;
key(#wsib{key=Key}) ->
  Key.

get_value(#wobj{value=Value}) ->
  Value.

set_value(Obj, Value)         ->
  Obj#wobj{value=Value}.

get_links(#wobj{links=Links})  ->
  Links.

remove_links(Obj, Bucket, Tag) ->
  Filter = link_filter(Bucket, Tag),
  set_links(Obj, [Link || Link <- get_links(Obj), Filter(Link) =/= true]).

link_filter('_', '_')                     ->
  fun(_) -> true end;
link_filter(Bucket, '_')                  ->
  fun({{B,_},_}) -> B =:= Bucket end;
link_filter('_', Tag)                     ->
  fun({_,T}) -> T =:= Tag end;
link_filter(Bucket, Tag)                  ->
  fun({{B,_},T}) -> B =:= Bucket andalso T =:= Tag end.

set_links(Obj, Links) when is_list(Links) ->
  Obj#wobj{links=Links}.

add_link(Obj, Link={{_,_},_})             ->
  Links = get_links(Obj),
  case lists:member(Link, Links) of
    true -> Obj;
    false -> set_links(Obj, [Link|Links])
  end.

get_content_type(#wobj{ctype=ContentType}) ->
  ContentType.

set_content_type(Obj, ContentType)         ->
  Obj#wobj{ctype=ContentType}.

get_siblings(#wsib{bucket=Bucket, key=Key, vclock=Vclock, sibs=Siblings}) ->
  [set_links(
     set_content_type(
       set_vclock(
         create(Bucket, Key, Value),
         Vclock),
       ContentType),
     Links)
   || {ContentType, Links, Value} <- Siblings].

has_siblings(#wsib{}) -> true;
has_siblings(#wobj{}) -> false.

get_json_field(RecObj, Field)        ->
  {struct, Props} = wobj:get_value(RecObj),
  proplists:get_value(Field, Props).

set_json_field(RecObj, Field, Value) ->
  {struct, Props} = wobj:get_value(RecObj),
  NewProps = [{Field, Value}
              | [ {F, V} || {F, V} <- Props, F =/= Field ]],
  wobj:set_value(RecObj, {struct, NewProps}).

from_riakc_obj(RCObj) ->
  case riakc_obj:get_contents(RCObj) of
    [{MD, V}] -> % lone value
      WObj = create(riakc_obj:bucket(RCObj),
                    riakc_obj:key(RCObj),
                    decode_value(dict:fetch(?MD_CTYPE, MD), V)),
      VWObj = set_vclock(WObj,
                         encode_vclock(riakc_obj:vclock(RCObj))),
      CVWObj = set_content_type(
                 VWObj, riakc_obj:get_content_type(RCObj)),
      case dict:find(?MD_LINKS, MD) of
        {ok, Links} -> set_links(CVWObj, Links);
        error       -> CVWObj
      end;
    RCSibs -> % siblings
      WSibs = [{dict:fetch(?MD_CTYPE, RCMD),
                case dict:find(?MD_LINKS, RCMD) of
                  {ok, Links} -> Links;
                  error       -> []
                end,
                decode_value(dict:fetch(?MD_CTYPE, RCMD),
                             RCV)}
               || {RCMD, RCV} <- RCSibs],
      create_siblings(riakc_obj:bucket(RCObj),
                      riakc_obj:key(RCObj),
                      encode_vclock(riakc_obj:vclock(RCObj)),
                      WSibs)
  end.

to_riakc_obj(WObj)    ->
  O = riakc_obj:new_obj(bucket(WObj),
                        key(WObj),
                        decode_vclock(get_vclock(WObj)),
                        []),
  MD = dict:from_list([{?MD_LINKS, get_links(WObj)},
                       {?MD_CTYPE, get_content_type(WObj)}]),
  MDO = riakc_obj:update_metadata(O, MD),
  riakc_obj:update_value(MDO, encode_value(get_content_type(WObj),
                                           get_value(WObj))).

decode_value(?CTYPE_JSON, V)         ->
  mochijson2:decode(V);
decode_value(_, V) when is_list(V)   ->
  iolist_to_binary(V);
decode_value(_, V) when is_binary(V) ->
  V.

encode_value(?CTYPE_JSON, V)         ->
  iolist_to_binary(mochijson2:encode(V));
encode_value(_, V) when is_list(V)   ->
  iolist_to_binary(V);
encode_value(_, V) when is_binary(V) ->
  V.

encode_vclock(undefined) ->
  undefined;
encode_vclock(V)         ->
  base64:encode(V).

decode_vclock(undefined) ->
  undefined;
decode_vclock(V)         ->
  base64:decode(V).

to_json(JsonRoot, Wobjs)           ->
  F = fun(Wobj, Acc) ->
          Key    = wobj:key(Wobj),
          Vclock = wobj:get_vclock(Wobj),
          {struct, FieldList} = wobj:get_value(Wobj),
          [{JsonRoot, {struct, [ {<<"key">>,    Key}
                               , {<<"vclock">>, Vclock}
                               | FieldList]}} | Acc]
      end,
  to_json({struct, lists:foldl(F, [], Wobjs)}).

to_json(JsonRoot, Wobjs, PropLFun) ->
  F = fun(Wobj, Acc) ->
          Key    = wobj:key(Wobj),
          Vclock = wobj:get_vclock(Wobj),
          {struct, FieldList} = wobj:get_value(Wobj),
          [{JsonRoot, {struct, PropLFun(Wobj) ++
                         [ {<<"key">>,    Key}
                         , {<<"vclock">>, Vclock}
                         | FieldList]}} | Acc]
      end,
  to_json({struct, lists:foldl(F, [], Wobjs)}).

to_json(JsonStruct)                     ->
  list_to_binary(mochijson2:encode(JsonStruct)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

remove_links_test()                                                       ->
  WObj = wobj:create(<<"bucket">>,<<"key">>,<<"value">>),
  WObj1 = wobj:add_link(WObj, {{<<"bucket">>,<<"key">>},<<"tag1">>}),
  WObj2 = wobj:add_link(WObj1, {{<<"bucket">>,<<"key">>},<<"tag2">>}),
  WObj3 = wobj:remove_links(WObj2, <<"bucket">>, <<"tag1">>),
  ?assertEqual([{{<<"bucket">>,<<"key">>},<<"tag2">>}], wobj:get_links(WObj3)).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

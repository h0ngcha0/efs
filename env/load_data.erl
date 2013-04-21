#!/usr/bin/env escript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc utility to load img datasets to riak
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% csv format
%% image_id,center_lng,center_lat,nadir_lng,nadir_lat,corner1_lng,corner1_lat,corner2_lng,corner2_lat,corner3_lng,corner3_lat,corner4_lng,corner4_lat,altitude,lens_size,frame_width,frame_height,taken_utc,mission_name,orbit_num,image_page,image_link,kml_link,schools

%% this script convert the csv to simple flat json format and put it into riak

%%%_* Macros ===================================================================
%%%_* Records ==================================================================
%%%_* Code =====================================================================
main([Filename]) ->
  load([Filename]);
main([Filename|Filenames]) ->
  load([Filename]),
  main(Filenames).

load([Filename]) ->
  {ok, Data} = file:read_file(Filename),
  Lines = tl(re:split(Data, "\r?\n", [{return, list},trim])),
  lists:foreach(fun(L) -> LS0 = re:split(L, ","),
                          LS  = lists:sublist(LS0, 1, 23),  %% load first 24 fields
                          format_and_insert(LS)
                end, Lines).

%%%_* Internal Functions =======================================================
format_and_insert(Line) ->
  FinalFieldsStrFmt = lists:foldl(fun(Field, FieldsFmt) ->
                                    FieldsFmt ++ mk_field(Field)
                                  end, [], all_fields()),
  JSONFmt = "{"++FinalFieldsStrFmt++"\"kml_link\":~s}",
  JSON = io_lib:format(JSONFmt, Line),
  Command = io_lib:format("curl -XPUT http://127.0.0.1:10018/riak/img/~s -d '~s' -H 'content-type: application/json'", [hd(Line),JSON]),
  io:format("Inserting: ~s~n", [hd(Line)]),
  os:cmd(Command).

mk_field(Field) ->
  "\""++atom_to_list(Field)++"\""++":~s,".

all_fields() ->
  [ image_id, center_lng, center_lat, nadir_lng, nadir_lat, corner1_lng
  , corner1_lat, corner2_lng, corner2_lat, corner3_lng, corner3_lat
  , corner4_lng, corner4_lat, altitude, lens_size, frame_width, frame_height
  , taken_utc, mission_name, orbit_num, image_page, image_link].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

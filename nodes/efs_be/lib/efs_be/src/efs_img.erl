%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc efs img query module. beam needs to be accessible from riak
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(efs_img).

-export([ efs_query/3
        ]).

%% efs_query(Obj, _, _)  ->
%%   Print = fun(Str) ->
%%                file:write_file("/tmp/query", Str, [append])
%%            end,
%%   Print(io_lib:format("Obj:~p~n", [Obj])),
%%   [1].
efs_query({error, notfound}, _, _)  ->
  [];
efs_query(Obj, _, {Se, Sw, Ne, Nw}) ->
  Print = fun(Str) ->
              file:write_file("/tmp/query", Str, [append])
          end,
  try
    %% Print(io_lib:format("Obj:~p~n", [Obj])),
    %% Print(io_lib:format("Se:~p, Sw:~p, Ne:~p, Nw:~p~n", [Se, Sw, Ne, Nw])),
    Val = riak_object:get_value(Obj),
    %% Print(io_lib:format("Val:~p~n", [Val])),
    {struct, Structs} = mochijson2:decode(Val),
    %% Print(io_lib:format("Structs:~p~n", [Structs])),
    {_, CenterLng0} = lists:keyfind(<<"center_lng">>, 1, Structs),
    {_, CenterLat0} = lists:keyfind(<<"center_lat">>, 1, Structs),
    %% Print(io_lib:format("CenterLng0:~p~n", [CenterLng0])),
    %% Print(io_lib:format("CenterLat0:~p~n", [CenterLat0])),
    CenterLng = list_to_float(binary_to_list(CenterLng0)),
    CenterLat = list_to_float(binary_to_list(CenterLat0)),
    %% Print(io_lib:format("CenterLng:~p~n", [CenterLng])),
    %% Print(io_lib:format("CenterLat:~p~n", [CenterLat])),
    case (CenterLng > Se) andalso (CenterLng < Sw)
      andalso (CenterLat > Ne) andalso (CenterLat < Nw) of
      true  -> [{struct, Structs}];
      false -> []
    end
  catch
    _:_ -> []
  end.

  %% case (CenterLng > Sw) andalso (CenterLng < Se) andalso
  %%   (CenterLat > Nw) andalso (CenterLat < Ne) of
  %%   true  -> io:format("----------------~n"),[];
  %%   false -> io:format("++++++++++++++++~n"),[]
  %% end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

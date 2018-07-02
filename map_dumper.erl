-module(map_dumper).

-compile(export_all).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

parse_tiles(<<>>, _, _) -> ok;
parse_tiles(<<Id:16/little, Id2:8, TileKindIndex:8, Bin/binary>>, {MaxX,MaxY}, {X,Y,EtsTable}) ->
	ets:insert(EtsTable, {{Y,X}, {Id, Id2, TileKindIndex}}),
	{NewX, NewY} = case X rem MaxX of
		0 when X /= 0 -> {0, Y+1};
		_ -> {X+1, Y}
	end,

	parse_tiles(Bin, {MaxX, MaxY}, {NewX, NewY, EtsTable})
	.

parse_objects(<<>>, Acc) -> Acc;
parse_objects(<<X:32/little, Y:32/little, Name:24/binary, R/binary>>, Acc) ->
	NameParts = binary:split(Name, <<0>>, [global, trim_all]),
	%?PRINT({Name, NameParts}),
	Name4 = lists:foldl(fun(Part, Acc)-> 
		case unicode:characters_to_list(normalize_korean(Part), utf16) of
			{incomplete, _, _} -> Acc;
			{error, _, _} -> Acc;
			Res -> Acc ++ "|" ++ Res
		end
	end, [], NameParts),
	Name3 = case Name4 of
		[] -> [1];
		_ -> Name4
	end,
	%?PRINT(Name3),
	[_| Name2] = Name3,
	%?PRINT({Name2}),
	parse_objects(R, Acc#{{Y, X}=> Name2})
	.


dump_tile_to_csv([], File) -> file:close(File);
dump_tile_to_csv([{{Y, X}, {Id, Id2, TileIdx}}|T], File) ->
	Bin = <<
		(integer_to_binary(Y))/binary, ",",
		(integer_to_binary(X))/binary, ",",
		(integer_to_binary(Id))/binary, ",",
		(integer_to_binary(Id2))/binary, ",",
		(integer_to_binary(TileIdx))/binary, "\r\n"
	>>,
	ok = file:write(File, unicode:characters_to_list(Bin)),
	%?PRINT(Bin),
	dump_tile_to_csv(T, File)
	.

dump_tile_to_tmx([], File) -> file:close(File);
dump_tile_to_tmx([{{Y, X}, {Id, Id2, TileIdx}}|T], File) ->
	CalcedGid = lktil_gid(Id, Id2, TileIdx),

	Bin = <<"<tile gid=\"", (integer_to_binary(CalcedGid))/binary, "\"/>\r\n">>,

	ok = file:write(File, unicode:characters_to_list(Bin)),
	%?PRINT(Bin),
	dump_tile_to_tmx(T, File)
	.

dump_obj_to_csv([], File) -> file:close(File);
dump_obj_to_csv([{{Y, X}, Name}|T], File) ->
	ok = file:write(File, <<(integer_to_binary(Y))/binary, ",">>),
	ok = file:write(File, <<(integer_to_binary(X))/binary, ",">>),
	BinName = unicode:characters_to_binary(Name),
	ok = file:write(File, BinName),
	ok = file:write(File, <<"\r","\n">>),
	%?PRINT(Bin),
	dump_obj_to_csv(T, File)
	.

dump_map(FileName) ->
	{ok, <<_:32/binary, W:32/little, H:32/little, 
		TilName:16/binary, ObjName:16/binary,
		ObjCount:32/little, 
		Bin/binary>>} = file:read_file("Map/"++FileName),

	RootFileName = filename:rootname(FileName),

	?PRINT({FileName, W, H}),
	TilesBinLen = W*H*4,
	<<TileBin:TilesBinLen/binary, Bin2/binary>> = Bin,

	TileEts = ets:new(obj_ets, [ordered_set]),
	parse_tiles(TileBin, {W-1, H-1}, {0, 0, TileEts}),
	TileList = ets:tab2list(TileEts),

	{ok, TileFile} = file:open("mapdump/"++RootFileName++".tile.txt", [write]),
	?PRINT(TileFile),
	dump_tile_to_csv(TileList, TileFile),

	{ok, TmxFile} = file:open("mapdump/"++RootFileName++".tmx.txt", [write]),
	dump_tile_to_tmx(TileList, TmxFile),
	%?PRINT({ets:tab2list(TileEts)}),


	ObjBinLen = ObjCount*32,
	<<ObjBin:ObjBinLen/binary>> = Bin2,

	RR = parse_objects(ObjBin, #{}),
	ObjEts = ets:new(obj_ets, [ordered_set]),
	lists:foreach(fun(K)-> 
		V = maps:get(K, RR),
		ets:insert(ObjEts, {K, V})
	end, maps:keys(RR)),
	ObjList = ets:tab2list(ObjEts),

	{ok, ObjFile} = file:open("mapdump/"++RootFileName++".obj.txt", [write]),
	dump_obj_to_csv(ObjList, ObjFile),


	%?PRINT({ets:tab2list(ObjEts)}),
	%?PRINT(RR),

	pass
	.


test() ->
	{ok, All3} = file:list_dir("Map"),
	All2 = lists:foldl(fun(FileName, Acc) ->
		case filename:extension(FileName) of
			".map" -> Acc ++ [FileName];
			_ -> Acc
		end
	end, [], All3),
	All1 = All2 -- ["51.map"],
	All = All1 -- ["52.map"],
	%All_ = All -- ["vv0.map"],
	%All_ = ["32.map"],
	All_ = ["anold.map"],
	lists:foreach(fun(FileName)-> 
		dump_map(FileName)
	end, All_)
	.

normalize_korean(Bin) -> normalize_korean_1(Bin, <<>>).
normalize_korean_1(<<>>, Acc) -> Acc;
normalize_korean_1(<<C:8, R/binary>>, Acc) ->
	C2 = case C of
		Z when Z >= 32, Z =< 127 -> <<0, Z:8>>;
		X -> <<X:8>>
	end,
	normalize_korean_1(R, <<Acc/binary, C2/binary>>).


lktil_gid(1, 0, Index) -> 1 + Index;
lktil_gid(1, 4, Index) -> 26 + Index;
lktil_gid(1, 6, Index) -> 51 + Index;
lktil_gid(2, 0, Index) -> 76 + Index;
lktil_gid(2, 3, Index) -> 101 + Index;
lktil_gid(3, 0, Index) -> 126 + Index;
lktil_gid(3, 7, Index) -> 151 + Index;
lktil_gid(3, 8, Index) -> 176 + Index;
lktil_gid(4, 0, Index) -> 201 + Index;
lktil_gid(4, 6, Index) -> 226 + Index;
lktil_gid(4, 10, Index) -> 251 + Index;
lktil_gid(5, 0, Index) -> 276 + Index;
lktil_gid(5, 1, Index) -> 301 + Index;
lktil_gid(5, 4, Index) -> 326 + Index;
lktil_gid(5, 10, Index) -> 351 + Index;
lktil_gid(6, 0, Index) -> 376 + Index;
lktil_gid(7, 0, Index) -> 401 + Index;
lktil_gid(8, 0, Index) -> 426 + Index;
lktil_gid(9, 0, Index) -> 451 + Index;
lktil_gid(9, 2, Index) -> 476 + Index;
lktil_gid(9, 7, Index) -> 501 + Index;
lktil_gid(9, 8, Index) -> 526 + Index;
lktil_gid(10, 0, Index) -> 551 + Index;
lktil_gid(10, 1, Index) -> 576 + Index;
lktil_gid(10, 6, Index) -> 601 + Index;
lktil_gid(100, 0, Index) -> 626 + Index;
lktil_gid(101, 0, Index) -> 651 + Index;
lktil_gid(102, 0, Index) -> 676 + Index;
lktil_gid(103, 0, Index) -> 680 + Index;
lktil_gid(104, 0, Index) -> 684 + Index;
lktil_gid(105, 0, Index) -> 688 + Index;
lktil_gid(106, 0, Index) -> 692 + Index;
lktil_gid(107, 0, Index) -> 696 + Index;
lktil_gid(108, 0, Index) -> 700 + Index;
lktil_gid(109, 0, Index) -> 704 + Index;
lktil_gid(110, 0, Index) -> 708 + Index;
lktil_gid(111, 0, Index) -> 712 + Index;
lktil_gid(112, 0, Index) -> 716 + Index;
lktil_gid(113, 0, Index) -> 720 + Index;
lktil_gid(114, 0, Index) -> 724 + Index.
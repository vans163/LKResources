-module(extractor).

-compile(export_all).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

write_bitmap16(XSize, YSize, Bin) ->
	FileSize = byte_size(Bin) + 2 + 24 + 4 + 40,
	YSize2 = YSize * -1,
	<<16#42, 16#4D, FileSize:32/little, 0:32, 16#46:32/little,
	16#38:32/little, XSize:32/little, YSize2:32/little, 1:16/little, 
	16:16/little, 3:32/little, (byte_size(Bin)):32/little, 
	16#0B13:32/little, 16#0B13:32/little, 0:32, 0:32, 
	16#7C00:32/little, 16#03E0:32/little, 16#1F:32/little, 0:32,
	(Bin)/binary
	>>
	.

write_bitmap32(XSize, YSize, Bin) ->
	FileSize = byte_size(Bin) + 2 + 24 + 4 + 40,
	YSize2 = YSize * -1,
	<<16#42, 16#4D, FileSize:32/little, 0:32, 16#46:32/little,
	16#38:32/little, XSize:32/little, YSize2:32/little, 1:16/little, 
	16#20:16/little, 3:32/little, (byte_size(Bin)):32/little, 
	16#0B13:32/little, 16#0B13:32/little, 0:32, 0:32, 
	16#FF0000:32/little, 16#FF00:32/little, 16#FF:32/little, 0:32,
	(Bin)/binary
	>>
	.



parse_items(<<>>, _) -> pass;
parse_items(<<X:32/little, Y:32/little, _:12/binary, Rest/binary>>, Itr) ->
	Total = X*Y*2,
	<<Image:Total/binary, R/binary>> = Rest,
	Bin = write_bitmap16(X, Y, Image),
	file:write_file(string:concat("item/", 
		string:concat(integer_to_list(Itr), ".bmp")), Bin),
	parse_items(R, Itr+1)
	.

parse_ect(<<>>, _) -> pass;
parse_ect(<<X:32/little, Y:32/little, _:12/binary, Rest/binary>>, Itr) ->
	Total = X*Y*2,
	<<Image:Total/binary, R/binary>> = Rest,
	Bin = write_bitmap16(X, Y, Image),
	file:write_file(string:concat("ect/", 
		string:concat(integer_to_list(Itr), ".bmp")), Bin),
	parse_ect(R, Itr+1)
	.


parse_color_table(Bin) -> parse_color_table_1(Bin, {0, #{}}).
parse_color_table_1(Rest, {256, Map}) -> {Map, Rest};
parse_color_table_1(<<Color:4/binary, Bin/binary>>, {Idx, Map}) ->
	parse_color_table_1(Bin, {Idx+1, Map#{Idx=>Color}})
	.

transform_with_color_table(<<>>, Acc, _) -> Acc;
transform_with_color_table(<<F:8, Bin/binary>>, Acc, ColorTable) ->
	Color = maps:get(F, ColorTable),
	<<R:8,G:8,B:8,A:8>> = Color,
	Color2 = <<B:8, G:8, R:8, A:8>>,

	transform_with_color_table(Bin, <<Acc/binary, Color2/binary>>, ColorTable)
	.

parse_custom(Name, <<>>, _) -> pass;
parse_custom(Name, <<X:32/little, Y:32/little, _:12/binary, Rest/binary>>, {Itr, ColorTable}) ->
	Total = X*Y,
	<<Image:Total/binary, R/binary>> = Rest,
	Image2 = transform_with_color_table(Image, <<>>, ColorTable),
	Bin = write_bitmap32(X, Y, Image2),
	file:write_file(string:concat(Name, 
		string:concat(integer_to_list(Itr), ".bmp")), Bin),
	parse_custom(Name, R, {Itr+1, ColorTable})
	.

items() ->
	{ok, <<_:1036/binary, Bin/binary>>} = file:read_file("item.Atz"),
	parse_items(Bin, 0)
	.

bubble() ->
	{ok, <<_:12/binary, Bin/binary>>} = file:read_file("bubble.atz"),
	{ColorTable, Bin2} = parse_color_table(Bin),
	parse_custom("bubble/", Bin2, {0, ColorTable})
	.

ect() ->
	{ok, <<_:1036/binary, Bin/binary>>} = file:read_file("ect.atz"),
	parse_ect(Bin, 0)
	.

magic() ->
	{ok, <<_:12/binary, Bin/binary>>} = file:read_file("magic.Atz"),
	{ColorTable, Bin2} = parse_color_table(Bin),
	parse_custom("magic/", Bin2, {0, ColorTable})
	.

sprites_1([]) -> done;
sprites_1(["M218.Atz"|T]) -> sprites_1(T);
sprites_1([SpriteFile|T]) ->
	{ok, <<_:12/binary, Bin/binary>>} = file:read_file(string:concat("Sprite/", SpriteFile)),
	{ColorTable, Bin2} = parse_color_table(Bin),
	FinalDir = string:concat("spritedump/", filename:rootname(SpriteFile)),
	FinalDir2 = string:concat(FinalDir, "/"),
	filelib:ensure_dir(FinalDir2),
	parse_custom(FinalDir2, Bin2, {0, ColorTable}),
	sprites_1(T)
	.

sprites() ->
	{ok, All} = file:list_dir("Sprite"),
	sprites_1(lists:reverse(All))
	.


til_header_1(32, 24, <<>>, {_, _, Acc}) -> Acc;
til_header_1(32, 24, Bin, {TotalTiles, CurY, Acc}) ->
	{Bin2, TotalTiles2, CurY2, Acc2} = case CurY rem 24 of
		0 when CurY /= 0 -> 
			{Bin, TotalTiles+1, 0, Acc};
		_ -> 
			<<XRow:32/binary, R/binary>> = Bin,
			{R, TotalTiles, CurY+1, <<Acc/binary, XRow/binary>>}
	end,

	til_header_1(32, 24, Bin2, {TotalTiles2, CurY2, Acc2})
	.

til_header(<<Idx:16/little, Idx2:16/little, _:32, W:32/little, H:32/little, X:32/little, Y:32/little, R/binary>>) ->
	BodySize = W*H*X*Y*2,
	%BodySize = 32*24*2,
	JunkToRead = W*H,
	%JunkToRead = W*H-BodySize+BodySize2,
	<<Body:BodySize/binary, _:JunkToRead/binary, R2/binary>> = R,
	?PRINT({Idx, Idx2, W, H, X, Y}),
	{Idx, Idx2, W, H, X, Y, Body, R2}.


til_save(<<>>, _, _) -> ok;
til_save(<<ImageBin:(32*24*2)/binary, R/binary>>, FileName, Idx) ->
	FileName2 = FileName ++ "-" ++ integer_to_list(Idx) ++ ".bmp",

	BmpImageBin = write_bitmap16(32, 24, ImageBin),
	file:write_file(FileName2, BmpImageBin),
	til_save(R, FileName, Idx+1)
	.

til_2(_, <<>>) -> ok;
til_2(FinalDir2, Bin) ->
	{Idx, Idx2, W, H, X, Y, Image, R2} = til_header(Bin),
	%BinImage = write_bitmap16(X*W, Y*H, Image),
%	BinImage = write_bitmap16(32, 24, Image),
	FullIdx = integer_to_list(Idx) ++ "-" ++ integer_to_list(Idx2),
	Layout = "-"++ integer_to_list(W) ++ "-" ++ integer_to_list(H),
	FileName = string:concat(FinalDir2, string:concat(FullIdx, Layout)),
	til_save(Image, FileName, 0),
	til_2(FinalDir2, R2)
	.

til_1([]) -> done;
til_1([File|T]) ->
	{ok, <<_:12/binary, Bin/binary>>} = file:read_file(string:concat("Til/", File)),

	FinalDir = string:concat("tildump/", filename:rootname(File)),
	FinalDir2 = string:concat(FinalDir, "/"),
	filelib:ensure_dir(FinalDir2),

	til_2(FinalDir2, Bin),

	til_1(T)
	.
til() ->
	{ok, All} = file:list_dir("Til"),
	All1 = ["LkTil.Til"],
	til_1(All)
	.



obj_normalize_korean_1(<<>>, Acc) -> Acc;
obj_normalize_korean_1(<<C:8, R/binary>>, Acc) ->
	?PRINT({C}),
	C2 = case C of
		Z when Z >= 32, Z =< 127 -> <<0, Z:8>>;
		X -> <<X:8>>
	end,
	obj_normalize_korean_1(R, <<Acc/binary, C2/binary>>).
obj_normalize_korean(Name) -> 
	NormalName = obj_normalize_korean_1(Name, <<>>),
	BinarySplit = binary:split(NormalName, [<<0,0>>, <<0,1>>]),
	Name2 = hd(BinarySplit),
	Name2Size = byte_size(Name2),
	Name2SizeOffset = Name2Size -1,
	Name3 = case binary:last(Name2) of
		0 -> <<Name2:Name2SizeOffset/binary>>;
		_ -> Name2
	end,
	Name3Size = byte_size(Name3),

	case Name3Size rem 2 of
		1 -> <<Name3/binary, 0>>;
		0 -> Name3
	end

	.

obj_header(<<Name:8/binary, Name2:8/binary, W:32/little, H:32/little, 
		X:32/little, Y:32/little, _:32, _:32, R/binary>>) ->
	BodySize = X*Y*2,
	JunkToRead = W*H,
	<<Body:BodySize/binary, _:JunkToRead/binary, R2/binary>> = R,
	?PRINT({Name, Name2, W, H, X, Y}),
	{Name, Name2, W, H, X, Y, Body, R2}.

obj_2(_, <<>>) -> ok;
obj_2(FinalDir2, Bin) ->
	{Name, Name2, W, H, X, Y, Image, R2} = obj_header(Bin),
	BinImage = write_bitmap16(X, Y, Image),
	NameList = unicode:characters_to_list(obj_normalize_korean(Name), utf16),
	file:write_file(string:concat(FinalDir2, 
		string:concat(NameList, ".bmp")), BinImage),
	obj_2(FinalDir2, R2)
	.

obj_1([]) -> done;
obj_1([File|T]) ->
	{ok, <<_:12/binary, Bin/binary>>} = file:read_file(string:concat("Obj/", File)),

	FinalDir = string:concat("objdump/", filename:rootname(File)),
	FinalDir2 = string:concat(FinalDir, "/"),
	filelib:ensure_dir(FinalDir2),

	obj_2(FinalDir2, Bin),

	obj_1(T)
	.
obj() ->
	{ok, All} = file:list_dir("Obj"),
	All1 = ["LkObj.Obj"],
	obj_1(All)
	.


dump_all() ->
	%items(),
	%bubble(),
	%ect(),
	%magic(),
	%sprites(),
	til(),
	%obj(),
	ok
	.
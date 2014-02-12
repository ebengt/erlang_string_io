
-module(string_io_test).

-export( [main/1,all/0] ).


main(_A) ->
	all(),
	init:stop().

all() ->
	open_close(),
	open_fail(),
	get_chars(),
	get_chars_eof(),
	get_chars_get_chars(),
	get_chars_put_chars(),
	put_chars(),
	put_chars_put_chars(),
	put_chars_get_chars(),
	append_put_chars(),
	append_get_chars(),
	format(),
	append_format(),
	scan_erl_form().

append_format() ->
	{ok, IO} = string_io:open( "asd", [append] ),
	ok = io:format( IO, "~sasd", ["qwe"] ),
	"asdqweasd" = string_io:close( IO ).

append_get_chars() ->
	{ok, IO} = string_io:open( "asdqwe", [read, append] ),
	"asd" = io:get_chars( IO, '', 3 ),
	ok = io:put_chars( IO, "123" ),
	"asdqwe123" = string_io:close( IO ).

append_put_chars() ->
	{ok, IO} = string_io:open( "asd", [append] ),
	ok = io:put_chars( IO, "qwe" ),
	"asdqwe" = string_io:close( IO ).

format() ->
	{ok, IO} = string_io:open( "", [write] ),
	ok = io:format( IO, "asd~s", ["qwe"] ),
	"asdqwe" = string_io:close( IO ).

get_chars() ->
	{ok, IO} = string_io:open( "asd", [read] ),
	"asd" = io:get_chars( IO, '', 3 ),
	"asd" = string_io:close( IO ).

get_chars_eof() ->
	{ok, IO} = string_io:open( "", [read] ),
	eof = io:get_chars( IO, '', 3 ),
	"" = string_io:close( IO ).

get_chars_put_chars() ->
	{ok, IO} = string_io:open( "asdqwe", [read, write] ),
	"asd" = io:get_chars( IO, '', 3 ),
	io:put_chars( IO, "123" ),
	"asd123" = string_io:close( IO ).

get_chars_get_chars() ->
	{ok, IO} = string_io:open( "asdqwe", [read] ),
	"asd" = io:get_chars( IO, '', 3 ),
	"qwe" = io:get_chars( IO, '', 3 ),
	"asdqwe" = string_io:close( IO ).

open_close() ->
	{ok, IO} = string_io:open( "asd", [read] ),
	"asd" = string_io:close( IO ).

open_fail() ->
	{error,{mode,[gustav]}} = (catch string_io:open( "asd", [gustav] )).

put_chars() ->
	{ok, IO} = string_io:open( "", [write] ),
	io:put_chars( IO, "qwe" ),
	"qwe" = string_io:close( IO ).

put_chars_get_chars() ->
	{ok, IO} = string_io:open( "123qwe", [read, write] ),
	ok = io:put_chars( IO, "asd" ),
	"qwe" = io:get_chars( IO, '', 3 ),
	"asdqwe" = string_io:close( IO ).

put_chars_put_chars() ->
	{ok, IO} = string_io:open( "", [write] ),
	ok = io:put_chars( IO, "asd" ),
	ok = io:put_chars( IO, "qwe" ),
	"asdqwe" = string_io:close( IO ).

scan_erl_form() ->
	{ok, IO} = string_io:open( "a()->b.", [read] ),
	{ok,[{atom,1,a},{'(',1},{')',1},{'->',1},{atom,1,b},{dot,1}],1} =
		io:scan_erl_form( IO, "", 1),
	string_io:close( IO ).


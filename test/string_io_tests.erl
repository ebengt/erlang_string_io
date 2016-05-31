
-module( string_io_tests ).

-include_lib( "eunit/include/eunit.hrl" ).




append_fwrite_test() ->
	{ok, IO} = string_io:open( "asd", [append] ),
	?assertEqual( ok, io:fwrite(IO, "~sasd", ["qwe"]) ),
	?assertEqual( "asdqweasd", string_io:close(IO) ).

append_get_chars_test() ->
	{ok, IO} = string_io:open( "asdqwe", [read, append] ),
	?assertEqual( "asd", io:get_chars(IO, '', 3) ),
	?assertEqual( ok, io:put_chars(IO, "123") ),
	?assertEqual( "asdqwe123", string_io:close(IO) ).

append_put_chars_test() ->
	{ok, IO} = string_io:open( "asd", [append] ),
	?assertEqual( ok, io:put_chars(IO, "qwe") ),
	?assertEqual( "asdqwe", string_io:close(IO) ).

format_test() ->
	{ok, IO} = string_io:open( "", [write] ),
	?assertEqual( ok, io:format(IO, "asd~s", ["qwe"]) ),
	?assertEqual( "asdqwe", string_io:close(IO) ).

get_chars_test() ->
	{ok, IO} = string_io:open( "asd", [read] ),
	?assertEqual( "asd", io:get_chars(IO, '', 3) ),
	?assertEqual( "asd", string_io:close(IO) ).

get_chars_eof_test() ->
	{ok, IO} = string_io:open( "", [read] ),
	?assertEqual( eof, io:get_chars(IO, '', 3) ),
	?assertEqual( "", string_io:close(IO) ).

get_chars_put_chars_test() ->
	{ok, IO} = string_io:open( "asdqwe", [read, write] ),
	?assertEqual( "asd", io:get_chars(IO, '', 3) ),
	?assertEqual( ok, io:put_chars(IO, "123") ),
	?assertEqual( "asd123", string_io:close(IO) ).

get_chars_get_chars_test() ->
	{ok, IO} = string_io:open( "asdqwe", [read] ),
	?assertEqual( "asd",  io:get_chars(IO, '', 3) ),
	?assertEqual( "qwe", io:get_chars(IO, '', 3) ),
	?assertEqual( "asdqwe", string_io:close(IO) ).

open_close_test() ->
	{ok, IO} = string_io:open( "asd", [read] ),
	?assertEqual( "asd", string_io:close(IO) ).

open_fail_test() ->
	?assertError( {mode,[gustav]}, string_io:open("asd", [gustav]) ).

put_chars_test() ->
	{ok, IO} = string_io:open( "", [write] ),
	?assertEqual( ok, io:put_chars(IO, "qwe") ),
	?assertEqual( "qwe", string_io:close(IO) ).

put_chars_get_chars_test() ->
	{ok, IO} = string_io:open( "123qwe", [read, write] ),
	?assertEqual( ok, io:put_chars(IO, "asd") ),
	?assertEqual( "qwe", io:get_chars(IO, '', 3) ),
	?assertEqual( "asdqwe", string_io:close(IO) ).

put_chars_put_chars_test() ->
	{ok, IO} = string_io:open( "", [write] ),
	?assertEqual( ok, io:put_chars(IO, "asd") ),
	?assertEqual( ok, io:put_chars(IO, "qwe") ),
	?assertEqual( "asdqwe", string_io:close(IO) ).

scan_erl_form_test() ->
	{ok, IO} = string_io:open( "a()->b.", [read] ),
	?assertEqual( {ok,[{atom,1,a},{'(',1},{')',1},{'->',1},{atom,1,b},{dot,1}],1},
		io:scan_erl_form(IO, "", 1) ),
	string_io:close( IO ).


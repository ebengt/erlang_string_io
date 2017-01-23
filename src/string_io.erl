%%% @copyright (C) 2017 Bengt Kleberg
%%%
%%% This library is free software; you can redistribute it and/or modify
%%% it under the terms of the BSD License.
%%%
%%% You should have received a copy of the BSD
%%% License along with this library; if not, contact the author.
%%%
%%% @author Bengt Kleberg <cean.ebengt@gmail.com>
%%%
%%% @doc Allow io operations on an Erlang string.
%%%		<p>This module makes it possible to open/2 an Erlang
%%%		string, like file:open/2 for a file.
%%%		The pid() returned can be passed to the <code>io</code>
%%%		library module, for reading and/or writing into the string.</p>
%%%		<p>Useful when some module expects to read/write to a file,
%%%		but you rather not create a temporary file.
%%%		The other module must, of course, have an interface that handles an <code>io</code> pid().
%%%		As luck would have it, the one place where I needed this feature, epp_dodger:parse/1,
%%%		has such an interface.</p>
%%% @see epp_dodger
%%% @end

-module( string_io ).

-export( [
	close/1,
	open/2
] ).

-type modes()::[read | write | append].
-type mode()::'append' | 'error' | 'read' | 'read_append' | 'read_write' | 'write'.

-record( string_buffer, {
	is_readonly=false::boolean(), % is it possible to write?
	is_writeonly=false::boolean(), % is it possible to read?
	is_append=false::boolean(), % should all writes go to the end of the string?
	remaining_length=0::integer(), % length of initial string that remains to be read/written
	remaining=eof::eof | string(), % part of initial string that remains to be read/written
		% eof is used by get_until()
	accumulator=[]::string() % part of initial string that has been read/written
		% it can no longer change
} ).

%% @doc Close the string IO pid().
%% @returns the string that has been written to/read from.
%% Might be useful when writing to.
%% @throws {error, Description}
%% @end
-spec close(Server::pid()) -> string().
close( Server ) ->
	Self = erlang:self(),
	send_to( Server, {stop, Self} ),
	case receive_all(  ) of
	{stop, Self, String} -> String;
	Other -> erlang:throw( {error, Other} )
	end.


%% @doc Open a string for IO module operations.
%% The return pid() only works as long as the Erlang process that opened the string is alive.
%% @end
-spec open(String::string(), Modes::modes()) -> {ok, pid()} | {error, modes}.
open( String, Modes ) when is_list( String ) ->
	case decode( Modes ) of
	error -> {error, modes};
	Mode ->
		Caller = erlang:self(),
		Fun = fun ( ) ->
				% the new process will be informed about the ''death'' of
				% the process that created it
				erlang:monitor( process, Caller ),
				String_buffer = string_buffer( String, Mode ),
				loop( String_buffer )
			end,
		{ok, erlang:spawn( Fun )}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions


% check with string_buffer/2 if any return value is changed
-spec decode(modes()) -> mode().
decode( Modes ) ->
	case {lists:member( read, Modes ),
			lists:member( write, Modes ),
			lists:member( append, Modes )} of
	{true, false, false} -> read;
	{false, true, false} -> write;
	{true, true, false} -> read_write;
	{false, _, true} -> append;
	{true, _, true} -> read_append;
	_Other -> error
	end.


% handle io reqest of type get_chars
%	Is_readable : is the string buffer readable?
%	Length : how many chars in String_buffer
%	How_many : how many chars that should be read
%	String_buffer : #string_buffer{}
-spec get_chars(boolean(),integer(),integer(),#string_buffer{}) -> {Result,#string_buffer{}} when Result::'eof' | string() | {'error',{'mode','is_writeonly'}}.
get_chars( false, _Length, _How_many, String_buffer ) -> {{error, {mode, is_writeonly}}, String_buffer};
get_chars( true, 0, _How_many, String_buffer ) -> {eof, String_buffer};
get_chars( true, Length, How_many, String_buffer ) when (Length > How_many) ->
	Read = string_buffer_remaining( String_buffer, How_many ),
	{Read, string_buffer_after_read( String_buffer, How_many )};
get_chars( true, _Length, _How_many, String_buffer ) ->
	Remaining = string_buffer_remaining( String_buffer ),
	{Remaining, string_buffer_after_read( String_buffer, all_read )}.


% handle io reqest of type get_until
%	Is_readable : is the string buffer readable?
%	Module : module
%	Function : function
%	Arguments : arguments
%	Cont : the part of a result from previous
%		apply(Mod, Func, [Cont, String | Args])
%		that Module:Function() wants to have again,
%		to be able to build the whole result
%	String_buffer : #string_buffer{}
-spec get_until(boolean(),atom(),atom(),[any()], any(),#string_buffer{}) -> {Result,#string_buffer{}} when Result::'eof' | string() | {'error',{'mode','is_writeonly'}}| {'error',{'apply',any()}}.
get_until( false, _Module, _Function, _Arguments, _Cont, String_buffer ) ->
	{{error, {mode, is_writeonly}}, String_buffer};
get_until( true, Module, Function, Arguments, Cont, String_buffer ) ->
	String = string_buffer_remaining( String_buffer ),
	Result = (catch erlang:apply( Module, Function, [Cont, String | Arguments] )),
	get_until( Result, {Module, Function, Arguments}, String_buffer ).

get_until( {done, Result, eof}, _MFA, String_buffer ) ->
	{Result, string_buffer_after_read( String_buffer, all_read )};
get_until( {done, Result, Chars_remaining}, _MFA, String_buffer ) ->
	{Result, string_buffer_after_read( String_buffer, Chars_remaining )};
get_until( {more, Cont}, {M, F, A}, String_buffer ) ->
	% we do not have any more characters.
	% try again with empty String_buffer
	get_until( true, M, F, A, Cont, string_buffer_after_read(String_buffer, all_read) );
get_until( _Other, MFA, String_buffer ) ->
	{{error, {apply, MFA}}, String_buffer}.

% input
%	Request : tuple()
%	Acc : {Status, #string_buffer{}}
% return {Result, #string_buffer{}}
%	Result : ok | {error, Reason}
io_request( {put_chars, Chars}, {ok, String_buffer} ) ->
	put_chars( string_buffer_is_writeable(String_buffer), Chars, String_buffer );
io_request( {put_chars, unicode, Chars}, {ok, String_buffer} ) ->
	put_chars( string_buffer_is_writeable(String_buffer), Chars, String_buffer );
io_request( {put_chars, Module, Func, Args}, String_buffer ) ->
	io_request( {put_chars, catch erlang:apply( Module, Func, Args )}, String_buffer );
io_request( {put_chars, unicode, Module, Func, Args}, String_buffer ) ->
	io_request( {put_chars, unicode, catch erlang:apply( Module, Func, Args )}, String_buffer );
io_request( {get_chars, _Prompt, How_many}, {ok, String_buffer} ) ->
	get_chars( string_buffer_is_readable(String_buffer), string_buffer_remaining_length(String_buffer), How_many, String_buffer );
io_request( {get_chars, unicode, _Prompt, How_many}, {ok, String_buffer} ) ->
	get_chars( string_buffer_is_readable(String_buffer), string_buffer_remaining_length(String_buffer), How_many, String_buffer );
io_request( {get_until, _Prompt, Module, Func, Args}, {ok, String_buffer} ) ->
	get_until( string_buffer_is_readable(String_buffer), Module, Func, Args, [], String_buffer );
io_request( {get_until, unicode, _Prompt, Module, Func, Args}, {ok, String_buffer} ) ->
	get_until( string_buffer_is_readable(String_buffer), Module, Func, Args, [], String_buffer );
io_request( {requests, Requests}, {ok, String_buffer} ) ->
	Fun = fun( Request, Acc ) -> io_request( Request, Acc ) end,
	lists:foldl( Fun, {ok, String_buffer}, Requests );
io_request( Request, {_Status, String_buffer} ) ->
	{{error, {request_unknown, Request}}, String_buffer}.


loop( String_buffer ) ->
	case receive_all(  ) of
	{stop, Sender} ->
		send_to( Sender,
			{stop, Sender, string_buffer_to_string( String_buffer )} ),
		erlang:exit(normal);
	{io_request, Sender, Reply_as, Request} ->
		{Result, New} = io_request( Request, {ok, String_buffer} ),
		send_to( Sender, {io_reply, Reply_as, Result} ),
		loop( New );
	{'DOWN', _Ref, _Type, _Object, _Info} ->
		erlang:exit(normal);
	_Other ->
		loop( String_buffer )
	end.


% handle io reqest of type put chars
% transform binary to list in input
-spec put_chars(boolean(),_,#string_buffer{}) -> {'ok' | {'error',_},#string_buffer{}}.
put_chars( false, _Binary, String_buffer ) ->{{error, {mode, is_readonly}}, String_buffer};
put_chars( true, Binary, String_buffer ) when is_binary(Binary) ->
	put_chars( true, erlang:binary_to_list(Binary), String_buffer );
put_chars( true, Chars, String_buffer ) when is_list(Chars) ->
	{ok, string_buffer_after_write( String_buffer, Chars )};
put_chars( true, Error, String_buffer ) ->
	{{error, Error}, String_buffer}.


receive_all(  ) ->
	receive
	All ->
%io:fwrite( "receive_all ~w ~w~n", [All, erlang:self()]),
		All
	end.


send_to( Receiver, Data ) ->
%io:fwrite( "send_to ~w ~w ~w~n", [ Receiver, Data, erlang:self() ]),
	Receiver ! Data.


% create new #string_buffer{} with String value
% depending upon Mode the value goes in various places
-spec string_buffer(string(),mode()) -> #string_buffer{}.
string_buffer( String, Mode )->
	case Mode of
	read ->
		#string_buffer{is_readonly=true, remaining=String,
			remaining_length=erlang:length( String )};
	write ->
		% file:open() truncates file on write only. same here
		#string_buffer{is_writeonly=true};
	append ->
		#string_buffer{is_writeonly=true, is_append=true, accumulator=String};
	read_write ->
		#string_buffer{remaining=String,
			remaining_length=erlang:length( String )};
	read_append ->
		% remember to put all of remaining into accumulator at first append
		#string_buffer{is_append=true, remaining=String,
			remaining_length=erlang:length( String )}
	end.


% do the things neccessary to modify string buffer after a _read of Chars
% first all chars read
-spec string_buffer_after_read(#string_buffer{},'all_read' | non_neg_integer() | string()) -> #string_buffer{}.
string_buffer_after_read( String_buffer, all_read ) ->
	#string_buffer{remaining=Remaining} = String_buffer,
	string_buffer_modify( String_buffer, Remaining );
% second is a clause with How_many chars read
string_buffer_after_read( String_buffer, How_many )
		when is_integer( How_many ) ->
	#string_buffer{remaining=Remaining, remaining_length=Remaining_length} =
		String_buffer,
	{Read, Rest} = lists:split( How_many, Remaining ),
	string_buffer_modify( String_buffer, Read, Rest, Remaining_length-How_many );
% last is a clause with New_remaining chars,
% after read of some of the remaining chars.
string_buffer_after_read( String_buffer, New_remaining ) ->
	#string_buffer{remaining=Remaining, remaining_length=Remaining_length} =
		String_buffer,
	New_remaining_length = erlang:length( New_remaining ),
	Read_length = Remaining_length - New_remaining_length,
	Read = lists:sublist( Remaining, Read_length ),
	string_buffer_modify( String_buffer, Read, New_remaining, New_remaining_length ).


% do the things neccessary to modify string buffer after a write of Chars
-spec string_buffer_after_write(#string_buffer{},string()) -> #string_buffer{}.
string_buffer_after_write( String_buffer, Chars ) ->
	#string_buffer{is_append=Append, remaining_length=Remaining_length} =
		String_buffer,
	case {Append, Remaining_length} of
	{true, 0} ->
		% append with nothing left in remaining
		string_buffer_modify( String_buffer, Chars );
	{true, _Remaining_length} ->
		% append with some chars left in remaining
		% save what we have left in remaining first
		#string_buffer{remaining=Remaining} = String_buffer,
		string_buffer_modify( String_buffer,
			lists:append(Remaining, Chars) );
	{false, _Remaining_length} ->
		% no append. write over chars in remining
		Length = erlang:length( Chars ),
		case (Length > Remaining_length) of
		true ->
			% write over all (just ignore them)
			string_buffer_modify( String_buffer, Chars );
		false ->
			% write over some. save the rest.
			#string_buffer{remaining=Remaining} = String_buffer,
			Rest = lists:nthtail( Length, Remaining ),
			string_buffer_modify( String_buffer, Chars, Rest, Remaining_length-Length )
		end
	end.


-spec string_buffer_is_writeable(#string_buffer{}) -> boolean().
string_buffer_is_writeable( #string_buffer{is_readonly=Readonly} ) ->
	not Readonly.

-spec string_buffer_is_readable(#string_buffer{}) -> boolean().
string_buffer_is_readable( #string_buffer{is_writeonly=Writeonly} ) ->
	not Writeonly.


% this is a simpler interface to string_buffer_modify/4
% Done is the new addition to the accumulator
% if Done is eof we sanitise input
-spec string_buffer_modify(#string_buffer{},'eof' | string()) -> #string_buffer{}.
string_buffer_modify( String_buffer, eof ) ->
	string_buffer_modify( String_buffer, [], eof, 0 );
string_buffer_modify( String_buffer, Done ) ->
	string_buffer_modify( String_buffer, Done, eof, 0 ).

% modify existing string buffer. 
% Done is the new addition to the accumulator
% if Remaining is [] we sanitise input
% if Done is [] we skip append/2
-spec string_buffer_modify(#string_buffer{},string(),'eof' | string(),integer()) -> #string_buffer{}.
string_buffer_modify( String_buffer, Done, [], _Remaining_length ) ->
	string_buffer_modify( String_buffer, Done, eof, 0 );
string_buffer_modify( String_buffer, [], Remaining, Remaining_length ) ->
	String_buffer#string_buffer{
		remaining=Remaining,
		remaining_length=Remaining_length};
string_buffer_modify( String_buffer, Done, Remaining, Remaining_length ) ->
	#string_buffer{accumulator=Accumulator}=String_buffer,
	String_buffer#string_buffer{
		accumulator=lists:append( Accumulator, Done ),
		remaining=Remaining,
		remaining_length=Remaining_length}.


% get remaining part from string_buffer.
-spec string_buffer_remaining(#string_buffer{}) -> 'eof' | string().
string_buffer_remaining( #string_buffer{remaining=String} ) ->
	String.

% get How_many characters of remaining part from string_buffer.
-spec string_buffer_remaining(#string_buffer{},non_neg_integer()) -> string().
string_buffer_remaining( #string_buffer{remaining=String}, How_many ) ->
	lists:sublist( String, How_many ).

% get length of remaining part from string_buffer.
-spec string_buffer_remaining_length(#string_buffer{}) -> integer().
string_buffer_remaining_length( #string_buffer{remaining_length=Length} ) ->
	Length.


% get all strings from string_buffer as one string.
-spec string_buffer_to_string(#string_buffer{}) -> string().
string_buffer_to_string( #string_buffer{accumulator=A, remaining=R} ) ->
	Remaining = case R of
		eof -> [];
		String -> String
		end,
	lists:flatten( lists:append( A, Remaining ) ).

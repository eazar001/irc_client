:- module(irc_client_info, [
	get_irc_stream/2,
	get_irc_write_stream/2,
	get_irc_server/2,
	handle_server/2,
	connection/7,
	min_msg_len/2,
	assert_irc_stream/2,
	assert_irc_write_stream/2,
	assert_connection/7,
	assert_irc_server/2,
	assert_min_msg_len/2,
	assert_handle_server/2
]).

:- dynamic get_irc_stream/2.
:- dynamic get_irc_write_stream/2.
:- dynamic connection/7.
:- dynamic get_irc_server/2.
:- dynamic min_msg_len/2.
:- dynamic handle_server/2.

assert_irc_stream(Id, Stream) :-
	asserta(get_irc_stream(Id, Stream)).

assert_irc_write_stream(Id, Stream) :-
	asserta(get_irc_write_stream(Id, Stream)).

assert_connection(Id, Nick, Password, Channels, Hostname, Servername, Realname) :-
	asserta(connection(Id, Nick, Password, Channels, Hostname, Servername, Realname)).

assert_irc_server(Id, Server) :-
	asserta(get_irc_server(Id, Server)).

assert_min_msg_len(Id, Length) :-
	asserta(min_msg_len(Id, Length)).

assert_handle_server(Id, Goals) :-
	asserta(handle_server(Id, Goals)).

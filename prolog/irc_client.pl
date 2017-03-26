:- module(irc_client, [
	assert_handlers/2,
	connect/6,
	disconnect/1
]).


:- use_module(library(socket)).
:- use_module(library(func)).
:- reexport(irc_client_info).

:- reexport(irc_client_parser, [
	prefix_id/2,
	prefix_id/4
]).

:- reexport(irc_client_dispatch, [
	send_msg/2,
	send_msg/3,
	send_msg/4
]).

:- reexport(irc_client_utilities, [
	priv_msg/3,
	priv_msg/4,
	priv_msg_rest/4,
	priv_msg_rest/5,
	priv_msg_paragraph/4
]).


/** <module> irc_client main interface
This is the main interface to irc_client. Connections and their respective
information sets are maintained here.

@author Ebrahim Azarisooreh
@license MIT

@tbd Add some thread abstractions for establishing connections
@tbd Link more commands from operator.pl to dispatch.pl
@tbd More documentation of pack useage is needed, perhaps even wiki
@tbd Add direct support for CTCP actions
*/


%--------------------------------------------------------------------------------%
% Connection Details
%--------------------------------------------------------------------------------%


%% connect(+Host, +Port, +Pass, +Nick, +Names, +Chans) is nondet.
%
%  Open socket on host, port, nick, user, with the specified password, names,
%  and channels to be joined.
%
%  @arg Host An atom that represents the address of the IRC host to connect to
%  @arg Port A positive integer that represents the port to connect to
%  @arg Pass An atom that represents the password of the connection
%  @arg Nick an atom that represents the nickname of the user's connection
%  @arg Names A list containing three three atoms of the format:
%  =|[Hostname, Servername, Realname]|=
%  @arg Chans A list of atoms, each atom representing a channel to connect to

connect(Host, Port, Pass, Nick, Names, Chans) :-
	setup_call_cleanup(
		setup_connection(Me, Host, Port, Pass, Nick, Names, Chans),
		start,
		disconnect(Me)
	).

%% setup_connection(-Id, +Host, +Port, +Nick, +Names, +Chans) is semidet.
%
%  Run connection setup routine, and unify Id with the thread Id of the connection.

setup_connection(Me, Host, Port, Pass, Nick, Names, Chans) :-
	thread_self(Me),
	init_structs(Pass, Nick, Names, Chans),
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port, Stream),
	set_stream(Stream, timeout(300)),
	stream_pair(Stream, _Read, Write),
	assert_irc_write_stream(Me, Write),
	set_stream(Write, encoding(utf8)),
	assert_irc_stream(Me, Stream).

%% start is semidet.
%
%  Register user and join irc server. Afterwards, start reading input from the server.

start :-
	register_and_join,
	read_server_loop.

%% register_and_join is semidet.
%
%  Present credentials and register user on the irc server.
register_and_join :-
	thread_self(Me),
	maplist(send_msg(Me), [pass, user, nick, join]).


%% init_structs is det.
%
%  Assert the 'connection' structure at the top level so that access to important
%  user information is available at the top level throughout the program.

init_structs(P_, N_, Names, Chans_) :-
	thread_self(Me),
	Names = [Hn_, Sn_, Rn_],
	maplist(atom_string, Chans_, Chans),
	maplist(atom_string, [N_, P_, Hn_, Sn_, Rn_], Strs),
	Strs = [N, P, Hn, Sn, Rn],
	assert_connection(Me, N, P, Chans, Hn, Sn, Rn).


%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop is nondet.
%
%  Read the server output one line at a time. Each line will be sent directly
%  to a predicate that is responsible for handling the output that it receives.
%  The program will terminate successfully if EOF is reached. Reply is a list
%  of codes that represents one line of a relayed IRC server message.

read_server_loop :-
	thread_self(Me),
	get_irc_stream(Me, Stream),
	repeat,
		read_server(Stream),
		!.

%% read_server(+Stream) is nondet.
%
%  Translate server line to codes. If the codes are equivalent to EOF then succeed
%  and go back to the main loop for termination. If not then then display the
%  contents of the server message and process the reply.

read_server(Stream) :-
	read_line_to_codes(Stream, Reply),
	(	Reply = end_of_file
	->	true
	;	read_server_handle(Reply),
		fail
	).


%% read_server_handle(+Reply) is det.
%
%  Process the current line relayed by the IRC server via the user's asserted
%  handlers (if any). Other essential non-user related actions are called
%  from this point as well.

read_server_handle(Reply) :-
	thread_self(Me),
	parse_line(Reply, Msg),
	thread_create(run_det(process_server(Me, Msg)), _Id, [detached(true)]).


%% process_server(+Me, +Msg) is nondet.
%
%  All processing of server message will be handled here. Pings will be handled by
%  responding with a pong to keep the connection alive. If the message is "001"
%  or a server "welcome", then a successful connection to a server will be
%  assumed. In this case, all instances of get_irc_server/1 will be retracted,
%  and the new server will be asserted for use.
%
%  @arg Msg A server line parsed into a compound term for IRC message consumption

process_server(Me, msg("PING", [], O)) :-
	% handle pings
	string_codes(Origin, O),
	send_msg(Me, pong, Origin).
process_server(Me, msg(Server, "001", _, _)) :-
	% get irc server and assert info
	retractall(get_irc_server(Me, _)),
	assert_irc_server(Me, Server),
	% request own user info
	connection(Me, Nick, _, _, _, _, _),
	send_msg(Me, who, atom_string $ Nick).
process_server(Me, msg(_Server, "352", Params, _)) :-
	% get own host and nick info
	(	min_msg_len(Me, _)
	->	true
	;	connection(Me, N, _, _, _, _, _),
		atom_string(N, Nick),
		Params = [_Asker, _Chan, H, Host, _, Nick|_],
		% calculate the minimum length for a private message and assert info
		format(string(Template), ':~s!~s@~s PRIVMSG :\r\n ', [Nick,H,Host]),
		assert_min_msg_len(Me, string_length $ Template),
		catch(
			thread_create(ping_loop(Nick, 180), _Status, [detached(true), alias(checker)]),
			_Any,
			true
		)
	).
process_server(Me, Msg) :-
	% run user's custom goals
	handle_server(Me, [Goal|Goals]),
	maplist(process_msg(Me-Msg), [Goal|Goals]).

ping_loop(Nick, Timeout) :-
	repeat,
		send_msg(irc, ping, Nick),
		sleep(Timeout),
		fail.

%% assert_handlers(+Id, +Handlers) is det.
%
%  Assert handlers at the toplevel, where Handlers is a potentially empty list
%  of goals to be called as irc messages come in. This is meant to be used as a
%  directive in the user's program; however, there are plenty of cases where
%  it's acceptable to call this as a normal goal during runtime.
%
%  @arg Id The identity or alias of the connection; this should match the alias
%  of the thread started to initiate the connection via connect/6
%  @arg Handlers A list of goals that are made available to irc_client. All
%  goals should have an arity of 1, and deal with processing an Id-Msg pair that
%  an IRC server relays to the client
%
%  @throws instantiation_error if Id is not ground

assert_handlers(Id, Handlers) :-
	must_be(ground, Id),
	retractall(handle_server(Id,_)),
	assert_handle_server(Id, Handlers).


:- meta_predicate process_msg(+, 1).
process_msg(Msg, Goal) :-
	call(Goal, Msg).


%--------------------------------------------------------------------------------%
% Cleanup/Termination
%--------------------------------------------------------------------------------%


%% disconnect(+Id) is semidet.
%
%  Issue a disconnect (quit) command, and clean up all unneeded information from
%  the top level. This process will only be handled for the connection that
%  contains the alias Id.

disconnect(Me) :-
	catch(send_msg(Me, quit), _E0, true),
	catch(thread_signal(checker, throw(abort)), _E1, true),
	info_cleanup(Me),
	retractall(get_irc_stream(Me,Stream)),
	(	catch(stream_property(Stream, _), _Error, fail)
	->	close(Stream)
	;	true
	).


%% info_cleanup(+Id) is det.
%
%  Retract all obsolete facts from info module, under the connection alias Id.
info_cleanup(Me) :-
	maplist(
		retractall,
		[
			connection(Me,_,_,_,_,_,_),
			min_msg_len(Me,_),
			handle_server(Me,_),
			get_irc_server(Me,_),
			get_irc_write_stream(Me,_)
		]
	).

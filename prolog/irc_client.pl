
:- module(irc_client,
     [ assert_handlers/2
      ,connect/6
      ,disconnect/1 ]).


:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(info).

:- reexport(info).

:- use_module(parser).

:- reexport(parser,
     [ prefix_id/2
      ,prefix_id/4 ]).

:- use_module(dispatch).

:- reexport(dispatch,
     [ send_msg/2
      ,send_msg/3
      ,send_msg/4 ]).

:- use_module(utilities).

:- reexport(utilities,
     [ priv_msg/3
      ,priv_msg/4
      ,priv_msg_rest/4
      ,priv_msg_rest/5
      ,priv_msg_paragraph/4 ]).


/** <module> irc_client main interface
This is the main interface to irc_client. Connections and their respective
information sets are maintained here.

@author Ebrahim Azarisooreh
@license MIT

@tbd Add some thread abstractions for establishing connections
@tbd Link more commands from operator.pl to dispatch.pl
@tbd More documentation of pack useage is needed

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
    (  thread_self(Me),
       asserta(info:c_specs(Me,Host,Port,Pass,Nick,Names,Chans)),
       init_structs(Pass, Nick, Names, Chans),
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       stream_pair(Stream, _Read, Write),
       asserta(info:get_irc_write_stream(Me, Write)),
       set_stream(Write, encoding(utf8)),
       asserta(info:get_irc_stream(Me, Stream))
    ),
    (  register_and_join,
       read_server_loop(_Reply)
    ),
    disconnect(Me)
  ).


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
  Connection =.. [connection, Me, N, P, Chans, Hn, Sn, Rn],
  asserta(info:Connection).


%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop(-Reply) is nondet.
%
%  Read the server output one line at a time. Each line will be sent directly
%  to a predicate that is responsible for handling the output that it receives.
%  The program will terminate successfully if EOF is reached. Reply is a list
%  of codes that represents one line of a relayed IRC server message.

read_server_loop(Reply) :-
  thread_self(Me),
  get_irc_stream(Me, Stream),
  init_timer(_TQ),
  asserta(info:known(Me, tq)),
  repeat,
    read_server(Reply, Stream), !.


%% read_server(-Reply, +Stream) is nondet.
%
%  Translate server line to codes. If the codes are equivalent to EOF then succeed
%  and go back to the main loop for termination. If not then then display the
%  contents of the server message and process the reply.

read_server(Reply, Stream) :-
  read_line_to_codes(Stream, Reply),
  (  Reply = end_of_file
  -> true
  ;  read_server_handle(Reply),
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

process_server(Me, Msg) :-
  timer(Me, T),
  thread_send_message(T, true),
  (  % Handle pings
     Msg = msg("PING", [], O),
     string_codes(Origin, O),
     send_msg(Me, pong, Origin)
  ;  % Get irc server and assert info
     Msg = msg(Server, "001", _, _),
     retractall(get_irc_server(Me,_)),
     asserta(info:get_irc_server(Me,Server)),
     asserta(info:known(Me,irc_server)),
     % Request own user info
     connection(Me,Nick,_,_,_,_,_),
     send_msg(Me, who, atom_string $ Nick)
  ;  % Get own host and nick info
     Msg = msg(_Server, "352", Params, _),
     connection(Me,N,_,_,_,_,_),
     atom_string(N, Nick),
     Params = [_Asker, _Chan, H, Host, _, Nick| _],
     % Calculate the minimum length for a private message and assert info
     format(string(Template), ':~s!~s@~s PRIVMSG :\r\n ', [Nick,H,Host]),
     asserta(info:min_msg_len(Me, string_length $ Template))
  ;  handle_server(Me, Goals),
     Goals \= [],
     maplist(process_msg(Me-Msg), Goals)
  ).


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
  asserta(info:handle_server(Id, Handlers)).


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
  atom_concat(Me, '_ping_checker', Ping),
  info_cleanup(Me),
  catch(thread_signal(Ping, throw(abort)), _E1, true),
  catch(thread_join(Ping, _Status), _E2, true),
  retractall(get_irc_stream(Me,Stream)),
  retractall(timer(Me,Q)),
  (  message_queue_property(Q, alias(_))
  -> message_queue_destroy(Q)
  ;  true
  ),
  (  catch(stream_property(Stream, _), _Error, fail)
  -> close(Stream)
  ;  true
  ).


%% info_cleanup(+Id) is det.
%
%  Retract all obsolete facts from info module, under the connection alias Id.
info_cleanup(Me) :-
  maplist(retractall,
    [ connection(Me,_,_,_,_,_,_)
     ,c_specs(Me,_,_,_,_,_)
     ,min_msg_len(Me,_)
     ,handle_server(Me,_)
     ,get_irc_server(Me,_)
     ,get_irc_write_stream(Me,_)
     ,known(Me,_) ]).


%--------------------------------------------------------------------------------%
% Connectivity/Timing/Handling
%--------------------------------------------------------------------------------%


%% init_timer(-Id) is semidet.
%
%  Initialize a message queue that stores one thread which acts as a timer that
%  checks connectivity of the client when established interval has passed.

init_timer(Id) :-
  thread_self(Me),
  atom_concat(Me, '_tq', Timer),
  atom_concat(Me, '_ping_checker', Checker),
  asserta(info:timer(Me,Timer)),
  message_queue_create(Id, [alias(Timer)]),
  thread_create(check_pings(Id), _, [alias(Checker)]).


%% check_pings(+Id) is failure.
%
%  If Limit seconds has passed, then signal the connection thread to abort. If a
%  ping has been detected and the corresponding message is sent before the time
%  limit expires, then the goal will succeed and so will the rest of the
%  predicate. The thread will then return to its queue, reset its timer, and wait
%  for another ping signal.

check_pings(Id) :-
  repeat,
    (  thread_get_message(Id, Goal, [timeout(300)])
    -> Goal
    ;  throw(abort)
    ),
    fail.


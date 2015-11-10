

:- module(irc_client,
     [ connect/5
      ,disconnect/0 ]).


:- use_module(info).
:- use_module(parser).
:- use_module(dispatch).
:- use_module(utilities).
:- use_module(library(socket)).
:- use_module(library(func)).

:- reexport(dispatch,
     [ send_msg/1
      ,send_msg/2
      ,send_msg/3 ]).


%--------------------------------------------------------------------------------%
% Connection Details
%--------------------------------------------------------------------------------%


%% connect is nondet.
%
%  Open socket on host, port, nick, user, hostname, and servername that will all
%  be specified in the bot_config module. The socket stream that is established
%  will be asserted at the top level for access from anywhere in the program.

connect(Host, Port, Pass, Nick, Chans) :-
  asserta(info:c_specs(Host,Port,Pass,Nick,Chans)),
  setup_call_cleanup(
    (  init_structs(Pass, Nick, Chans),
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       stream_pair(Stream, _Read, Write),
       asserta(info:get_irc_write_stream(Write)),
       set_stream(Write, encoding(utf8)),
       asserta(info:get_tcp_socket(Socket)),
       asserta(info:get_irc_stream(Stream)),
       register_and_join
    ),
    read_server_loop(_Reply),
    reconnect
  ).


%% register_and_join is semidet.
%
%  Present credentials and register user on the irc server.
register_and_join :-
  maplist(send_msg, [pass, user, nick, join]).


%% init_structs is det.
%
%  Assert the 'connection' structure at the top level so that access to important
%  user information is available at the top level throughout the program. All of
%  this information should be specified in the bot_config module.

init_structs(P_, N_, Chans_) :-
  maplist(atom_string, Chans_, Chans),
  %bot_hostname(Hn_),
  %bot_servername(Sn_),
  %bot_realname(Rn_),
  maplist(atom_string, [N_, P_, host, server, name], Strs),
  Strs = [N, P, Hn, Sn, Rn],
  Connection =.. [connection, N, P, Chans, Hn, Sn, Rn],
  asserta(info:Connection).


%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop(-Reply:codes) is nondet.
%
%  Read the server output one line at a time. Each line will be sent directly
%  to a predicate that is responsible for handling the output that it receives.
%  The program will terminate successfully if EOF is reached.

read_server_loop(Reply) :-
  get_irc_stream(Stream),
  init_timer(_TQ),
  asserta(info:known(tq)),
  repeat,
    read_server(Reply, Stream), !.


%% read_server(-Reply:codes, +Stream) is nondet.
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


%% read_server_handle(+Reply:codes) is det.
%
%  Concurrently process server lines via loaded extensions and output the server
%  line to stdout for debugging.


read_server_handle(Reply) :-
  thread_self(Me),
  G = maplist(predicate_property(P),
        [thread_local, imported_from(info)]),
  findall(P, call(G), Ps),
  maplist(ignore, Ps),
  parse_line(Reply, Msg),
  thread_create(run_det(process_server(Me, Msg, Ps)), _Id, [detached(true)]),
  format('~s~n', [Reply]).


%% process_server(+Me, +Msg:compound, +List) is nondet.
%
%  All processing of server message will be handled here. Pings will be handled by
%  responding with a pong to keep the connection alive. If the message is "001"
%  or a server "welcome", then a successful connection to a server will be
%  assumed. In this case, all instances of get_irc_server/1 will be retracted,
%  and the new server will be asserted for use. It is important that this is
%  serialized with respect to process_msg/1 so as to avoid race conditions.
%  Anything else will be processed as an incoming message.

process_server(Me, Msg, List) :-
  maplist(asserta, List),
  timer(T),
  thread_send_message(T, true),
  (  % Handle pings
     Msg = msg("PING", [], O),
     string_codes(Origin, O),
     send_msg(pong, Origin)
  ;  % Get irc server and assert info
     Msg = msg(Server, "001", _, _),
     (  get_irc_server(Me, Server)
     -> true
     ;  asserta(info:get_irc_server(Me, Server))
     ),
     asserta(info:known(irc_server)),
     % Request own user info
     connection(Nick,_,_,_,_,_),
     send_msg(who, atom_string $ Nick)
  ;  % Get own host and nick info
     Msg = msg(_Server, "352", Params, _),
     connection(N,_,_,_,_,_),
     atom_string(N, Nick),
     Params = [_Asker, _Chan, H, Host, _, Nick| _],
     % Calculate the minimum length for a private message and assert info
     format(string(Template), ':~s!~s@~s PRIVMSG :\r\n ', [Nick,H,Host]),
     asserta(info:min_msg_len(Me, string_length $ Template))
  ).


%--------------------------------------------------------------------------------%
% Cleanup/Termination
%--------------------------------------------------------------------------------%


%% reconnect is semidet.
%
%  Disconnect from the server, run cleanup routine, and attempt to reconnect.
reconnect :-
  c_specs(Host, Port, Pass, Nick, Chans),
  disconnect,
  repeat,
    writeln("Connection lost, attempting to reconnect ..."),
    (  catch(connect(Host, Port, Pass, Nick, Chans), _E, fail)
    -> !
    ;  sleep(30),
       fail
    ).


%% disconnect is semidet.
%
%  Clean up top level information access structures, issue a disconnect command
%  to the irc server, close the socket stream pair, and attempt to reconnect.

disconnect :-
  thread_self(Me),
  atom_concat(Me, '_ping_checker', Ping),
  get_irc_stream(Stream),
  send_msg(quit),
  timer(T),
  message_queue_destroy(T),
  thread_join(Ping, _),
  close(Stream),
  info_cleanup.


%% info_cleanup is det.
%
%  Retract all obsolete facts from info module.
info_cleanup :-
  thread_self(Me),
  (  get_tcp_socket(Socket)
  -> ignore(catch(tcp_close_socket(Socket), _E, fail))
  ;  true
  ),
  maplist(retractall,
    [ get_irc_stream(_)
     ,get_tcp_socket(_)
     ,connection(_,_,_,_,_,_)
     ,min_msg_len(Me,_)
     ,get_irc_server(Me,_)
     ,get_irc_write_stream(_)
     ,known(_) ]).


%--------------------------------------------------------------------------------%
% Connectivity/Timing/Handling
%--------------------------------------------------------------------------------%


%% init_timer(-Id:atom) is semidet.
%
%  Initialize a message queue that stores one thread which acts as a timer that
%  checks connectivity of the bot when established interval has passed.

init_timer(Id) :-
  thread_self(Self),
  atom_concat(Self, '_tq', Timer),
  atom_concat(Self, '_ping_checker', Checker),
  asserta(info:timer(Timer)),
  message_queue_create(Id, [alias(Timer)]),
  thread_create(check_pings(Id), _, [alias(Checker)]).


%% check_pings(+Id:atom) is failure.
%
%  If Limit seconds has passed, then signal the connection thread to abort. If a
%  ping has been detected and the corresponding message is sent before the time
%  limit expires, then the goal will succeed and so will the rest of the
%  predicate. The thread will then return to its queue, reset its timer, and wait
%  for another ping signal.

check_pings(Id) :-
  thread_self(Self),
  repeat,
    (  thread_get_message(Id, Goal, [timeout(300)])
    -> Goal
    ;  thread_signal(Self, throw(abort))
    ),
    fail.


%% restart is semidet.
%
%  Signals the main connection thread with an exception that will trigger the
%  the main connection predicate to disconnect, cleanup, and reconnect to the
%  server.

restart :-
  thread_self(Self),
  thread_signal(Self, throw(abort)).



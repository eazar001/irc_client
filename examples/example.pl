#!/usr/bin/swipl

:- use_module(library(irc_client)).

:- initialization run.

% assert_handlers/2 is used for asserting event handlers. These event handlers
% must have an arity of 1. The first argument is the Id of the connection. The
% Id of the connection is established as the alias of the thread of the actual
% connection. The second argument is a list of event goals to be be applied to
% incoming server messages. The connection Id _must_ be ground.

:- assert_handlers(ct1, [output, echo_connected]).
:- assert_handlers(ct2, [output, echo_connected]).


% The next two predicates below (output/1 and echo_connected/1) are the actual
% implementations of the event goals that process the incoming server messages.
% As mentioned in the README of this pack, this is a very low level protocol
% library. Hence, not even outputting IRC messages is included by default!!!
% However, this is easy enough to implement, as shown by the output/1 predicate
% below. It is a mere 3 line event handler. The other handler, echo_connected/1,
% simply echoes to the user terminal that a successful connection to a server
% has been established when it reaches IRC message code 352.

output(Id-Msg) :-
  % This is the basic format of an incoming server message. It is a compound
  % term of the format: msg(Server, Code, Params, Text), or in other words,
  % msg(string, string, list(string), string)
  % The third argument is a variable list of strings that represent specific
  % IRC parameters
  Msg = msg(Server, Code, Params, Text),
  format("~s: ~s ~s ~w ~s~n", [Id, Server, Code, Params, Text]).

echo_connected(Id-Msg) :-
  % Every incoming message relayed by the server takes the form of a pair ...
  % Id-Msg. The Id is the aliased Id of the connection when the thread for it
  % is created. The Msg part of the pair is the actual message itself.
  Msg = msg(Server, "352", _, _),
  format("Successfully connected to ~s on ~s~n", [Server,Id]).


run :-
  % Here we create two threads that attempt to connect to the same server
  % the alias of each thread represents the connection Id. One can conceivably
  % connect to many servers each with many channels, under different user names.
  % We mearly attach the threads to separate consoles for demonstrative purposes.
  % (attaching to a debug console is not required)
  thread_create(connect, Connect, [detached(true),alias(ct1)]),
  thread_signal(Connect, attach_console),
  sleep(0.1),
  thread_create(connect_2, Connect2, [detached(true),alias(ct2)]),
  thread_signal(Connect2, attach_console).

% Each connection uses the connect/6 predicate.
% connect(IRC Server, port, password, username, names, channels) or in other
% words, connect(atom, integer, atom, atom, list(atom), list(atom).

% ... and this is the basic idea of irc_client. It's not meant to be a framework
% for bots, or a complex system to hold your hands. It is a light abstraction
% over protocols that can conceivably allow you to make _any_ type of IRC client
% software. For more information on the protocol API, see the README.

connect :-
  connect('chat.freenode.net', 6667, pass, userbot, [host,server,real], ['##prolog','#coq']).

connect_2 :-
  connect('chat.freenode.net', 6667, pass, userbot2, [host,server,real], ['#math']).

connect_3 :-
  connect('irc.choopa.net', 6667, pass, userbot3, [host,server,real], ['#iphone']).

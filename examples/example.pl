#!/usr/bin/swipl

:- use_module(library(irc_client)).

:- initialization run.

:- assert_handlers([hello, write_out]).

hello(_) :-
  writeln(say_hello).

write_out(Msg) :-
  writeln(Msg).

run :-
  thread_create(connect, Connect, [detached(true),alias(ct1)]),
  thread_signal(Connect, attach_console),
  sleep(0.1),
  thread_create(connect_2, Connect2, [detached(true),alias(ct2)]),
  thread_signal(Connect2, attach_console).


connect :-
  connect('chat.freenode.net', 6667, pass, userbot, ['##prolog','#coq']).

connect_2 :-
  connect('chat.freenode.net', 6667, pass, userbot2, ['#erlang']).

connect_3 :-
  connect('irc.choopa.net', 6667, pass, userbot3, ['#iphone']).

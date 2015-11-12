
%% Utilities module for anything that might be generally useful for Yesbot


:- module(utilities,
     [ run_det/1
      ,priv_msg/3
      ,priv_msg/4
      ,priv_msg_rest/4
      ,priv_msg_rest/5
      ,priv_msg_paragraph/4 ]).


:- use_module(info).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(library(dcg/basics)).
:- use_module(library(predicate_options)).
:- use_module(library(list_util)).


%--------------------------------------------------------------------------------%
% Concurrency
%--------------------------------------------------------------------------------%


%% run_det(:Goal) is det.
%
%  Find all the solutions to a goal in order to precipitate the result as a
%  deterministic result. Used here for deterministically evaluating a possibly
%  nondet concurrently.

:- meta_predicate run_det(0).
run_det(Goal) :-
  ignore((Goal, fail)).


%--------------------------------------------------------------------------------%
% Sending Messages
%--------------------------------------------------------------------------------%


:- predicate_options(priv_msg/3, 3,
     [ auto_nl(boolean)
      ,at_most(nonneg)
      ,encoding(encoding) ]).


%% priv_message(+Text:string, +Recipient:string) is det.
%
%  This is a convenience predicate for sending private messages to recipients on
%  IRC channels. If there are any newlines they will be converted into individual
%  messages (i.e. paragraph style handling).

priv_msg(Id, Text, Recipient) :-
  priv_msg_rest(Id, Text, Recipient, _, [auto_nl(true)]).


priv_msg(Id, Text, Recipient, Options) :-
  priv_msg_rest(Id, Text, Recipient, _, Options).


priv_msg_rest(Id, Text, Recipient, Rest) :-
  priv_msg_rest(Id, Text, Recipient, Rest, [auto_nl(true)]).


priv_msg_rest(Id, Text, Recipient, Rest, Options) :-
  Send_msg = (\Msg^send_msg(Id, priv_msg, Msg, Recipient)),
  option(encoding(Encoding), Options, utf8),
  get_irc_write_stream(Id, Stream),
  set_stream(Stream, encoding(Encoding)),
  priv_msg_paragraph(Id, Text, Recipient, Paragraph),
  (  option(auto_nl(true), Options, true)
  -> option(at_most(Limit), Options, length $ Paragraph),  % auto-nl
     split_at(Limit, Paragraph, P, Rest),
     maplist(Send_msg, P)
  ;  maplist(Send_msg, Paragraph) % no auto-nl
  ),
  (  stream_property(Stream, encoding(utf8))
  -> true
  ;  set_stream(Stream, encoding(utf8))
  ).


priv_msg_paragraph(Id, Text, Recipient, Paragraph) :-
  min_msg_len(Id, Min),
  string_length(Recipient, N0),
  N is N0 + Min,
  Length is 508 - N,
  insert_nl_at(Length, string_codes $ Text, Formatted),
  Paragraph = exclude(\Str^(Str="", ! ; Str = "\r")) $
    split_string(Formatted, "\n") $ "".


insert_nl_at(Num, Codes, Formatted) :-
  insert_nl_at(Codes, F, Num, Num),
  string_codes(Formatted, F).

insert_nl_at([], [], _, _).
insert_nl_at([X|Xs], [X|Ys], N, N0) :-
  (  X = 10
  -> insert_nl_at(Xs, Ys, N, N), !
  ;  N0 > 1, !,
     N1 is N0-1,
     insert_nl_at(Xs, Ys, N, N1)
  ).

insert_nl_at([X|Xs], [X,10|Ys], N, 1) :-
  insert_nl_at(Xs, Ys, N, N).



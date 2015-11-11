
%% Message dispatching module
%
% This is a switchboard for routing message types to the correct message
% templates. Once the message template and respective substitution list is
% unified with the type, the process is consummated by dispatching the
% message through the stream.

:- module(dispatch,
     [ send_msg/1
      ,send_msg/2
      ,send_msg/3 ]).


:- use_module(info).
:- use_module(operator).


%--------------------------------------------------------------------------------%
% Command Routing
%--------------------------------------------------------------------------------%


% FIXME: Not all message types from operator are implemented yet.

%% return_server(-Server:string) is det.
%
%  If the server is known get the value from the core. If not, then the server is
%  'unknown'.

return_server(Server) :-
  thread_self(Me),
  (  known(Me,irc_server)
  -> get_irc_server(Me, Server)
  ;  Server = unknown
  ).


%% cmd_params(+Type, -N) is semidet.
%
%  True if N is the number of paramteres in Type's template.
cmd_params(Type, N) :-
  cmd(Type, Template),
  split_string(Template, "~", "\r~n", [_|Params]),
  length(Params, N).


%% send_msg(+Type:atom) is semidet.
%
%  Send a message of Type.
send_msg(Type) :-
  cmd(Type, Msg),
  thread_self(Me),
  get_irc_stream(Me, Stream),
  cmd_params(Type, 0), !, % green, no further matches
  write(Stream, Msg),
  flush_output(Stream),
  timer(Me, T),
  thread_send_message(T, true).

% This clause will deal with deal with message types that are possibly
% timer-independent
send_msg(Type) :-
  cmd(Type, Msg),
  thread_self(Me),
  get_irc_stream(Me, Stream),
  connection(Me, Nick, Pass, Chans, HostName, ServerName, RealName),
  (  Type = pass,
     format(Stream, Msg, [Pass])
  ;  Type = user,
     format(Stream, Msg, [Nick, HostName, ServerName, RealName])
  ;  Type = nick,
     format(Stream, Msg, [Nick])
  ;  Type = join,
     maplist(format(Stream, Msg), Chans)
  ),
  flush_output(Stream),
  (  known(Me, tq)
  -> timer(Me, T),
     thread_send_message(T, true)
  ;  true
  ).


%% send_msg(+Type:atom, +Param:string) is semidet.
%
%  Send message of Type with attention to some parameter Param.
send_msg(Type, Param) :-
  cmd(Type, Msg),
  cmd_params(Type, 1),
  (  Type = pong
  -> dbg(pong, Debug),
     format(Debug, [Param])
  ;  true
  ), !, % green, no further matches
  thread_self(Me),
  get_irc_stream(Me, Stream),
  format(Stream, Msg, [Param]),
  flush_output(Stream),
  timer(Me, T),
  thread_send_message(T, true).


%% send_msg(+Type:atom, +Str:text, +Target:string) is semidet.
%
%  Send a Str of Type to a specified Target.
send_msg(Type, Str, Target) :-
  cmd(Type, Msg),
  cmd_params(Type, 2),
  \+member(Type, [kick, invite]), !, % green, no further matches
  thread_self(Me),
  get_irc_stream(Me, Stream),
  format(Stream, Msg, [Target, Str]),
  flush_output(Stream),
  timer(Me, T),
  thread_send_message(T, true).

% Send a message of Type with respect to Chan, to the Target.
send_msg(Type, Chan, Target) :-
  cmd(Type, Msg),
  thread_self(Me),
  get_irc_stream(Me, Stream),
  (  Type = kick,
     format(Stream, Msg, [Chan, Target])
  ;  Type = invite,
     format(Stream, Msg, [Target, Chan])
  ), !,
  flush_output(Stream),
  timer(Me, T),
  thread_send_message(T, true).



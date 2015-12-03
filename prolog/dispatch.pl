
:- module(dispatch,
     [ send_msg/2
      ,send_msg/3
      ,send_msg/4 ]).


:- use_module(info).
:- use_module(operator).

/** <module> Message dispatching

This is a switchboard for routing message types to the correct message
templates. Once the message template and respective substitution list is
unified with the type, the process is consummated by dispatching the
message through the stream.

@tbd Implement more message types
*/

  
%--------------------------------------------------------------------------------%
% Command Routing
%--------------------------------------------------------------------------------%


%% cmd_params(+Type, -N) is semidet.
%
%  True if N is the number of parameters in Type's template.
cmd_params(Type, N) :-
  cmd(Type, Template),
  split_string(Template, "~", "\r~n", [_|Params]),
  length(Params, N).


%% send_msg(+Id, +Type) is semidet.
%
%  Send a Type of message from connection Id.
send_msg(Me, Type) :-
  cmd(Type, Msg),
  get_irc_stream(Me, Stream),
  cmd_params(Type, 0), !, % green, no further matches
  write(Stream, Msg),
  flush_output(Stream).

% This clause will deal with deal with message types that are possibly
% timer-independent
send_msg(Me, Type) :-
  cmd(Type, Msg),
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
  flush_output(Stream).


%% send_msg(+Id, +Type, +Param) is semidet.
%
%  Send a Type of message with attention to some Param from connection Id.
send_msg(Me, Type, Param) :-
  cmd(Type, Msg),
  cmd_params(Type, 1), !, % green, no further matches
  get_irc_stream(Me, Stream),
  format(Stream, Msg, [Param]),
  flush_output(Stream).


%% send_msg(+Id, +Type, +Str, +Target) is semidet.
%
%  Send a Type of message with attention to Str directed at a Target from
%  connection Id.
send_msg(Me, Type, Str, Target) :-
  cmd(Type, Msg),
  cmd_params(Type, 2),
  \+member(Type, [kick, invite]), !, % green, no further matches
  get_irc_stream(Me, Stream),
  format(Stream, Msg, [Target, Str]),
  flush_output(Stream).

% Send a message of Type with respect to Chan, to the Target.
send_msg(Me, Type, Chan, Target) :-
  cmd(Type, Msg),
  get_irc_stream(Me, Stream),
  (  Type = kick,
     format(Stream, Msg, [Chan, Target])
  ;  Type = invite,
     format(Stream, Msg, [Target, Chan])
  ), !,
  flush_output(Stream).

:- module(parser,
     [ parse_line/2
      ,prefix_id/2
      ,prefix_id/4 ]).


/** <module> Parsing messages

This module takes a message line relayed from an IRC server and parses
it into a compound term that acts as an acceptable message type for
the messages themselves. This is useful for manipulating, processing
and acting on events in an IRC session.

Source : http://www.networksorcery.com/enp/protocol/irc.htm

Alternative Source : http://irchelp.org/irchelp/rfc/

CTCP : http://irchelp.org/irchelp/rfc/ctcpspec.html

Message syntax:

message =	[ ":" prefix SPACE ] command [ params ] crlf

prefix =	servername / ( nickname [ [ "!" user ] "@" host ])

command =	1*letter / 3digit

params =	*14( SPACE middle ) [ SPACE ":" trailing ]
=/	14( SPACE middle ) [ SPACE [ ":" ] trailing ]

// Any byte except NUL, CR, LF, " " and ":".

nospcrlfcl =	%x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF

middle =	nospcrlfcl *( ":" / nospcrlfcl )

trailing =	*( ":" / " " / nospcrlfcl )

SPACE =	%x20	; Whitespace.

crlf =	%x0D %x0A	; Carriage return/linefeed.

*/


%% parse_line(+Line, -Msg) is semidet.
%
%  After splitting the line from any potential trailing parameters, format
%  the line in a message compound term for proper reading outside of this
%  module.
%
%  @arg Line A list of codes representing a line of a relayed message
%  @arg Msg A compound term representation of Line

parse_line(Line, Msg) :-
  split_from_trailer(Line, Out),
  once(fmt_line(Out, Msg)).


%% prefix_id(+Prefix, -Servername) is semidet.
%
%  Extract a servername from a msg prefix.
%
%  @arg Prefix A string (usually in message structure) that represents the prefix
%  of the sender
%  @arg Servername The server name obtained from the sender's prefix

prefix_id(Prefix, Servername) :-
  split_string(Prefix, " ", "", [Servername|_]).


%% prefix_id(+Prefix, -Nick, -User, -Host) is semidet.
%
%  Extract the Nick, User, and Host portions of a prefix from a msg. All of these
%  arguments are to take the form of a string type.

prefix_id(Prefix, Nick, User, Host) :-
  split_string(Prefix, "!", "", [Nick|[Rest]]),
  split_string(Rest, "@", "", [User|[Host]]).


%--------------------------------------------------------------------------------%


%% fmt_line(+Line, -Msg) is semidet.
%
%  Split a server message into (potentially) 3 parts. The message can be split
%  into a prefix, command, and some command parameters. However, the prefix
%  is optional and the parameter list can potentially be empty.
%
% Msg output is of the format:
%
% 1) msg(prefix, command, [parameters...], trailing_parameter).
%
% 2) msg(prefix, command, [parameters...]).
%
% 3) msg(command, [parameters...], trailing_parameter).
%
% 4) msg(command, [parameters...]).

fmt_line([has_prefix, Main, Trailer], msg(Prefix, Cmd, Params, Trailer)) :-
  split_string(Main, " ", "", [Prefix,Cmd|Params]).

fmt_line([has_prefix, Main], msg(Prefix, Cmd, Params)) :-
  split_string(Main, " ", "", [Prefix,Cmd|Params]).

fmt_line([Main, Trailer], msg(Cmd, Params, Trailer)) :-
  split_string(Main, " ", "", [Cmd|Params]).

fmt_line([Main], msg(Cmd, Params)) :-
  split_string(Main, " ", "", [Cmd|Params]).


%% split_from_trailer(+Line, -Out) is det.
%
%  Split the main portion of the message from the trailer portion of the message
%  if a trailer does exist. These are the possibilities when operating
%  under the current IRC protocol:
%
%  1) [has_prefix, Main, Trailer]
%  2) [has_prefix, Main]
%  3) [Main, Trailer]
%  4) [Main]

split_from_trailer(Line, Out) :-
  (  split(First, Line, Trailer)
  -> (  First = [58|Main]
     -> Out = [has_prefix, Main, Trailer]
     ;  Main = First,
        Out = [Main, Trailer]
     )
  ;  (  Line = [58|Main]
     -> Out = [has_prefix, Main]
     ;  Main = Line,
        Out = [Main]
     )
  ).


split([]) --> ` :`.
split([M|Main]) -->
  [M], split(Main).

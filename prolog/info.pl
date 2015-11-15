
:- module(info,
     [ known/2
      ,get_irc_stream/2
      ,get_irc_write_stream/2
      ,get_irc_server/2
      ,handle_server/2
      ,connection/7
      ,min_msg_len/2
      ,c_specs/6
      ,timer/2 ]).

:- dynamic known/2.
:- dynamic get_irc_stream/2.
:- dynamic get_irc_write_stream/2.
:- dynamic connection/7.
:- dynamic c_specs/6.
:- dynamic timer/2.

:- dynamic get_irc_server/2.
:- dynamic min_msg_len/2.

:- dynamic handle_server/2.


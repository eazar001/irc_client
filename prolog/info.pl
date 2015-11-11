
:- module(info,
     [ known/2
      ,get_irc_stream/2
      ,get_irc_write_stream/2
      ,get_tcp_socket/2
      ,get_irc_server/2
      ,min_msg_len/2
      ,connection/7
      ,c_specs/6
      ,timer/2 ]).

:- dynamic known/2.
:- dynamic get_irc_stream/2.
:- dynamic get_irc_write_stream/2.
:- dynamic get_tcp_socket/2.
:- dynamic connection/7.
:- dynamic c_specs/6.
:- dynamic timer/2.

:- dynamic get_irc_server/2.
:- dynamic min_msg_len/2.



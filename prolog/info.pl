
:- module(info,
     [ known/1
      ,get_irc_stream/1
      ,get_irc_write_stream/1
      ,get_tcp_socket/1
      ,get_irc_server/2
      ,min_msg_len/2
      ,connection/6
      ,c_specs/5
      ,timer/1 ]).

:- thread_local known/1.
:- thread_local get_irc_stream/1.
:- thread_local get_irc_write_stream/1.
:- thread_local get_tcp_socket/1.
:- thread_local connection/6.
:- thread_local c_specs/5.
:- thread_local timer/1.

:- dynamic get_irc_server/2.
:- dynamic min_msg_len/2.



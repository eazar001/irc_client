
# What this is

This is a low level IRC client library for making anything related to
IRC usage. For example, one can make a traditional bot, conversation
analyzer, simple logging system, statistics tracker, or even a full
blown IRC Chat application.

# What this does

This is a layer of abstraction over the IRC protocol. It helps you
establish connections to multiple servers and channels, using different
nicknames. Each connection will run in separate threads that contain
aliases that you give to them. The job of irc_client is to handle the
bookkeeping of all the specs for each server connection, and handle the
streaming of IRC server relayed messages to event handlers that you
declare. This does NOT maintain persistent connections, or even display
the text for you. However, this is simple enough to do by asserting 
event handlers. See the `examples.pl` file in the examples folder for a
clue on how to get this done. Also, an example of an IRC bot that uses
this library can be found at: https://github.com/eazar001/yesbot

# How do I use this?

```prolog
% sends a message in the ##prolog channel saying "hello world", using
% the connection with the alias, "freenode".
?- priv_msg(freenode, "hello world", "##prolog").

% sends a message to the user "nick02" with "hello" and "world", each
% on separate lines, on the connection with the alias, "efnet".
?- priv_msg(efnet, "hello\nworld", "nick02").

% sends an invitation to ##math, for freenode user, "user001"
?- send_msg(freenode, invite, "user001", "##math").

% disconnects from server on the efnet connection alias.
?- send_msg(efnet, quit).
```

One of the most important predicates is `priv_msg/N`. This is needed
for sending private messages to channels, or to individual users.

* priv_msg(+Id, +Text, +Recipient)
  This will send Text to the Recipient, on the connection Id. Id is
  an atom; Text and Recipient are strings. If the character limit is
  exceeded, the text will automatically be segmented into several
  messages, until the full message is delivered.

* priv_msg(+Id, +Text, +Recipient, :Options).
  Here one has special options. `auto_nl(Boolean)` where Boolean can be
  specified as true or false. If true, messages that are too large
  will be automatically segmented into multiple messages. `at_most(N)`,
  where N represents the number of lines that will be sent (at most).
  This is useful for rate limiting.

Another important group of predicates is `send_msg/N`. This group of
predicates is responsible for sending certain types of messages that
are responsible for carrying out various IRC actions. A basic
understanding of IRC protocols is required to use these. The arity (N)
of send_msg/N, at this point, ranges from 2 to 4. ALL OF THESE commands
must have the first argument as the connection ID to send the message
on. The format for these commands are listed below:

* send_msg(ID, admin, "target").
* send_msg(ID, away, "away message")
* send_msg(ID, help)
* send_msg(ID, info).
* send_msg(ID, links).
* send_msg(ID, lusers, "##channel").
* send_msg(ID, names, "##channel").
* send_msg(ID, oper, "username", "pass").
* send_msg(ID, rules).
* send_msg(ID, userip, "nick").
* send_msg(ID, users).
* send_msg(ID, who_op, "##channel").
* send_msg(ID, who_ops).
* send_msg(ID, whois, "nick").
* send_msg(ID, whowas, "nick").
* send_msg(ID, notice, "message", "chan or nick").
* send_msg(ID, time, "server").
* send_msg(ID, kick, "##channel", "user").
* send_msg(ID, invite, "user", "##channel").
* send_msg(ID, topic, "##channel", "topic message").
* send_msg(ID, quit).
* send_msg(ID, nick, "nickname").
* send_msg(ID, join, "channel").

There is an examples folder containing examples (with comments) that
demonstrate how this is meant to be used.

# What else?

I will add more commands and features as I find time to work on this,
but for now ... pull requests are very much welcome.

At the time of writing this, this library is being used for one of the
bots in ##prolog (Yesbot). There's another bot that's used for
interactive prolog commands in-channel (PrologMUD). The second bot can
be found at https://github.com/TeamSPoon/MUD_ircbot


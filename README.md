
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
streaming of IRC server relayed message to event handlers that you
declare. This does NOT maintain persistent connections, or even display
the text for you. However, this is simple enough to by asserting event
handlers. See the `examples.pl` file in the examples folder for a clue
on how to get this done. Also, an example of an IRC bot that uses this
library can be found at: https://github.com/eazar001/yesbot

# How do I use this?

```prolog
% sends a message in the ##prolog channel saying "hello world", using
% the connection with the alias, "freenode".

?- priv_msg(freenode, "hello world", "##prolog").

% sends a message to the user "nick02" with "hello" and "world", each
% on seperate lines, one the connection with the alias, "efnet".

?- priv_msg(efnet, "hello\nworld", "nick02").

% sends an invitation to ##math, for freenode user, "user001"

?- send_msg(freenode, invite, "user001", "##math").

% disconnects from server on the efnet connection alias.

?- send_msg(efnet, quit).
```

There is an examples folder containing examples (with comments) that
demonstrate how this is meant to be used for the moment. This may
change over time.

# What else?

There are more commands that have been implemented; there are also more
to come in the future. Not all of these have been explained above, but
but they will be documented eventually. As for now, see the file
`operator.pl`, for a list of available commands.

I will add more commands and features as I find time to work on this,
but for now ... pull requests are very much welcome.

At the time of writing this, this library is being used for one of the
bots in ##prolog (Yesbot). There's another bot that's used for
interactive prolog commands in-channel (PrologMUD). The second bot can
be found at https://github.com/TeamSPoon/MUD_ircbot


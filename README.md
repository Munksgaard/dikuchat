dikuchat
========

Erlang server for diku-chat-net (https://github.com/sshine/diku-chat-net)


Usage
-----

 - Start the server on port 8090 by running
   `dikuchat:start_server(8090)`.

 - Connect with telnet by running `telnet 127.0.0.1 8090`


Commands
--------

`who` -  List the current users on the server.

`name [NAME]` - Assume the name `[NAME]`.

`broadcast [MESSAGE]` - Send the message `[MESSAGE]` to all users.

`quit` - Disconnect from the server.


Deviations from the standard
----------------------------

At this point, dikuchat commands are written in lower case.

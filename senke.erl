-module(senke).
-export([start/0]).

start() ->
	receive
		{content, Text} ->
			werkzeug:logging("senke.log",lists:concat(["Nachricht erhalten: ", Text, "\r\n"])),
			start()
	end.
	

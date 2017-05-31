-module(senke).
-export([start/1]).

start(SNo) ->
	receive
		{content, Text} ->
			werkzeug:logging(lists:concat(["senke", SNo, ".log"]),lists:concat(["Nachricht erhalten: ", Text, "\r\n"])),
			start(SNo)
	end.
	

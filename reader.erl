-module(reader).
-export([start/0]).

-define(logfile, "quelle.log").

start() ->
	spawn(reader, readBuffer, [self()]),
	provideData("__________INIT__________").

% Lese 24 Bytes aus dem Puffer
readBuffer(PID) ->
	case io:get_chars('', 24) of
		eof ->
			%werkzeug:logging("quelle.log",pid_to_list(self()) ++ ": " ++ "readBuffer: EOF\r\n"),
			timer:sleep(500);
		Text ->
			%werkzeug:logging("quelle.log",pid_to_list(self()) ++ ": Read Text " ++ Text ++ "and sent to:" ++ pid_to_list(PID) ++ ": " ++ "readBuffer: READ\r\n"),
			PID ! {content, Text}
	end,
	readBuffer(PID).

% Nachrichtengenerator
provideData(Data) ->
	receive
		{content, NewData} ->
			provideData(NewData);
		{generate, PID} ->
			werkzeug:logging(?logfile,"generated and provided new message.\r\n"),
			PID ! {content, Data},
			provideData(Data)
	end.

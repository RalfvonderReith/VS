-module(receiver).
-export([start/4]).

-define(logfile, "receiver.log").

start(Interface, IP, Port, TimeManager) ->
	%USE IN PRODUCTION!
	%spawn(receiver, netreceive, [self(), Interface, IP, Port]),
	communication([], TimeManager).

% Empfang von UDP Nachrichten ueber Socket
netreceive(Receiver, Interface, IP, Port) ->
	Socket = werkzeug:openRec(IP, Interface, Port),
	gen_udp:controlling_process(Socket, self()),
	netloop(Receiver, Socket).

netloop(Receiver, Socket) ->
	Message = gen_udp:recv(Socket, 0),
	Receiver ! {netmsg, Message},
	netloop(Receiver, Socket).


% Nachrichtenbearbeitung
communication(Messages, TimeManager) ->
	receive
		% Inhalt extrahieren
		{netmsg, {ok, {_Address, _Port, Packet}}} ->
			TimeManager ! {get_time, self()},
			receive
				{time, LocalTime} ->
					LocalTime
			end,
			% Bit Syntax - Value:Size/TypeSpecifierList
			<<StationClass:1/binary,
			  Payload:24/binary,
			  SlotNr:8/integer,
			  TimeSent:64/integer-big>> = Packet,
			Station = erlang:binary_to_list(StationClass),
			Content = erlang:binary_to_list(Payload),
			Slot = SlotNr,
			Timestamp = TimeSent,
			werkzeug:logging(?logfile, lists:concat(["received message: ", Station, " | ", Content, " | ", Slot, " | ", Timestamp, " at ", LocalTime, "\r\n"])),
			werkzeug:logging(?logfile, lists:concat(["log length before receiving: ", length(Messages) ,"\r\n"])), 
			communication([{Station, Content, Slot, Timestamp, LocalTime} | Messages], TimeManager);
		% Slot ist vorbei
		{get_message, PID} ->
			% Pruefe ob genau eine Nachricht angekommen ist
			case length(Messages) of
				% keine Nachrichten
				0 ->
					PID ! {msg, none},
					werkzeug:logging(?logfile, "received message request: no messages provided. \r\n"),
					communication([], TimeManager);
				% genau eine Nachricht angekommen
				1 ->
					[Message] = Messages,
					werkzeug:logging(?logfile, "received message request: returned a message. \r\n"),
					PID ! {msg, Message},
					communication([], TimeManager);
				% eine Kollision, da mehrere Nachrichten innerhalb eines Slots angekommen sind
				_ ->
					PID ! {collision},
					werkzeug:logging(?logfile, "received message request: collision. \r\n"),
					communication([], TimeManager)
			end
	end.


%start() -> 
	%Station = werkzeug:createBinaryS("A"),
	%PayLoad = werkzeug:createBinaryD("randomrandomrandomrandom"),
	%Slot = werkzeug:createBinaryNS(1),
	%Timestamp = werkzeug:createBinaryT(werkzeug:getUTC()),
	%SomeMessage = werkzeug:concatBinary(Station, PayLoad, Slot, Timestamp),
	
	%sio:format("message: ~p~n",[werkzeug:message_to_string(SomeMessage)]).
	
	%werkzeug:openSe('127.0.0.1', 5000),
	

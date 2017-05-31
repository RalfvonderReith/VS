-module(receiver).
-export([start/5]).

start(Interface, IP, Port, TimeManager, SNo) ->
	%USE IN PRODUCTION!
	spawn(receiver, netreceive, [self(), Interface, IP, Port]),
	communication([], TimeManager, SNo).

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
communication(Messages, TimeManager, SNo) ->
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
			werkzeug:logging(lists:concat(["receiver",SNo,".log"]), lists:concat(["received message: ", Station, " | ", Content, " | ", Slot, " | ", Timestamp, " at ", LocalTime, "\r\n"])),
			werkzeug:logging(lists:concat(["receiver",SNo,".log"]), lists:concat(["log length before receiving: ", length(Messages) ,"\r\n"])), 
			communication([{Station, Content, Slot, Timestamp, LocalTime} | Messages], TimeManager, SNo);
		% Slot ist vorbei
		{get_message, PID} ->
			% Pruefe ob genau eine Nachricht angekommen ist
			case length(Messages) of
				% keine Nachrichten
				0 ->
					PID ! {msg, none},
					werkzeug:logging(lists:concat(["receiver",SNo,".log"]), "received message request: no messages provided. \r\n");
				% genau eine Nachricht angekommen
				1 ->
					[Message] = Messages,
					werkzeug:logging(lists:concat(["receiver",SNo,".log"]), "received message request: returned a message. \r\n"),
					PID ! {msg, Message};
				% eine Kollision, da mehrere Nachrichten innerhalb eines Slots angekommen sind
				_ ->
					PID ! {collision},
					werkzeug:logging(lists:concat(["receiver",SNo,".log"]), "received message request: collision. \r\n")
			end,
			communication([], TimeManager, SNo)
	end.

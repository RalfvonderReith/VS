-module(sender).
-export([start/8]).

start(LocalAddress, SendAddress, Port, Type, Reader, TimeManager, SlotManager, SNo) ->
	%ONLY FOR TESTING: 
	%Socket = socket,
	%USE IN PRODUCTION!
	Socket = werkzeug:openSe(LocalAddress, Port),
	werkzeug:logging(lists:concat(["sender",SNo,".log"]), "Sender started\r\n"),
	communication(Socket, SendAddress, Port, Type, Reader, TimeManager, SlotManager, SNo).
	
communication(Socket, Address, Port, Type, Reader, TimeManager, SlotManager, SNo) ->
	receive
		{send, PID, SlotStart, SlotEnd} ->
			werkzeug:logging(lists:concat(["sender",SNo,".log"]), "received send\r\n"),
			Reader ! {generate, self()},
			receive
				{content, Data} ->
					Data
			end,
			werkzeug:logging(lists:concat(["sender",SNo,".log"]), "received data\r\n"),
			%get slot
			NewSlot = getNewSlot(SlotManager),
			werkzeug:logging(lists:concat(["sender",SNo,".log"]), "received slot\r\n"),
			%send message
			Time = getTime(TimeManager),
			werkzeug:logging(lists:concat(["sender",SNo,".log"]), "received time\r\n"),
			
			if
				(Time > SlotStart) and (Time < SlotEnd) ->
					Packet = werkzeug:concatBinary(
								werkzeug:createBinaryS(Type),
								werkzeug:createBinaryD(Data), 
								werkzeug:createBinaryNS(NewSlot),
								werkzeug:createBinaryT(Time)),
					werkzeug:logging(lists:concat(["sender",SNo,".log"]), "created packet\r\n"),
					%%ONLY FOR TESTING! 
					%tester ! {message, Type, Data, NewSlot, Time},
					%use in production! : 
					ok = gen_udp:send(Socket, Address, Port, Packet),
					werkzeug:logging(lists:concat(["sender",SNo,".log"]), lists:concat(["new message sent; Time:", Time, "; Reservation: ", NewSlot, ".\r\n"])),
					PID ! {ok, NewSlot};
				true ->
					werkzeug:logging(lists:concat(["sender",SNo,".log"]), lists:concat(["missed slot : currentTime ", Time, " expected: ", SlotStart, "-", SlotEnd, "\r\n"])),
					PID ! {missed}
					
			end,
			communication(Socket, Address, Port, Type, Reader, TimeManager, SlotManager, SNo)
	end.
	
getNewSlot(SlotManager) ->
	SlotManager ! {get_slot, self()},
	receive
		{slot, Slot} ->
			Slot
	end.

getTime(TimeManager) ->
	TimeManager ! {get_time, self()},
	receive
		{time, LocalTime} ->
			LocalTime
	end.

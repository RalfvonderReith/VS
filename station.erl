-module(station).
-export([start/5, start/6]).

-define(logfile, "station.log").
-define(frameLength, 1000).
-define(slots, 25).
-define(slotlength, 40).
-define(no_slot, none).

%takes interface to use, target IP and Port, StationClass and initial TimeOffset as strings and starts the station
start(SInterface, SIP, SPort, SClass, SOffset) ->
	start(SInterface, SIP, SPort, SClass, SOffset, 0).
start(SInterface, SIP, SPort, SClass, SOffset, TimeOffset) ->
	werkzeug:logging(?logfile, "---starting Station---\r\n"),
	%USE IN PRODUCTION!
	Interface = getIfAddr(SInterface),
	{ok, IP} = inet_parse:address(SIP),
	{Port, _} = string:to_integer(SPort),
	%USE WITH TESTER
	%Interface = SInterface,
	%IP = SIP, 
	%Port = SPort,
	Class = SClass,
	{Offset, _} = string:to_integer(SOffset),
	
	TimeManager = spawn(timemanager, start, [TimeOffset]),
	SlotManager = spawn(slotmanager, start, [?slots]),
	Receiver = spawn(receiver, start, [Interface, Port, IP, TimeManager]),
	%ONLY FOR TESTING: 
	spawn(tester, start, [Receiver, 10]),
	Reader = spawn(reader, start, []),
	Sender = spawn(sender, start, [Interface, IP, Port, Class, Reader, TimeManager, SlotManager]),
	Senke = spawn(senke, start, []),
	
	werkzeug:logging(?logfile, "started all modules\r\n"),
	
	ReservedSlot = {next, ?no_slot},
	LastOwnSlot = -1,
	LocalTime = getTime(TimeManager),
	TimeNextSlot = ?slotlength - (LocalTime rem ?slotlength),
	timer:send_after(TimeNextSlot, next_slot),
	werkzeug:logging(?logfile, lists:concat([LocalTime, ": started frame timer: ", TimeNextSlot, "\r\n"])),
	communication(LastOwnSlot, ReservedSlot, Class, Receiver, Sender, TimeManager, SlotManager, Senke).

%lastOwnSlot: slot in dem zuletzt gesendet wurde
%reserved slot: {dieser oder nächster frame/reservierter slot}
communication(LastOwnSlot, ReservedSlot, StationClass, Receiver, Sender, TimeManager, SlotManager, Senke) ->
	werkzeug:logging(?logfile, "wait for next_slot \r\n"),
	receive 
		next_slot ->
			
			%get current time
			LocalTime = getTime(TimeManager),
			%compute current slot number
			%reduce localtime to framelength then compute slotnumber (+1 because its based on 1 and not on 0).
			CurFrameTime = (LocalTime rem ?frameLength),
			CurrentSlot = (CurFrameTime div ?slotlength) + 1,
			werkzeug:logging(?logfile, lists:concat(["Station: ", LocalTime, ": ", CurFrameTime, " -> ", CurrentSlot, " ", ?slotlength, "\r\n"])),
			%get message -> returns original slot if there is no collision with own message 
			%and delegates content, slot and timestamp information to respective modules
			ReservedSlotAfterReceive = getMessage(ReservedSlot, LastOwnSlot, CurrentSlot, Receiver, TimeManager, SlotManager, Senke),
			
			if
				%if its first slot in frame:
				CurrentSlot == 1 -> %start of new frame
					%if there is no reservation for this frame, restart: pick a slot for this frame
					werkzeug:logging(?logfile, "=== new frame ===\r\n"),
					{Frame, CurrentReservation} = ReservedSlotAfterReceive,
					if
						%no slot reserved last frame -> reserve one for this to enter
						{Frame, CurrentReservation} == {this, ?no_slot} ->
							EntrySlot = getNewSlot(SlotManager),
							ReservedSlotAfterFrameStart = {this, EntrySlot},
							werkzeug:logging(?logfile, lists:concat(["Station: new entry with slot: ", EntrySlot, "\r\n"]));
						%a slot reserved for next frame -> is a reservation for this frame, as a new one just started
						Frame == next ->
							ReservedSlotAfterFrameStart = {this, CurrentReservation};
						true ->
							io:format("received this-reservation at frame start ~p~n",[ReservedSlotAfterReceive]),
							ReservedSlotAfterFrameStart = {this, CurrentReservation}
						
					end,
					%also reset slotmanager and timemanager for new frame
					SlotManager ! {new_frame},
					TimeManager ! {sync_time};
				true ->
					ReservedSlotAfterFrameStart = ReservedSlotAfterReceive
			end,
			{_, SomeReservation} = ReservedSlotAfterFrameStart,
			werkzeug:logging(?logfile, lists:concat(["Station: check for sending, Current: ", CurrentSlot, " Reserved: ", SomeReservation,", CurrentTime ", LocalTime, "\r\n"])),
			%then check if currentSlot is own slot
			if
				{this, CurrentSlot} == ReservedSlotAfterFrameStart ->
					%get content
					SlotStart = (LocalTime - LocalTime rem ?slotlength),
					SlotEnd = SlotStart + ?slotlength,
					Sender ! {send, self(), SlotStart, SlotEnd},
					receive 
						{ok, NewSlotNumber} ->
							werkzeug:logging(?logfile, lists:concat(["Station: Sent message, next Reservation: ", NewSlotNumber, "\r\n"]));
						{missed} ->
							NewSlotNumber = ?no_slot,
							werkzeug:logging(?logfile, lists:concat(["Station: missed slot: ", SlotStart, " - ", SlotEnd, "!\r\n"]))
					end,
					LastSlot = CurrentSlot,
					NewSlotReservation = {next, NewSlotNumber};
				true ->
					LastSlot = LastOwnSlot,
					NewSlotReservation = ReservedSlotAfterFrameStart
			end,
			CurTime = getTime(TimeManager),
			TimeToNextSlot = ?slotlength - (CurTime rem ?slotlength),
			timer:send_after(TimeToNextSlot, next_slot),
			werkzeug:logging(?logfile, lists:concat(["Station: next slot in time ", CurTime, " wait ", TimeToNextSlot, "\r\n"])),
			communication(LastSlot, NewSlotReservation, StationClass, Receiver, Sender, TimeManager, SlotManager, Senke)
	end.
	%a new slot begins:
	%compute currentSlot
	%ask for messages of last slot
	%-> if collision -> is it own collision? -> remove reservation
	%else -> add slotreservation
	%	add timestamp if Station A
	%	add content
	%
	%frame start?
	%-> yes
	%	no slot reserved -> reserve one
	%	sm ! reset
	%	tm ! reset 
	%
	%get time
	%current slot = reserved one?
	%yes ->
	%	get message
	%	get slot
	%	get timestamp
	%	send message
	%no ->
	%	reserved slot already missed?
	%	yes -> remove reservation
	% 
	%compute time to next slot
	%start timer to next slot
	
	%Slot slot is needed to check for collisions of own messages
getMessage(ReservedSlot, LastOwnSlot, CurrentSlot, Receiver, TimeManager, SlotManager, Senke) ->
	Receiver ! {get_message, self()},
	receive
		{msg, none} ->
			werkzeug:logging(?logfile, "got no message last Slot\r\n"), 
			ReservedSlot;
		{msg, {"A", Content, Slot, TimestampSender,TimestampReceived}} ->
			werkzeug:logging(?logfile, "MESSAGE A last Slot\r\n"), 
			TimeManager ! {remote_time, TimestampSender, TimestampReceived},
			extractData(Content, Slot, SlotManager, Senke),
			ReservedSlot;
		{msg, {"B", Content, Slot, _TimestampSender, _TimestampReceived}} ->	
			werkzeug:logging(?logfile, "MESSAGE B last Slot\r\n"), 
			extractData(Content, Slot, SlotManager, Senke),
			ReservedSlot;
		{collision} ->
			werkzeug:logging(?logfile, "--COLLISION-- last Slot\r\n"), 
			if
				CurrentSlot == LastOwnSlot ->
					{this, ?no_slot};
				true ->
					ReservedSlot
			end;
		Any ->
			io:format("unexpected message format: ~p~n",[Any])
	end.
	
%helper functions
	
getIfAddr(Interface) ->
	{ok, InterfaceList} = inet:getifaddrs(),
	getAddr(Interface, InterfaceList).

getAddr(_Interface, []) ->
	{addr, err};
getAddr(Interface, [{Interface, InterfaceOptions} | _Rest]) ->
	getIP(InterfaceOptions);
getAddr(Interface, [_| Rest]) ->
	getAddr(Interface, Rest).

getIP([]) -> 
	{addr, err};
getIP([{addr, Wert} | _Rest]) ->
	Wert;
getIP([_| Rest]) ->
	getIP(Rest).

extractData(Content, Slot, SlotManager, Senke) ->
	SlotManager ! {reserve, Slot},
	Senke ! {content, Content}.

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

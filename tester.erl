-module(tester).
-export([start/2]).

start(Receiver, Size) ->
	register(tester, self()), 
	Time = 1000 - (werkzeug:getUTC() rem 1000),
	timer:send_after(Time, slot),
	communication(Receiver,Size,createReservations(Size), 0).
	
communication(Receiver, RemainingReservations, Reservations, SomeInt) ->
	receive
		slot ->	
			CurrentSlot = (werkzeug:getUTC() rem 1000) div 40 + 1,
			if %new frame
				CurrentSlot == 1 ->
					NewReservations = Reservations,
					werkzeug:logging("tester.log","tester: -----new frame-----\r\n");
				true -> 
					NewReservations = RemainingReservations
			end,
			
			[First | Rest] = NewReservations,
			if
				CurrentSlot == First ->
					Packet = werkzeug:concatBinary(
									werkzeug:createBinaryS("A"),
									werkzeug:createBinaryD(lists:concat([SomeInt, getZeros(24-length(lists:concat(["",SomeInt])))])), 
									werkzeug:createBinaryNS(First),
									werkzeug:createBinaryT(werkzeug:getUTC())),
					Receiver ! {netmsg, {ok, {irrelevant, irrelevant, Packet}}}, 
					Res = Rest,
					NewInt = SomeInt+1,
					werkzeug:logging("tester.log", lists:concat(["tester: ",werkzeug:getUTC(), ": Slot ", First, " / A \r\n"]));
				true ->
					NewInt = SomeInt,
					Res = NewReservations
			end,
			Time = 40 - (werkzeug:getUTC() rem 40),
			timer:send_after(Time, slot),
			communication(Receiver, Res, Reservations, NewInt);
		{message, Station, Data, Slot, Time} ->
			Packet = werkzeug:concatBinary(
									werkzeug:createBinaryS(Station),
									werkzeug:createBinaryD(Data), 
									werkzeug:createBinaryNS(Slot),
									werkzeug:createBinaryT(Time)),
			Receiver ! {netmsg, {ok, {irrelevant, irrelevant, Packet}}}, 
			werkzeug:logging("tester.log", lists:concat(["tester: ",werkzeug:getUTC(), ": " , Station, "|", Data, "|", Slot, "|", Time, "\r\n"])),
			communication(Receiver, RemainingReservations, Reservations, SomeInt)
	end.
			
createReservations(Size) ->
	Elems = lists:seq(1,25),
	RandomList = werkzeug:shuffle(Elems),
	{ReservedRdmList, _Rest} = lists:split(Size, RandomList),
	ReservedList = lists:sort(ReservedRdmList),
	ReservedList.

getZeros(Size) ->
	{Return, _Rest} = lists:split(Size, "000000000000000000000000"),
	Return.

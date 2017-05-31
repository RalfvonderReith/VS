-module(slotmanager).
-export([start/1]).

-define(logfile, "slots.log").

start(Size) ->
	SlotList = lists:seq(1, Size),
	communicate(SlotList, Size).
	
communicate(SlotList, Size) ->
	io:format("SlotList ~p~n", [SlotList]),
	receive
		{new_frame} ->
			werkzeug:logging(?logfile, "Station: A new frame has been started\r\n"),
			communicate(lists:seq(1, Size), Size);
		{reserve, Slot} ->
			werkzeug:logging(?logfile, lists:concat(["Slot ", Slot, "Station:  has been reserved.\r\n"])),
			communicate(reserveSlot(SlotList, Slot), Size);
		{get_slot, PID} -> 
			Slot = getSlot(SlotList),
			PID ! {slot, Slot},
			werkzeug:logging(?logfile, lists:concat(["Slot ",Slot, "Station:  has been chosen for next frame.\r\n"])),
			communicate(reserveSlot(SlotList, Slot), Size);
		Any ->
			io:format("Station: unexpected message: ~p~n",[Any])
	end.

%reserves a specified slot
reserveSlot(SlotList, Slot) -> reserveSlotH(SlotList, [], Slot).
reserveSlotH([], CheckedSlots, Slot) -> 
	werkzeug:logging(?logfile, lists:concat(["Station: Slot ", Slot, " already reserved \r\n"])),
	CheckedSlots;
reserveSlotH([Slot|Rest], CheckedSlots, Slot) -> lists:concat([CheckedSlots, Rest]);
reserveSlotH([First|Rest], CheckedSlots, Slot) -> reserveSlotH(Rest, lists:append(CheckedSlots, [First]), Slot).

%retrieves a random entry from the list
getSlot([]) -> -1;
getSlot(SlotList) -> 
	lists:nth(rand:uniform(length(SlotList)), SlotList).

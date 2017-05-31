-module(slotmanager).
-export([start/2]).

start(Size, SNo) ->
	SlotList = lists:seq(1, Size),
	communicate(SlotList, Size, SNo).
	
communicate(SlotList, Size, SNo) ->
	io:format("SlotList ~p~n", [SlotList]),
	receive
		{new_frame} ->
			werkzeug:logging(lists:concat(["slotM",SNo,".log"]), "Station: A new frame has been started\r\n"),
			communicate(lists:seq(1, Size), Size, SNo);
		{reserve, Slot} ->
			werkzeug:logging(lists:concat(["slotM",SNo,".log"]), lists:concat(["Slot ", Slot, "Station:  has been reserved.\r\n"])),
			communicate(reserveSlot(SlotList, Slot, SNo), Size, SNo);
		{get_slot, PID} -> 
			Slot = getSlot(SlotList),
			PID ! {slot, Slot},
			werkzeug:logging(lists:concat(["slotM",SNo,".log"]), lists:concat(["Slot ",Slot, "Station:  has been chosen for next frame.\r\n"])),
			communicate(reserveSlot(SlotList, Slot, SNo), Size, SNo);
		Any ->
			io:format("Station: unexpected message: ~p~n",[Any])
	end.

%reserves a specified slot
reserveSlot(SlotList, Slot, SNo) -> reserveSlotH(SlotList, [], Slot, SNo).
reserveSlotH([], CheckedSlots, Slot, SNo) -> 
	werkzeug:logging(lists:concat(["slotM",SNo,".log"]), lists:concat(["Station: Slot ", Slot, " already reserved \r\n"])),
	CheckedSlots;
reserveSlotH([Slot|Rest], CheckedSlots, Slot, _SNo) -> lists:concat([CheckedSlots, Rest]);
reserveSlotH([First|Rest], CheckedSlots, Slot, SNo) -> reserveSlotH(Rest, lists:append(CheckedSlots, [First]), Slot, SNo).

%retrieves a random entry from the list
getSlot([]) -> -1;
getSlot(SlotList) -> 
	lists:nth(rand:uniform(length(SlotList)), SlotList).

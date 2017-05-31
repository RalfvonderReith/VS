-module(timemanager).
-export([start/2]).

start(TimeShift, SNo) ->
	communication(TimeShift,[], SNo).
	
% Uhr-Operationen
communication(TimeShift, TimeOffsets, SNo) ->
	receive
		% Erzeuge aktuellen Zeitstempel nach Anfrage
		{get_time, PID} ->
			TimeInMillis = werkzeug:getUTC(),
			CurrentTime = TimeInMillis+TimeShift,
			PID ! {time, CurrentTime},
			werkzeug:logging(lists:concat(["timeM",SNo,".log"]), lists:concat(["Sent current time to ",pid_to_list(PID),"\r\n"])),
			communication(TimeShift, TimeOffsets, SNo);
		% Merke die Zeit von Typ-A-Station aus der eingegangenen Nachricht
		{remote_time, RemoteTimeStamp, ReceiveTime} ->
			Offset = (RemoteTimeStamp - ReceiveTime),
			werkzeug:logging(
				lists:concat(["timeM",SNo,".log"]), 
				lists:concat(["Received Timestamp ",RemoteTimeStamp, " with offset ",Offset,"\r\n"])),
			communication(TimeShift, [Offset | TimeOffsets], SNo);
		% Zeit-Korrektur (Mittelwert Berechnung ueber alle Zeitstempel von Typ-A-Stationen innerhalb eines Frames)
		{sync_time} ->
			if
				length(TimeOffsets) > 0  ->
					AvgOffset = lists:sum(TimeOffsets) div length(TimeOffsets),
					werkzeug:logging(lists:concat(["timeM",SNo,".log"]), lists:concat(["Syncing time with offset ",AvgOffset, "\r\n"]));
				true ->
					AvgOffset = 0
			end,
			communication((TimeShift + AvgOffset), [], SNo)
	end.
	

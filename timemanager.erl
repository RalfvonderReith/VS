-module(timemanager).
-export([start/1]).

-define(logfile, "time.log").

start(TimeShift) ->
	communication(TimeShift,[]).
	
% Uhr-Operationen
communication(TimeShift, TimeOffsets) ->
	receive
		% Erzeuge aktuellen Zeitstempel nach Anfrage
		{get_time, PID} ->
			TimeInMillis = werkzeug:getUTC(),
			CurrentTime = TimeInMillis+TimeShift,
			PID ! {time, CurrentTime},
			werkzeug:logging(?logfile, lists:concat(["Sent current time to ",pid_to_list(PID),"\r\n"])),
			communication(TimeShift, TimeOffsets);
		% Merke die Zeit von Typ-A-Station aus der eingegangenen Nachricht
		{remote_time, RemoteTimeStamp, ReceiveTime} ->
			Offset = (RemoteTimeStamp - ReceiveTime),
			werkzeug:logging(
				?logfile, 
				lists:concat(["Received Timestamp ",RemoteTimeStamp, " with offset ",Offset,"\r\n"])),
			communication(TimeShift, [Offset | TimeOffsets]);
		% Zeit-Korrektur (Mittelwert Berechnung ueber alle Zeitstempel von Typ-A-Stationen innerhalb eines Frames)
		{sync_time} ->
			if
				length(TimeOffsets) > 0  ->
					AvgOffset = lists:sum(TimeOffsets) div length(TimeOffsets),
					werkzeug:logging(?logfile, lists:concat(["Syncing time with offset ",AvgOffset, "\r\n"]));
				true ->
					AvgOffset = 0
			end,
			communication((TimeShift + AvgOffset), [])
	end.
	

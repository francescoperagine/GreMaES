logFile(gremaes, 'logs/GreMaES.log').
% logFile(diagnosis, 'logs/diagnosis.log').

% logger_init/0 - Deletes every log at the start
logger_init :-
    all(LogFile, logFile(File, LogFile), LogFiles),
    (member(LogFile, LogFiles) ->  file_exists(LogFile), delete_file(LogFile)).

% logFile(readings, 'logs/readings.log').
% logFile(devices, 'logs/devices.log').

log(Message) :-
    functor(Message, Name, _),
    Name \= actuator,
    % (Name = symptom ; Name = diagnosis ; Name = plant_reading),
    logLine(gremaes, Message).
% If the message it's not a reading, appends it to the current line
% log(Message) :-
%     functor(Message, diagnosis, _),
%     logLine(gremaes, Message).

% Writes a reading per line
% log(Message) :-
%     functor(Message, plant_reading, _),
%     logLine(gremaes, Message).
% Append an actuator's action
log(Message) :-
    functor(Message, Name, _),
    Name = actuator,
    logAppend(gremaes, Message).

log(Message) :-
    logLine(gremaes, Message).

% logForwardAppend/2 - Appends to the current line
logAppend(File, Message) :-
    logFile(File, FilePath),
    open(FilePath, append, Stream),
    write(Stream, Message),
    close(Stream).

% logForwardNewline/2 - Writes on a new line
logLine(File, Message) :-
    logFile(File, FilePath),
    open(FilePath, append, Stream),
    write(Stream, Message),
    write(Stream, '.'),
    nl(Stream),
    close(Stream).

% log/1 Default behavior if no diagnosis nor readings are provided: appends X to the current log line
% log(Term) :- 
%     functor(Term, Name, _),
%     Name \= plant_reading,
%     Name \= diagnosis,
%     logFile(diagnosis, FilePath),
%     open(FilePath, append, Logfile),
%     write(Logfile, Term),
%     close(Logfile).
% lognl/1 Same as log/1, but with a new line for every entry
% logln(Term) :-
%     functor(Term, Name, _),
%     Name \= plant_reading,
%     Name \= diagnosis,
%     logFile(diagnosis, FilePath),
%     open(FilePath, append, Logfile),
%     nl(Logfile),
%     write(Logfile, Term),
%     write(Logfile, ','),
%     close(Logfile).
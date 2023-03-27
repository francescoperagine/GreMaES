logFile(gremaes, 'logs/GreMaES.log').

% logger_init/0 - Deletes every log at the start
logger_init :-
    all(LogFile, logFile(File, LogFile), LogFiles),
    (member(LogFile, LogFiles) ->  file_exists(LogFile), delete_file(LogFile)).

log(Message) :-
    logFile(File, FilePath),
    open(FilePath, append, Stream),
    write(Stream, Message),
    close(Stream),
    write(Message).

logln(Message) :-
    logFile(File, FilePath),
    open(FilePath, append, Stream),
    write(Stream, Message),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    writeln(Message).
% log_file/2
log_file(observation,'logs/observations.log').
log_file(actuator,'logs/actuators.log').

% logger_init/0 - Deletes every log at the start
logger_init :-
    all(LogFile,(log_file(File,LogFile),(file_exists(LogFile),delete_file(LogFile))),LogFiles).

% log/2
log(X,Message) :-
    log_file(X,File),
    open(File,append,Stream),
    write(Stream,Message),
    nl(Stream),
    close(Stream),
    writeln(Message).
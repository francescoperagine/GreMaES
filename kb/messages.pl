% welcome/1
welcome :-
    writeln('*******************************************************************************************************'),
    writeln('\nWelcome to GreMaES  - the Greehouse Management Expert System developed by Francesco Peragine.\n'),
    writeln('You can interact with the ES in the \'user mode\', start the monitor mode or consult the knowledge base.'),
    writeln('The software will provide diagnosis and suggestions about the treatment, if any.\n'),
    writeln('Ethics & Privacy: no plants were harmed during develompent and tests phases (but few died of old age).'),
    writeln('No personal informations will be stored nor disclosed.\n'),
    writeln('*******************************************************************************************************').

welcome_monitor :-
    writeln('*******************************************************************************************************'),
    writeln('\nWelcome to GreMaES - Monitor mode.\n'),
    writeln('The system will now perform random samplings from few simulated sensors.'),
    writeln('To smoothe and speed up the process the sampling size is fixed to 10 and the interval is set to 0.\n'),
    writeln('*******************************************************************************************************'),
    writeln('\nPlants hosted in the virtual greenhouse:\n').

goodbye :-
    nl,writeln('Thank you for using GreMaES - the Greehouse Management Expert System.'),
    writeln('See you next time. FP.').

question_code(fruition_mode(mode_user),'\nDo you want to perform a diagnostic evaluation of the health status of a plant').
explanation(fruition_mode(mode_user),'\nProvide the symptoms that the plant exibits to understand if it\'s affected by any problem and to get eventual treatment instructions.\n').
question_code(fruition_mode(mode_monitor),'\nDo you want to start the monitor mode').
explanation(fruition_mode(mode_monitor),'\nSimulates the use of devices to test the automations of the system.\n').
question_code(fruition_mode(mode_kb),'\nDo you want to explore the Knowledge Base').
explanation(fruition_mode(mode_kb),'\nShows all the informations about health problems,symptoms,treatments.\n').

question_code(start_again,'\nWould you like to run the program again').
question_code(new_symptom,'\nWould you like to register a new symptom').
question_code(continue_monitor_loop,'\nDo you want to continue the sampling').
question_code(need_explanation,'\nDo you want to check how the inference went by').

question_code(view(X),X) :- write('\nWould you like to browse the ').

message_code(not_recognized_value,'The selected value was not recognized. Try again.').
message_code(yes_or_no,'Please answer yes or no.').
message_code(no_condition,'There is no condition to treat.').
message_code(no_explanation,'There is no explanation for this.').
message_code(preserve_environment,'Preserve the plant\'s environment accordingly to its needs.').
message_code(inference,'\nReasoning performed by the forward engine (ID-Inference step):\n').
message_code(no_symptom,'There are no observed symptoms.').

message_code(signs,'\nHow is the symptom manifested?\n').
message_code(sign_locations,'\nWhich location is affected by the symptom?\n').
message_code(sign_colors,'\nWhat color does it have?\n').

message_code(missing_nutrient,'Treatment: provide the missing nutrient to the plant.').
message_code(treatment,'* Treatment: ').
message_code(treatment_none,'* There are no treatments for ').
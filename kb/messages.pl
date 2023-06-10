% welcome/1
welcome :-
    writeln('Welcome to GreMaES  - the Greehouse Management Expert System made by Francesco Peragine.'),
    writeln('You can submit the symptoms manifested on your plants to get a diagnosis,browse the Knowledge Base or start the monitor mode.'),
    writeln('If there are any,the software will provide both diagnosis and suggestions about the treatment.'),
    writeln('Ethics & Privacy: no plants were harmed during the tests and no personal informations will be stored nor disclosed.'),nl.

welcome_monitor :-
    writeln('This is the GreMaES monitor mode.'),
    writeln('To check out the functionalities of the system,some devices\' readings sampling will be randomly generated.'),
    writeln('\nPlants hosted in the virtual greenhouse:'),nl.

goodbye :-
    nl,writeln('Thank you for using GreMaES - the Greehouse Management Expert System.'),
    writeln('See you next time. FP.').

% question_code(temperature_reading,'What is the temperature').
% question_code(humidity_reading,'What is the humidity').
% question_code(caption_reading,'What is the caption').

question_code(fruition_mode(mode_user),'Do you want to perform a diagnostic evaluation of the health status of a plant').
explanation(fruition_mode(mode_user),'Provide the symptoms that the plant exibits to understand if it\'s affected by any problem and to get eventual treatment instructions').
question_code(fruition_mode(mode_kb),'Do you want to explore all the Knowledge Base facts').
explanation(fruition_mode(mode_kb),'Shows all the informations about health problems,symptoms,treatments').
question_code(fruition_mode(mode_monitor),'Do you want to start the monitor mode').
explanation(fruition_mode(mode_monitor),'Simulates the use of devices to test the automations of the system').
question_code(debug_mode,'Do you want to proceed with debug mode').
question_code(start_again,'\nWould you like to run the program again').
question_code(new_symptom,'Would you like to register a new symptom').
question_code(continue_monitor_loop,'Do you want to continue the sampling').
question_code(need_explanation,'Do you want to check how the inference went by').

question_code(view(X),X) :- write('Would you like to browse the ').
question_code(has(X),X) :- write('Does it have a particular ').
question_code(what(X),X) :- write('What ').

% message_code(item_number_no_exit,'Enter the entry number of your interest,followed by a dot (.).').
message_code(stop_selection,'Stop by user selection.').
message_code(not_recognized_value,'The selected value was not recognized. Try again.').
message_code(yes_or_no,'Please answer yes or no.').
message_code(no_condition,'There is no condition to treat.').
message_code(no_explanation,'There is no explanation for this.').
message_code(no_symptom,'There are no observed symptoms.').
message_code(no_actuator,'There is no suitable actuator for ').
message_code(no_plant_actuator,'There is no actuator for plant ').
message_code(sign_locations,'Which location is affected by the problem?').
message_code(signs,'How is the problem manifested?').
message_code(sign_colors,'What color does it have?').
message_code(because_of,'\nBecause of ').
message_code(diagnosis_of,' the diagnosis is of ').
message_code(due_from,',could be due from ').
message_code(option_selected,'You have selected option n.').
message_code(missing_nutrient,'Treatment: provide the missing nutrient to the plant.').
message_code(treatment,'* Treatment: ').
message_code(treatment_none,'* There a no treatments for this.').

message_code(no_type_actuator,'There is no actuator for that type').
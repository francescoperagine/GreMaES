% welcome/1
welcome :-
    writeln('Welcome to the greehouse management program by Francesco Peragine.'),
    writeln('You can either submit the symptoms manifested on your plants to get a diagnosis or browse the Knowledge Base.'),
    writeln('If there are any, the software will provide both diagnosis and suggestions about the treatment.'),
    writeln('Ethics & Privacy: no plants were harmed during the tests and no personal informations will be stored nor disclosed.'), nl.

welcome_sensor :-
    writeln('This is the GreMaES sensor mode.'),
    writeln('To check out the functionalities of the system, some devices\' readings sampling will be randomly generated.'),
    writeln('Plants hosted in our virtual greenhouse:').

goodbye :-
    nl, writeln('Thank you for using the greenhouse management program.'),
    writeln('See you next time. FP.').

% question_code(temperature_reading,'What is the temperature').
% question_code(humidity_reading,'What is the humidity').
% question_code(caption_reading,'What is the caption').

question_code(fruition_mode(user_mode), 'Do you want to perform a diagnosis about the health status of a plant').
explanation(fruition_mode(user_mode), 'Provide the symptoms that the plant exibits to understand if it\'s affected by any problem and to get eventual treatment instructions').
question_code(fruition_mode(kb_mode), 'Do you want to browse all the Knowledge Base facts').
explanation(fruition_mode(kb_mode), 'Shows all the informations about health problems, symptoms, treatments').
question_code(fruition_mode(sensor_mode), 'Do you want to enter the sensors\' mode').
explanation(fruition_mode(sensor_mode), 'Simulates the use of devices to test the automations of the system').
question_code(start_again, 'Would you like to run the program again').
question_code(new_symptom, 'Would you like to register a new symptom').
question_code(continue_time_loop, 'Do you want to continue the sampling').

question_code(view(X), X) :- write('Would you like to browse the ').
question_code(has(X), X) :- write('Does it have a particular ').
question_code(what(X), X) :- write('What ').

message_code(item_number_no_exit, 'Enter the entry number of your interest, followed by a dot (.).').
message_code(stop_selection, 'Stop by user selection.').
message_code(not_recognized_value, 'The selected value was not recognized. Try again.').
message_code(yes_or_no, 'Please answer yes or no.').
message_code(no_problem, 'There is no problem.').
message_code(no_diagnosis, 'There is no clear diagnosis.').
message_code(sections, 'What section is affected by the problem?').
message_code(appearances, 'How is the problem manifested?').
message_code(colors, 'What color does it have?').
message_code(behaviours, 'What is the behaviour?').
message_code(no_explanation, 'There is no explanation for this.').
message_code(diagnosis_of, '- The diagnosis is of ').
message_code(because_of, ', because of ').
message_code(no_symptom, 'There are no observed symptoms.').
message_code(option_selected, 'You have selected option n.').
message_code(missing_nutrient, 'Treatment: provide the missing nutrient to the plant.').
message_code(treatment, 'Treatment: ').
message_code(treatment_none, ' -> There a no treatments for this.').
message_code(treatment_healthy, ' -> The plant is perfectly healthy. There is no treatment to apply.').
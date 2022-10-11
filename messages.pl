% welcome/1
welcome :-
    writeln('Welcome to the greehouse management program by Francesco Peragine.'),
    writeln('You can either submit the symptoms manifested on your plants to get a diagnosis or browse the Knowledge Base.'),
    writeln('If there are any, the software will provide both diagnosis and suggestions about the treatment.'),
    writeln('Ethics & Privacy: no plants were harmed during the tests and no personal informations will be stored nor disclosed.'), nl.

goodbye :-
    nl, writeln('Thank you for using the greenhouse management program.'),
    writeln('See you next time. FP.').

% question_code(temperature_reading,'What is the temperature').
% question_code(humidity_reading,'What is the humidity').
% question_code(caption_reading,'What is the caption').

question_code(fruition_mode(user_consult), 'Do you want to perform a diagnosis about the health status of a plant').
explanation(fruition_mode(user_consult), 'Provide the symptoms that the plant exibits to understand if it\'s affected by any problem and to get eventual treatment instructions').
question_code(fruition_mode(kb_consult), 'Do you want to browse all the Knowledge Base facts').
explanation(fruition_mode(kb_consult), 'Shows all the informations about health problems, symptoms, treatments').
question_code(start_again, 'Would you like to run the program again').
question_code(new_symptom, 'Would you like to register a new symptom').

% question_code(observed_problem(no_problem), 'Is the plant is healthy').
% question_code(observed_problem(deficiency), 'Does the plant show signs of yellowing, growth issues or disformation').
% question_code(observed_problem(disease), 'Are there holes, black spots, necrotic lesions or any strange and not known phenomena').
% question_code(observed_problem(infestation), 'Are there any walking/flying/creeping visible bugs').

% question_code(mobility(low), 'Is the problem distributed over all the plant').
% question_code(mobility(high), 'Is the problem manifested only on the lower section of the plant').
% question_code(mobility(none), 'Is the problem manifested only on the upper section of the plant').

% question(problem_affect, 'Does the problem affect the ').

% question_code(section(all), 'Does the problem affect the whole plant').
% question_code(section(shoot), 'Does the problem affect the shoot (stem together with appendages and buds)').
% question_code(section(leaves), 'Does the problem affect the leaves').
% question_code(section(upper_side_older_leaves), 'Does the problem affect the leaves').
% question_code(section(leaves), 'Does the problem affect the leaves').
% question_code(section(roots), 'Does the problem affect the roots').
% question_code(section(stem), 'Does the problem affect the stem').
% question_code(section(buds), 'Does the problem affect the buds').
% question_code(section(fruits), 'Does the problem affect the fruits').

question_code(view(X), X) :- write('Would you like to browse the ').
question_code(has(X), X) :- write('Does it have a particular ').
question_code(what(X), X) :- write('What ').


% question_code(has_color, 'Does it have a particular color').
% question_code(what_color, 'What color  is it').
% question_code(has_smell, 'Does it smell funny').
% question_code(has_appearance, 'Does it look strange').
% question_code(what_appearance, 'What does it look like').
% question_code(has_behaviour, 'Does it have strange behaviour').
% question_code(what_behaviour, 'What strange behaviour does it have').
% question_code(has_growth, 'Does it have an altered growth').
% question_code(what_growth, 'What growth does it have').

% message_code(section_number, 'Enter the item number of the affected section of the plant from the list, followed by a dot (.). Press 0 to exit.').
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
message_code(no_symptom, 'There are no observed symptoms.').





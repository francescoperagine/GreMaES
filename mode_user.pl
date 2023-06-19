:- dynamic problem/1.
:- dynamic issue/1.

% user_start/0
user_start :-
    symptomatology,
    forward,
    backward,
    diagnosis.

% symptomatology/0
symptomatology :-
    repeat,
    build_symptom,
    !,
    not(once_again).

% once_again/0
once_again :- askif(new_symptom).

% build_symptom/0
% Initializes the questioning for the user to provide informations about the manifested symptoms
build_symptom :-
    unset_asked(new_symptom),
    ask_sign(Sign),
    ask_location(Sign,Location),
    ask_color(Sign,Color),
    save_observation(curr,symptom(Location,Sign,Color)).
    
% Reads the previously stored signs' list and initializes the menu for the selection from the list of elements
% Signs are independent from each other, whereas locations and colors are always dependent on signs.
% All signs are manifested on a location, but not all the signs show a peculiar color.
% If the options' lists are reduced to only one element, it's selected by default.

% ask_sign/1 (-Sign)
ask_sign(Sign) :-
    signs(Signs),
    ask_menu(signs,Signs,Sign).
% ask_location/2 (+Sign,-Location)
ask_location(Sign,Location) :-
    all(Location,sign_location(Sign,Location),Locations),
    length(Locations,L),
    (L > 1 -> ask_menu(sign_locations,Locations,Location) ; nth1(1,Locations,Location)).
% ask_color (+Sign,-Color)
ask_color(Sign,Color) :-
    all(Color,sign_color(Sign,Color),Colors),
    length(Colors,L),
    (L > 1 -> ask_menu(sign_colors,Colors,Color) ; nth1(1,Colors,Color)).
% ask_color (+Sign,-Color)
ask_color(Sign,none) :-
    \+ sign_color(Sign,_).

% ask_menu/3 (+MessageCode,+Menu,-Selection)
% Displays a menu and get user's selection
ask_menu(MessageCode,Menu,Selection) :-
    writeln_message(MessageCode),
    display_menu(Menu,1),
    repeat,
    read(Index),
    ask_menu_forward(Menu,Index,Selection).

% ask_menu_forward/3 (+Menu,+Index,-Selection)
% Returns Index's Selection.
ask_menu_forward(Menu,Index,Selection) :-
    nth1(Index,Menu,Selection).
ask_menu_forward(Menu,Index,Selection) :-
    \+ nth1(Index,Menu,Selection),
    writeln_message(not_recognized_value),
    !,
    fail.

% display_menu/2(+List,+Index)
% Helper predicate to display the menu options with their indexes
display_menu([],_).
display_menu([Option|Rest],Index) :-
    write(Index),write('. '),write(Option),nl,
    NewIndex is Index + 1,
    display_menu(Rest,NewIndex).
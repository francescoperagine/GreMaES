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
    ask_menu(Signs,Sign).
% ask_location/2 (+Sign,-Location)
ask_location(Sign,Location) :-
    all(Location,sign_location(Sign,Location),Locations),
    length(Locations,L),
    (L > 1 -> ask_menu(Locations,Location) ; nth1(1,Locations,Location)).
% ask_color (+Sign,-Color)
ask_color(Sign,Color) :-
    all(Color,sign_color(Sign,Color),Colors),
    length(Colors,L),
    (L > 1 -> ask_menu(Colors,Color) ; nth1(1,Colors,Color)).
% ask_color (+Sign,-Color)
ask_color(Sign,none) :-
    \+ sign_color(Sign,_).
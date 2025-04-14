/**
 * Projekt FLP 2024/25 - Turinguv stroj
 * Autor: Veronika Jirmusová
 * Login: xjirmu00
 * Akademicky rok: 2024/2025
 *
 * Popis:
 * Tento program simuluje chovani nedeterministickeho Turingova stroje.
 */

:- use_module(library(readutil)).
:- dynamic(rule/4).

start :-
    writeln('>>> start'),
    read_lines(Lines),
    writeln('>>> nacteny radky'),
    maplist(split_words, Lines, Split),
    writeln('>>> splitnuto'),
    maplist(writeln, Split),
    append(Rules, [[TapeLine]], Split),
    maplist(parse_rule, Rules),
    writeln('>>> zacinam simulaci'),
    simulate([], 'S', TapeLine).


% Čte všechny řádky ze standardního vstupu
read_lines(Lines) :-
    read_line_to_codes(user_input, Line),
    ( Line == end_of_file ->
        Lines = []
    ;
        Lines = [Line | Rest],
        read_lines(Rest)
    ).

% Rozdělení řádku na seznam slov podle mezer
split_words(LineCodes, Words) :-
    atom_codes(LineAtom, LineCodes),
    atomic_list_concat(Atoms, ' ', LineAtom),
    maplist(atom_chars, Atoms, Words).

% Parsování pravidla
parse_rule([S1, R1, S2, R2]) :-
    atom_chars(S, S1),
    atom_chars(R, R1),
    atom_chars(NS, S2),
    atom_chars(W, R2),
    (   W = ['L'] ; W = ['R']
    ->  assertz(rule(S, R, NS, W))         % Pohyb
    ;   assertz(rule(S, R, NS, W))         % Přepis znaku
    ).




% Simulace
simulate(Left, 'F', [H|R]) :-
    print_config(Left, 'F', H, R),
    halt(0).

simulate(Left, State, [H|R]) :-
    print_config(Left, State, H, R),
    format('>>> Hledam pravidlo pro stav: ~w a znak: ~w~n', [State, H]),
    (   rule(State, H, NewState, Action)  % ← bez []
    ->  format('>>> Nasel jsem: ~w -> ~w / ~w~n', [State, NewState, Action]),
        do_action(Action, Left, [H|R], NewLeft, NewHead, NewRight),
        simulate(NewLeft, NewState, [NewHead|NewRight])
    ;   writeln('Abnormalni zastaveni.'),
        halt(1)
    ).





print_config(Left, State, Head, Right) :-
    reverse(Left, Rev),
    append(Rev, [State, Head | Right], All),
    flatten(All, Flat),            % zajistí sloučení znaků/atomů
    print_chars(Flat).

print_chars([]) :- nl.
print_chars([H|T]) :- write(H), print_chars(T).


% Akce Turingova stroje
do_action(['L'], [L|Ls], [H|Rs], Ls, L, [H|Rs]).
do_action(['R'], Ls, [H, R|Rs], [H|Ls], R, Rs).
do_action([W], Ls, [_|Rs], Ls, W, Rs).  % Přepis znaku

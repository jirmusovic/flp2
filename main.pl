% main.pl – Nedeterministický Turingův stroj
:- dynamic rule/5.
:- use_module(library(readutil)).

initial('S').
accepting('F').
blank(' ').             % atom pro prázdný symbol (mezera)

start :-
    prompt(_, ''),      % vypnout interaktivní prompt
    read_lines(Lines),
    build(Lines, Rules, Tape),
    maplist(assertz, Rules),
    ( simulate('S', [], Tape, [], [], Out) ->
        print_all(Out),
        halt(0)
    ; writeln('Abnormalni zastaveni.'), halt(1)
    ).

% načte celé stdin jako seznam atomů (jedna řádka = jeden atom)
read_lines(Ls) :-
    read_line_to_codes(user_input, Codes),
    ( Codes == end_of_file ->
        Ls = []
    ; atom_codes(A, Codes),
      read_lines(Rest),
      Ls = [A|Rest]
    ).

% rozdělí seznam řádek na pravidla a pásku
build(Lines, Rules, Tape) :-
    append(RuleLines, [TapeAtom], Lines),
    maplist(parse_rule, RuleLines, Rules),
    atom_chars(TapeAtom, Tape).

%! parse_rule(+Line:atom, -rule(State,Symbol,NextState,Write,Dir))
% rozdělí surový řetězec podle prvních tří mezer tak, aby se zachovaly i empty‑tokeny
parse_rule(Line, rule(Q,Sym,NS,W,Dir)) :-
    atom_codes(Line, Cs),
    append(A, [32|R1], Cs),        % 32 = space
    append(B, [32|R2], R1),
    append(C, [32|D], R2),
    atom_codes(Q, A),              % původní stav
    ( B = [] -> Sym = ' '          % pokud mezi mezerami nic, je to blank
    ; atom_codes(Sym, B)
    ),
    atom_codes(NS, C),             % nový stav
    normalize_D(D, Ds),
    ( Ds = [76]   ->                % L
        Dir = left,  W = Sym
    ; Ds = [82]   ->                % R
        Dir = right, W = Sym
    ;                              % přepisovací pravidlo
        Dir = stay,
        ( Ds = [] -> W = ' ' ; atom_codes(W, Ds) )
    ).

% odstraní z D všechny kódy 32 (mezery), abychom rozeznali truly empty token
normalize_D(D, Ds) :-
    exclude(=(32), D, F),
    ( F = [] -> Ds = [] ; Ds = F ).

% simulace s detekcí zacyklení (Seen) – hledá první cestu do stavu 'F'
simulate('F', L, [H|R], Hist, _, Out) :-
    reverse(L, RevL),
    append(RevL, ['F',H|R], FinalConf),
    reverse([FinalConf|Hist], Out).

simulate(State, L, [H|R], Hist, Seen, Out) :-
    rule(State, H, Next, Write, Dir),
    move_head(Dir, L, [Write|R], NL, NR),
    reverse(L, RevL),
    append(RevL, [State,H|R], CurrConf),
    \+ member(CurrConf, Seen),
    simulate(Next, NL, NR, [CurrConf|Hist], [CurrConf|Seen], Out).

simulate(State, L, [], Hist, Seen, Out) :-
    blank(B),
    simulate(State, L, [B], Hist, Seen, Out).

% pohyb hlavy + doplnění blanku na okraji, pokud dojde páska
move_head(left,  [],     R,     [],    [B|R]) :- blank(B).
move_head(left,  [X|Xs], R,     Xs,    [X|R]).
move_head(right, L,      [],    [B|L], [])  :- blank(B).
move_head(right, L,      [X|Xs], [X|L], Xs).
move_head(stay,  L,      R,     L,     R).

% tisk výsledné posloupnosti konfigurací
print_all([]).
print_all([C|Cs]) :-
    maplist(write, C),
    nl,
    print_all(Cs).
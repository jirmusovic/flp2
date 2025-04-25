% main.pl — nondeterministic Turing Machine with full raw-debug and DCG parser
:- dynamic rule/5.
:- discontiguous simulate/5.
:- initialization(start).

% Entry point: swipl main.pl < vstup1.txt
start :-
    retractall(rule(_,_,_,_,_)),
    read_lines(Lines),
    Lines \= [],
    % RAW INPUT DUMP
    writeln('=== RAW INPUT LINES ==='),
    forall(member(L,Lines), writeln(L)),
    % Split rules vs tape
    append(RuleLines,[TapeAtom],Lines),
    writeln('=== RULE LINES ==='), forall(member(RL,RuleLines), writeln(RL)),
    writeln('=== TAPE LINE ==='), writeln(TapeAtom),
    % Parse rules via DCG
    (   maplist(parse_rule,RuleLines)
    ->  writeln('=== PARSE OK ===')
    ;   writeln('=== PARSE ERROR: bad rule syntax ==='), halt(2)
    ),
    atom_chars(TapeAtom,Tape),
    % DEBUG: final rule/5 table
    writeln('=== DEBUG: loaded rules ==='),
    forall(rule(Q,R,QS,WS,Dir),
           format('  ~w  ~w -> ~w  ~w  ~w~n',[Q,R,QS,WS,Dir])),
    writeln('=== DEBUG: initial tape ==='), writeln(Tape),
    initial(S0),
    % run simulation, reverse output for chronological order
    (   simulate(S0, [], Tape, [], RevOut)
    ->  reverse(RevOut, Out),
        print_all(Out), halt(0)
    ;   writeln('Abnormalni zastaveni.'), halt(1)
    ).

% Read all lines until EOF
read_lines(Ls) :-
    read_line_to_codes(user_input,Cs),
    ( Cs == end_of_file -> Ls = []
    ; atom_codes(L,Cs), read_lines(Rest), Ls = [L|Rest]
    ).

% DCG-based rule parser: "State ReadSym NewState [Action]"
parse_rule(Line) :-
    split_string(Line, " ", "", Parts0),
    normalize_parts(Parts0, Parts),
    ( Parts = [S, SymStr, NS, Act]
    ; Parts = [S, SymStr, NS], Act = "" ),
    atom_string(State, S), atom_string(NextState, NS),
    ( SymStr = "" -> Read = ' ' ; string_chars(SymStr, [Read]) ),
    ( Act = "L" -> Dir = left,  Write = Read
    ; Act = "R" -> Dir = right, Write = Read
    ; Act = ""  -> Dir = stay,  Write = Read
    ; string_chars(Act, [Wc]) -> Dir = stay, Write = Wc
    ),
    assertz(rule(State, Read, NextState, Write, Dir)).

% Normalize consecutive blank tokens ("") into a single one
normalize_parts([], []).
normalize_parts([A,B|T], Out) :-
    A = "", B = "", !,
    normalize_parts([A|T], Out).
normalize_parts([H|T], [H|Out]) :-
    normalize_parts(T, Out).

% DCG rules (unused in this version, retained for reference)
rule(State,Read,Next,Write,Dir) -->
    state_codes(SCs), " ",
    [SC], { (SC=:=32 -> Read=' ' ; char_code(Read,SC)) }, " ",
    state_codes(NSCs),
    ( " ", action(Read,Write,Dir) ; { Write=Read, Dir=stay } ),
    { atom_codes(State,SCs), atom_codes(Next,NSCs) }.

state_codes([C|Cs]) -->
    [C], { char_type(C, upper) },
    state_codes(Cs).
state_codes([]) --> [].

action(Read,Read,Dir) --> [76], { Dir=left }.
action(Read,Read,Dir) --> [82], { Dir=right }.
action(_, ' ',Dir) --> [32], { Dir=stay }.
action(_,W,Dir) --> [C], { char_code(W,C), Dir=stay }.

% Simulation: DFS until final state 'F'
simulate('F', L, R, Hist, [Conf|Hist]) :-
    reverse(L, RL),
    append(RL, ['F'|R], Conf).
simulate(St, L, [], Hist, Out) :-
    blank(B), simulate(St, L, [B], Hist, Out).
simulate(St, L, [H|T], Hist, Out) :-
    rule(St, H, NS, W, Dir),
    move(Dir, L, [W|T], NL, NR),
    reverse(L, RL),
    append(RL, [St,H|T], Conf),
    simulate(NS, NL, NR, [Conf|Hist], Out).

% Head movement
move(left,  [],    R,    [],      [' '|R]).
move(left,  [X|Xs],R,    Xs,      [X|R]).
move(right, L,     [],   [B|L],   []) :- blank(B).
move(right, L,     [X|Xs],[X|L],  Xs).
move(stay,  L,     R,    L,       R).

% Print configurations
print_all([]).
print_all([C|Cs]) :- maplist(write, C), nl, print_all(Cs).

% Defaults
initial('S').
blank(' ').
:- set_prolog_flag(double_quotes, chars).
:- dynamic ops/4.
:- dynamic '_main'/0.
:- dynamic debug_syntax/0.
:- dynamic ebnf_nonterminal/2.
:- assert(use(nothing)).

useid :- assert(use(id)).

% Debug mode control
enable_syntax_debug :- assert(debug_syntax).
disable_syntax_debug :- retractall(debug_syntax).

% Show all registered rules for debugging
show_registered_rules :-
    nl,
    write('=== REGISTERED RULES DEBUG OUTPUT ==='), nl,
    nl,
    % Show main clause
    write('--- Main Clause ---'), nl,
    (clause(main, Body) ->
        write('main :- '), write_canonical_debug(Body), write('.'), nl
    ;
        write('(no main clause found)'), nl
    ),
    nl,
    % Show rules for dynamically discovered functors
    show_dynamic_rules,
    % Show EBNF direct predicates
    write('--- EBNF Grammar Predicates ---'), nl,
    show_ebnf_predicates,
    nl,
    % Show operator definitions
    write('--- Operator Definitions ---'), nl,
    show_operators,
    write('=== END REGISTERED RULES ==='), nl,
    nl.

% Show rules for dynamically discovered functors
show_dynamic_rules :-
    findall(Functor/Arity, debug_rule_form(Functor/Arity, _), Functors),
    exclude(is_ebnf_predicate_pair, Functors, NonEBNFFunctors),
    sort(NonEBNFFunctors, SortedFunctors),
    maplist(show_categorized_rules, SortedFunctors).

% Check if a functor/arity pair represents an EBNF predicate
is_ebnf_predicate_pair(Functor/Arity) :-
    Arity = 1,
    is_ebnf_predicate(Functor).

% Check if a functor is an EBNF predicate (starts with _)
is_ebnf_predicate(Functor) :-
    atom_concat('_', _, Functor).

% Show rules for a specific functor with appropriate categorization
show_categorized_rules(Functor/Arity) :-
    % Determine category based on functor name patterns
    atom_concat('--- Rules (', Functor, Title1),
    atom_concat(Title1, ') ---', Title),
    write(Title), nl,
    show_rules_by_functor(Functor, Arity),
    nl.

% Show rules for a specific functor/arity
show_rules_by_functor(Functor, Arity) :-
    % First try to show debug rule forms if available
    findall(Rule, debug_rule_form(Functor/Arity, Rule), DebugRules),
    (DebugRules \= [] ->
        maplist(show_rule, DebugRules)
    ;
        % Fall back to regular rules
        functor(Head, Functor, Arity),
        findall((Head :- Body), clause(Head, Body), Rules),
        (Rules = [] ->
            write('(no rules found)'), nl
        ;
            maplist(show_rule, Rules)
        )
    ).

% Store debug rule form for display
store_debug_rule_form(Rule) :-
    (Rule = (Head :- _) ->
        functor(Head, Functor, Arity)
    ;
        functor(Rule, Functor, Arity)
    ),
    assertz(debug_rule_form(Functor/Arity, Rule)).

% Show a single rule
show_rule((Head :- true)) :-
    !,
    write_canonical_debug_with_readable_vars(Head), write('.'), nl.
show_rule((Head :- Body)) :-
    write_canonical_debug_with_readable_vars(Head), write(' :- '),
    write_canonical_debug_with_readable_vars(Body), write('.'), nl.

% Write canonical form with readable variable names
write_canonical_debug_with_readable_vars(Term) :-
    convert_vars_to_readable(Term, ReadableTerm),
    write_canonical_debug(ReadableTerm).

% Convert internal Prolog variables to readable $VAR form preserving original names
convert_vars_to_readable(Term, ReadableTerm) :-
    collect_var_names(Term, VarNameMap),
    apply_var_name_mapping(Term, VarNameMap, ReadableTerm).

% Collect original variable names from $VAR() terms
collect_var_names(Term, VarNameMap) :-
    collect_var_names(Term, [], VarNameMap).

collect_var_names(Term, Acc, VarNameMap) :-
    (var(Term) ->
        VarNameMap = Acc
    ; Term = '$VAR'(Name) ->
        (member(Term-Name, Acc) ->
            VarNameMap = Acc
        ;
            VarNameMap = [Term-Name|Acc]
        )
    ; atomic(Term) ->
        VarNameMap = Acc
    ; compound(Term) ->
        Term =.. [_|Args],
        collect_var_names_args(Args, Acc, VarNameMap)
    ;
        VarNameMap = Acc
    ).

collect_var_names_args([], Acc, Acc).
collect_var_names_args([Arg|Args], Acc, VarNameMap) :-
    collect_var_names(Arg, Acc, Acc1),
    collect_var_names_args(Args, Acc1, VarNameMap).

% Apply variable name mapping to term
apply_var_name_mapping(Term, VarNameMap, ReadableTerm) :-
    (var(Term) ->
        ReadableTerm = Term
    ; Term = '$VAR'(Name) ->
        (member(Term-Name, VarNameMap) ->
            ReadableTerm = Name
        ;
            ReadableTerm = Name
        )
    ; atomic(Term) ->
        ReadableTerm = Term
    ; compound(Term) ->
        Term =.. [Functor|Args],
        maplist(apply_var_name_mapping_arg(VarNameMap), Args, ReadableArgs),
        ReadableTerm =.. [Functor|ReadableArgs]
    ;
        ReadableTerm = Term
    ).

apply_var_name_mapping_arg(VarNameMap, Arg, ReadableArg) :-
    apply_var_name_mapping(Arg, VarNameMap, ReadableArg).

% Show EBNF direct predicates (_T, _V, etc.)
show_ebnf_predicates :-
    findall(NT, ebnf_nonterminal(NT, _), NTs),
    (NTs = [] ->
        write('(no EBNF non-terminals found)'), nl
    ;
        maplist(show_ebnf_predicate, NTs)
    ).

show_ebnf_predicate(NT) :-
    ebnf_nonterminal(NT, UC),
    atom_concat('_', UC, DirectUC),
    write('Non-terminal: '), write(NT), write(' -> '), write(UC), write(' (predicate: '), write(DirectUC), write(')'), nl,
    functor(Head, DirectUC, 1),
    findall((Head :- Body), clause(Head, Body), Rules),
    (Rules = [] ->
        write('  (no rules found)'), nl
    ;
        maplist(show_ebnf_rule, Rules)
    ).

show_ebnf_rule((Head :- true)) :-
    !,
    write('  '), write_ebnf_rule_with_meaningful_vars(Head), write('.'), nl.
show_ebnf_rule((Head :- Body)) :-
    write('  '), write_ebnf_rule_with_meaningful_vars(Head), write(' :- '), write_ebnf_rule_with_meaningful_vars(Body), write('.'), nl.

% Convert EBNF rules to use meaningful variable names
write_ebnf_rule_with_meaningful_vars(Term) :-
    convert_to_meaningful_ebnf_vars(Term, ReadableTerm),
    write_canonical_debug(ReadableTerm).

% Convert variables in EBNF rules to meaningful names
convert_to_meaningful_ebnf_vars(Term, ReadableTerm) :-
    % Use a more sophisticated approach that doesn't collapse duplicate variables
    convert_ebnf_vars_with_position(Term, ReadableTerm, 1, _).

% Convert EBNF variables with position-based naming
convert_ebnf_vars_with_position(Term, ReadableTerm, PosIn, PosOut) :-
    (var(Term) ->
        % Generate position-based variable name
        atom_concat(t, PosIn, ReadableTerm),
        PosOut is PosIn + 1
    ; atomic(Term) ->
        ReadableTerm = Term,
        PosOut = PosIn
    ; compound(Term) ->
        Term =.. [Functor|Args],
        convert_ebnf_args_with_position(Args, ReadableArgs, PosIn, PosOut),
        ReadableTerm =.. [Functor|ReadableArgs]
    ;
        ReadableTerm = Term,
        PosOut = PosIn
    ).

% Convert arguments with position tracking
convert_ebnf_args_with_position([], [], Pos, Pos).
convert_ebnf_args_with_position([Arg|Args], [ReadableArg|ReadableArgs], PosIn, PosOut) :-
    convert_ebnf_vars_with_position(Arg, ReadableArg, PosIn, PosNext),
    convert_ebnf_args_with_position(Args, ReadableArgs, PosNext, PosOut).

% Show operator definitions
show_operators :-
    findall(ops(Op, Prec, Type, Tokens), clause(ops(Op, Prec, Type, Tokens), true), Ops),
    (Ops = [] ->
        write('(no operators found)'), nl
    ;
        maplist(show_operator, Ops)
    ).

show_operator(ops(_Op, Prec, Type, Tokens)) :-
    write('op '), write(Prec), write(' : '), write_tokens(Tokens), write(' ('), write(Type), write(')'), nl.

write_tokens([]) :- !.
write_tokens([Token]) :- 
    !, 
    (Token = 99 -> write('_') ; write(Token)).
write_tokens([Token|Tokens]) :-
    (Token = 99 -> write('_') ; write(Token)),
    write(' '),
    write_tokens(Tokens).

% Write canonical form for debugging
write_canonical_debug(Term) :-
    (var(Term) ->
        write('_'), write(Term)
    ; Term = '$VAR'(Name) ->
        write(Name)
    ; is_list(Term) ->
        write('['), write_list_canonical(Term), write(']')
    ; compound(Term) ->
        Term =.. [Functor|Args],
        write(Functor), write('('),
        write_args_canonical(Args),
        write(')')
    ;
        write(Term)
    ).

write_list_canonical([]).
write_list_canonical([H]) :-
    !,
    write_canonical_debug(H).
write_list_canonical([H|T]) :-
    write_canonical_debug(H),
    write(','),
    write_list_canonical(T).

write_args_canonical([]).
write_args_canonical([Arg]) :-
    !,
    write_canonical_debug(Arg).
write_args_canonical([Arg|Args]) :-
    write_canonical_debug(Arg),
    write(','),
    write_args_canonical(Args).

debug_msg(Msg) :- 
    (debug_syntax -> (write(Msg), nl) ; true).

debug_msg(Format, Args) :- 
    (debug_syntax -> (format(Format, Args), nl) ; true).

% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens(Ts) --> comment, !, tokens(Ts).
tokens([T|Ts]) --> tok(T), !, tokens(Ts).
tokens([]) --> "".

% # から行末までがコメント
comment --> "#", string(_), "\n", !.
string([]) --> [].
string([X|Xs]) --> [X], string(Xs).

tok(N) --> num(N).
tok(;) --> ['\n'].
tok(;) --> ";".
tok('::=') --> "::", "=".
tok(Atom) --> puncts(Cs), {atom_chars(Atom, Cs)}.
tok(Atom) --> word(Cs), {length(Cs, N), N > 1, atom_chars(Atom, Cs)}.
tok(Atom) --> [C], {code_type(C, upper), atom_chars(Atom, [C])}.
tok(Alnum) --> alnum(Alnum).
tok(Alpha) --> alpha(Alpha).
num(N) --> digits(Cs), { number_chars(N, Cs) }.
word([C|Cs]) --> [C], { code_type(C, lower) }, word(Cs).
word([C]) --> [C], { code_type(C, lower) }.
alpha(T) --> [C], { code_type(C, lower), T = '$VAR'(C) }.
alnum(T) --> [C], [N], "'",
    { code_type(C, lower), code_type(N, digit), atom_concat(C, N, CN),
    atom_concat(CN, '\'', CND), T = '$VAR'(CND) }.
alnum(T) --> [C], [N],
    { code_type(C, lower), code_type(N, digit), atom_concat(C, N, CN), T = '$VAR'(CN) }.
alnum(T) --> [C], "'",
    { code_type(C, lower), atom_concat(C, '\'', CN), T = '$VAR'(CN) }.
puncts([C|Cs]) --> [C], { code_type(C, punct) }, puncts(Cs).
puncts([C]) -->  [C], { code_type(C, punct) }.
digits([C|Cs]) --> digit(C), digits(Cs).
digits([C])    --> digit(C).
digit(C)   --> [C], { code_type(C, digit) }.

skip(W) --> "" | (W, skip(W)).

% mixfix parser
% https://zenn.dev/pandaman64/books/pratt-parsing
% https://qiita.com/h_sakurai/items/40abccdaad4728e0602e

e(P,O)-->
    [U], 
    {not(U = ';'), not(U = '{'), not(U = '}') },
    t(P,U,O).
t(P,T,O)-->
    {ops(A, _, leading, [L|M])},
    {L=T},     
    f(M,V),             
    {call(A,V,W)},      
    t(P,W,O).           
t(P,T,O)-->   
    [U],      
    {not(U = ';'), not(U = '{'), not(U = '}') },
    {ops(A, _, following, [L,L2|M])},
    {L2=U, P<L},        
    f(M,V),             
    {call(A,[T|V],W)},   
    t(P,W,O).            
t(P,T,O)-->
    {ops(A, _, following, [L,L2|M])},
    {number(L2), P<L},
    g([L2|M],V),      
    {call(A,[T|V],W)},
    t(P,W,O)          
    ; {T=O}.          
f([P|N],R)-->         
    (
        {number(P),!},
        e(P,T),       
        {R=[T|I]}     
        ;[P],         
        {R=I}),       
    f(N,I).           
f([],[])-->!.         

g([P|N],R)-->         
    e1(P,T),         
    {R=[T|I]},       
    g(N,I).            
g([],[])-->!.          

e1(P,O)-->    
    [U],     
    {not(U = ';'), not(U = '{'), not(U = '}') },
    {number(U) ; variable(U); all_alpha(U); U = '('; U = '{'},
    t(P,U,O).

a(P,T,R) :- R=..[P|T].

variable(U) :- U = '$VAR'(_).

all_alpha(U) :-
    not(U = '$VAR'(_)),
    atom_chars(U, Cs), all_alpha_chars(Cs).

all_alpha_chars([C]) :- code_type(C, lower).
all_alpha_chars([C]) :- code_type(C, upper).
all_alpha_chars([C|Cs]) :- code_type(C, alpha), all_alpha_chars(Cs).

% parse
% rules ::= rule | rule rules
% rule ::= pred ';' | pred '{' body '}'
% body ::= pred ';' | pred ';' body
rules([R | Rs]) --> skip(";"), rul(R), {add_rule(R)}, rules(Rs).
rules([]) --> skip(";").

rul(useid) --> [use], [id], ";".
rul(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
rul(syntax_processed) --> [syntax], "{", syntax_rules(Rules), "}", {debug_msg('Processing syntax rules: ~w', [Rules]), catch(process_syntax_rules(Rules), Error, (debug_msg('Error in process_syntax_rules: ~w', [Error]), fail)), debug_msg('Syntax rules processed successfully')}.
rul(P :- true) --> pred(P), ";".
rul(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

notation([R]) --> [R], {not(R = ';')}.
notation([R|Rs]) --> [R], {not(R = ';')}, notation(Rs).

pred(O) --> e(0, O).

body(P) --> pred(P), skip(";").
body((P,Bs)) --> pred(P), ";", body(Bs).

% syntax block parsing
syntax_rules([Rule|Rules]) --> skip(";"), syntax_rule(Rule), syntax_rules(Rules).
syntax_rules([]) --> skip(";").

syntax_rule(rule(NT, Productions)) --> nonterminal(NT), ['::='], productions(Productions).

nonterminal(NT) --> ['$VAR'(V)], {atom_chars(V, [C]), code_type(C, lower), NT = V}.
nonterminal(NT) --> [NT], {atom(NT), atom_chars(NT, [C]), code_type(C, lower)}.

% Manual implementation for debugging
productions(Result, Input, Output) :-
    production(P1, Input, Rest1),
    (Rest1 = ['|'|Rest2] ->
        productions(Ps, Rest2, Output),
        Result = [P1|Ps]
    ;
        Result = [P1],
        Output = Rest1
    ).

% Production uses ron's expression parser for complex terms
production(Expr) --> expression(Expr).

% Parse expression using ron's mixfix parser
expression(Expr) --> e(0, Expr).

production_item(nt(X)) --> ['$VAR'(V)], {atom_chars(V, [C]), code_type(C, lower), X = V}.
production_item(nt(X)) --> [X], {atom(X), atom_chars(X, [C]), code_type(C, lower)}.
production_item(term(X)) --> [X], {X \= '$VAR'(_), X \= '|', not((atom_chars(X, [C]), code_type(C, lower)))}.

% make rules (old version - will be refactored)
add_rules([R]) :- add_rule(R).
add_rules([R | Rs]) :- add_rule(R), add_rules(Rs).

% Apply rules
assert_all_rules([]).
assert_all_rules([Rule|Rules]) :-
    apply_single_rule(Rule),
    assert_all_rules(Rules).

% Apply a single rule - handles both old and new rule formats
apply_single_rule(fact(Term)) :-
    assert(Term),
    debug_msg('Applied fact: ~w', [Term]).
apply_single_rule(rule_template(var_rule, Head, Var)) :-
    apply_extracted_ebnf_rule(rule_template(var_rule, Head, Var)).
apply_single_rule(rule_template(compound_rule, Head, CanonicalProduction)) :-
    apply_extracted_ebnf_rule(rule_template(compound_rule, Head, CanonicalProduction)).
apply_single_rule(Rule) :-
    % Fallback to old add_rule for compatibility
    add_rule(Rule).

% New EBNF rule generation functions (Stage 1)
% Extract EBNF rules without immediately asserting them
extract_ebnf_rules_from_syntax(SyntaxRules, GeneratedRules) :-
    debug_msg('=== Extracting EBNF rules ==='),
    group_rules_by_nt(SyntaxRules, GroupedRules),
    extract_grouped_ebnf_rules(GroupedRules, GeneratedRules),
    debug_msg('=== EBNF rule extraction complete ===').

% Extract rules from grouped syntax rules
extract_grouped_ebnf_rules([], []).
extract_grouped_ebnf_rules([rule(NT, Productions)|Rules], AllGenerated) :-
    extract_single_nt_rules(NT, Productions, NTGenerated),
    extract_grouped_ebnf_rules(Rules, RestGenerated),
    append(NTGenerated, RestGenerated, AllGenerated).

% Extract rules for a single non-terminal
extract_single_nt_rules(NT, Productions, GeneratedRules) :-
    get_nt_char(NT, C),
    code_type(C, lower),
    char_code(C, Code),
    UpperCode is Code - 32,
    char_code(UC, UpperCode),
    debug_msg('Extracting rules for non-terminal: ~w -> ~w', [NT, UC]),
    debug_msg('Productions: ~w', [Productions]),

    % Generate the rules that would be created
    % 1. Non-terminal registration fact
    NonTerminalFact = ebnf_nonterminal(NT, UC),

    % 2. Operator definition
    OpTerm = ops(a(UC), 99, following, [99, UC]),

    % 3. Direct predicate rules
    extract_direct_nt_rules(UC, Productions, DirectRules),

    % Combine all generated rules
    GeneratedRules = [fact(NonTerminalFact), fact(OpTerm) | DirectRules],

    debug_msg('Generated ~w rules for ~w', [length(GeneratedRules), NT]).

% Extract direct predicate rules without asserting them
extract_direct_nt_rules(UC, Productions, DirectRules) :-
    atom_concat('_', UC, DirectUC),
    maplist(extract_direct_nt_rule(DirectUC), Productions, DirectRulesList),
    append(DirectRulesList, DirectRules),
    debug_msg('Extracted direct predicate rules for: ~w', [DirectUC]).

% Extract a single direct rule
extract_direct_nt_rule(DirectUC, Production, Rules) :-
    (atom(Production) ->
        % Simple terminal: _T(true) :- true
        Fact =.. [DirectUC, Production],
        Rules = [fact(Fact)],
        debug_msg('  Extracted fact: ~w', [Fact])
    ; Production = '$VAR'(Var) ->
        % Variable: _T(X) :- _V(X)
        % Note: We need to look up the variable's UC, but since we're extracting,
        % we'll generate the rule structure and handle variables later
        Head =.. [DirectUC, '$VAR'('X')],  % Use generic variable
        % We'll need to resolve the VarDirectUC during rule application
        % For now, store enough info to reconstruct the rule
        Rules = [rule_template(var_rule, Head, Var)],
        debug_msg('  Extracted variable rule template for: ~w', [Var])
    ; compound(Production) ->
        % Compound: _T(if(X,Y,Z)) :- _T(X), _T(Y), _T(Z)
        canonical(Production, CanonicalProduction),
        CanonicalProduction =.. [_Functor|_Args],  % Use _ prefix to suppress warnings
        % For now, create a simplified compound rule template
        Head =.. [DirectUC, '$VAR'('CompoundResult')],
        Rules = [rule_template(compound_rule, Head, CanonicalProduction)],
        debug_msg('  Extracted compound rule template: ~w', [CanonicalProduction])
    ;
        % Fallback
        Rules = [],
        debug_msg('  Skipped production: ~w', [Production])
    ).

% Apply extracted EBNF rules
apply_extracted_ebnf_rules([]).
apply_extracted_ebnf_rules([Rule|Rules]) :-
    apply_extracted_ebnf_rule(Rule),
    apply_extracted_ebnf_rules(Rules).

% Apply a single extracted rule
apply_extracted_ebnf_rule(fact(Term)) :-
    assert(Term),
    debug_msg('Applied fact: ~w', [Term]).
apply_extracted_ebnf_rule(rule_template(var_rule, Head, Var)) :-
    % Resolve variable rule template
    Head =.. [_DirectUC, X],  % Use _ prefix to suppress warning
    (ebnf_nonterminal(Var, VarUC) ->
        atom_concat('_', VarUC, VarDirectUC),
        Body =.. [VarDirectUC, X],
        Rule = (Head :- Body),
        store_debug_rule_form(Rule),
        assert(Rule),
        debug_msg('Applied variable rule: ~w', [Rule])
    ;
        debug_msg('Warning: Could not resolve variable ~w in rule template', [Var])
    ).
apply_extracted_ebnf_rule(rule_template(compound_rule, _Head, CanonicalProduction)) :-
    % For now, create a simplified compound rule
    % This would need more sophisticated implementation for full compound handling
    debug_msg('Skipping compound rule template for now: ~w', [CanonicalProduction]).
apply_extracted_ebnf_rule(Rule) :-
    debug_msg('Unknown rule type: ~w', [Rule]).

% Stage 2: Unified rule transformation logic
% Categorize parsed rules by type
categorize_rules([], [], [], [], []).
categorize_rules([Rule|Rules], NormalRules, SyntaxRules, OpRules, SpecialRules) :-
    categorize_single_rule(Rule, Category),
    (Category = normal ->
        NormalRules = [Rule|RestNormal],
        RestSyntax = SyntaxRules,
        RestOp = OpRules,
        RestSpecial = SpecialRules
    ; Category = syntax ->
        SyntaxRules = [Rule|RestSyntax],
        RestNormal = NormalRules,
        RestOp = OpRules,
        RestSpecial = SpecialRules
    ; Category = operator ->
        OpRules = [Rule|RestOp],
        RestNormal = NormalRules,
        RestSyntax = SyntaxRules,
        RestSpecial = SpecialRules
    ; Category = special ->
        SpecialRules = [Rule|RestSpecial],
        RestNormal = NormalRules,
        RestSyntax = SyntaxRules,
        RestOp = OpRules
    ),
    categorize_rules(Rules, RestNormal, RestSyntax, RestOp, RestSpecial).

% Categorize a single rule
categorize_single_rule(syntax_processed, syntax).
categorize_single_rule(syntax(_), syntax).
categorize_single_rule(op(_, _), operator).
categorize_single_rule(useid, special).
categorize_single_rule(_ :- _, normal).
categorize_single_rule(_, normal).  % Default fallback

% Unified rule transformation pipeline
transform_rules_unified(ParsedRules, TransformedRules) :-
    debug_msg('=== Starting unified rule transformation ==='),
    % Step 1: Categorize rules
    categorize_rules(ParsedRules, NormalRules, SyntaxRules, OpRules, SpecialRules),
    length(NormalRules, NormalLen),
    length(SyntaxRules, SyntaxLen),
    length(OpRules, OpLen),
    length(SpecialRules, SpecialLen),
    debug_msg('Categorized: Normal=~w, Syntax=~w, Op=~w, Special=~w',
              [NormalLen, SyntaxLen, OpLen, SpecialLen]),

    % Step 2: Process each category
    process_special_rules(SpecialRules, ProcessedSpecial),
    process_operator_rules(OpRules, ProcessedOp),
    process_syntax_rules_new(SyntaxRules, ProcessedSyntax),
    process_normal_rules(NormalRules, ProcessedNormal),

    % Step 3: Combine all processed rules
    append([ProcessedSpecial, ProcessedOp, ProcessedSyntax, ProcessedNormal], TransformedRules),
    length(TransformedRules, TotalLen),
    debug_msg('=== Unified transformation complete: ~w total rules ===', [TotalLen]).

% Process special rules (like useid)
process_special_rules([], []).
process_special_rules([Rule|Rules], [Rule|ProcessedRules]) :-
    debug_msg('Processing special rule: ~w', [Rule]),
    process_special_rules(Rules, ProcessedRules).

% Process operator rules
process_operator_rules([], []).
process_operator_rules([Rule|Rules], [Rule|ProcessedRules]) :-
    debug_msg('Processing operator rule: ~w', [Rule]),
    process_operator_rules(Rules, ProcessedRules).

% Process syntax rules using our new extraction functions
process_syntax_rules_new([], []).
process_syntax_rules_new([syntax_processed|Rules], AllProcessed) :-
    % For now, return empty since syntax_processed rules are handled during parsing
    % In the future, we would extract the syntax rules and process them here
    debug_msg('Processing syntax_processed rule'),
    process_syntax_rules_new(Rules, AllProcessed).
process_syntax_rules_new([syntax(SyntaxRules)|Rules], AllProcessed) :-
    % Process syntax rules using our extraction functions
    debug_msg('Processing syntax rule with ~w rules', [length(SyntaxRules)]),
    extract_ebnf_rules_from_syntax(SyntaxRules, GeneratedRules),
    process_syntax_rules_new(Rules, RestProcessed),
    append(GeneratedRules, RestProcessed, AllProcessed).

% Process normal rules (predicates and facts)
process_normal_rules([], []).
process_normal_rules([Rule|Rules], [Rule|ProcessedRules]) :-
    debug_msg('Processing normal rule: ~w', [Rule]),
    process_normal_rules(Rules, ProcessedRules).

% Test function for new pipeline (Stage 3 preparation)
test_new_pipeline(Code) :-
    debug_msg('=== Testing new pipeline ==='),
    code_rules(Code, ParsedRules),
    debug_msg('Parsed ~w rules', [length(ParsedRules)]),
    transform_rules_unified(ParsedRules, TransformedRules),
    debug_msg('Transformed to ~w rules', [length(TransformedRules)]),
    debug_msg('=== Test complete ===').

% Experimental code_mi using new pipeline (Stage 3)
code_mi_experimental(Code) :-
    debug_msg('=== Using experimental pipeline ==='),
    code_rules(Code, ParsedRules),
    !,
    % Use new unified transformation
    transform_rules_unified(ParsedRules, TransformedRules),
    assert_all_rules(TransformedRules),
    % Show registered rules before main query if debug is enabled
    (debug_syntax -> show_registered_rules ; true),
    query(main).

% Pure parser without side effects (Stage 3)
code_rules_pure(Code, Rules) :-
    code_tokens(Code, Tokens),
    phrase(rules_pure(Rules), Tokens).

% Pure DCG parser without {add_rule(R)} calls
rules_pure([R | Rs]) --> skip(";"), rul_pure(R), rules_pure(Rs).
rules_pure([]) --> skip(";").

% Pure rule parsing that doesn't immediately process EBNF
rul_pure(useid) --> [use], [id], ";".
rul_pure(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
rul_pure(syntax(SyntaxRules)) --> [syntax], "{", syntax_rules(SyntaxRules), "}".
rul_pure(P :- true) --> pred(P), ";".
rul_pure(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

% Fully pure code_mi using pure parser and new pipeline (Stage 3)
code_mi_pure(Code) :-
    debug_msg('=== Using fully pure pipeline ==='),
    code_rules_pure(Code, ParsedRules),
    !,
    % Use new unified transformation
    transform_rules_unified(ParsedRules, TransformedRules),
    assert_all_rules(TransformedRules),
    % Show registered rules before main query if debug is enabled
    (debug_syntax -> show_registered_rules ; true),
    query(main).


add_rule(useid) :-
    useid.
add_rule(syntax_processed) :-
    true.  % Rules already processed during parsing
add_rule(op(Prec, ['_' | Ns])) :-
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    assert(Term).
add_rule(op(Prec, [N | Ns])) :-
    N \= '_',
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    assert(Term).
add_rule(main :- Body) :-
    %get_time(T),
    %write('a '), writeln(T),
    canonical2(Body, BodyC), % writeln('before cut'), !, writeln('after cut'),
    %write('main BodyC '), writeln(BodyC),
    % Add EBNF constraints to main clause too
    add_ebnf_constraints_to_canonical_rule(main, BodyC, NewBodyC),
    assert(main :- NewBodyC).
add_rule(Head :- Body) :-
    canonical(Head, HeadC),
    canonical(Body, BodyC),
    % Check if canonical rule contains EBNF variables and add constraints after canonicalization
    add_ebnf_constraints_to_canonical_rule(HeadC, BodyC, NewBodyC),
    % Store original form for debug display
    store_debug_rule_form(HeadC :- NewBodyC),
    varnumbers_names(HeadC :- NewBodyC, Term, _),
    %write('rule3: '), writeln(HeadC :- NewBodyC), sleep(0.1),
    assert(Term).

% () はあらかじめ定義しておく
ops(a('()'), 0, leading, ['(',0,')']).

% (( )) は prolog を参照することにする
ops(a('<>'), 0, leading, ['<',0,'>']).

% == operator support (maps to built-in ==)
'_=='(A, B) :- A = B.

% Prolog Term としての標準記法に変換
canonical((B, Bs), (P, Ps)) :-
    canonical(B, P),
    canonical(Bs, Ps).
canonical(Term, Canonical) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, canonical(Arg, Canonical)
    % <a> は a にするが、functor の先頭に _ をつけない
    ; Functor = '<>' -> [Arg] = Args, Arg = Canonical
    % $VAR はそのまま
    ; Functor = '$VAR' -> Canonical = Term
    % それ以外は Functor の先頭に _ をつける
    ; atomic_concat('_', Functor, FunctorC),
                        canonicalList(Args, ArgsC),
                        Canonical =.. [FunctorC | ArgsC]).  
canonical(T, T).

canonicalList([], []).
canonicalList([A|As], [C|Cs]) :-
    canonical(A,C),
    canonicalList(As, Cs).


canonical2((B, Bs), (P, Ps)) :-
    canonical2(B, P),
    canonical2(Bs, Ps).
canonical2(Term, Canonical) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, canonical2(Arg, Canonical)
    % <a> は a にするが、functor の先頭に _ をつけない
    ; Functor = '<>' -> [Arg] = Args, Arg = Canonical
    % $VAR は、$VAR(n) だけ n にする。他はそのまま
    %; Functor = '$VAR' -> Canonical = Term
    ; Functor = '$VAR' -> ([Arg] = Args, not(member(Arg, [t,u,v,w,x,y,z])) -> Canonical = Arg; Canonical = Term)
    % それ以外は Functor の先頭に _ をつける
    ; atomic_concat('_', Functor, FunctorC),
                        canonicalList2(Args, ArgsC),
                        Canonical =.. [FunctorC | ArgsC]).  
canonical2(T, T).

canonicalList2([], []).
canonicalList2([A|As], [C|Cs]) :-
    canonical2(A,C),
    canonicalList2(As, Cs).

replaceUnderScore([A|As], R, [R_|Bs]) :-
    replaceUnderScore(A, R, R_),
    replaceUnderScore(As, R, Bs).
replaceUnderScore([A], R, [R_]) :-
    replaceUnderScore(A, R, R_).
replaceUnderScore([], _, []).
replaceUnderScore('_', R, R).
replaceUnderScore(A, _, A) :- A \= '_'.

pickPunct(['_'|Ns], Punct) :-
    pickPunct(Ns, Punct).
pickPunct([N|Ns], Punct) :-
    N \= '_',
    pickPunct(Ns, Ps),
    atom_concat(N, Ps, Punct).
pickPunct([], '').

% EBNF syntax processing
process_syntax_rules([]).
process_syntax_rules(Rules) :-
    % Group rules by non-terminal to avoid duplicates
    group_rules_by_nt(Rules, GroupedRules),
    process_grouped_syntax_rules(GroupedRules).

% Group rules by non-terminal
group_rules_by_nt([], []).
group_rules_by_nt([rule(NT, Productions)|Rules], [rule(NT, AllProductions)|GroupedRules]) :-
    collect_productions_for_nt(Rules, NT, MoreProductions, RemainingRules),
    append(Productions, MoreProductions, AllProductions),
    group_rules_by_nt(RemainingRules, GroupedRules).

% Collect all productions for a specific non-terminal
collect_productions_for_nt([], _, [], []).
collect_productions_for_nt([rule(NT, Productions)|Rules], NT, AllProductions, RestRules) :-
    !,
    collect_productions_for_nt(Rules, NT, MoreProductions, RestRules),
    append(Productions, MoreProductions, AllProductions).
collect_productions_for_nt([Rule|Rules], NT, Productions, [Rule|RestRules]) :-
    collect_productions_for_nt(Rules, NT, Productions, RestRules).

% Process grouped syntax rules  
process_grouped_syntax_rules([]).
process_grouped_syntax_rules([rule(NT, Productions)|Rules]) :-
    get_nt_char(NT, C),
    code_type(C, lower),
    char_code(C, Code),
    UpperCode is Code - 32,
    char_code(UC, UpperCode),
    debug_msg('Processing non-terminal: ~w -> ~w', [NT, UC]),
    debug_msg('Productions: ~w', [Productions]),
    % Record non-terminal for rule transformation
    assert(ebnf_nonterminal(NT, UC)),
    % Generate operator for non-terminal recognition (op 99: UC _)
    OpTerm = ops(a(UC), 99, following, [99, UC]),
    assert(OpTerm),
    debug_msg('Generated operator: op 99: ~w _', [UC]),
    % Create direct predicate rules for easier access
    create_direct_nt_rules(UC, Productions),
    process_syntax_rules(Rules).



% Transform a single rule by adding EBNF constraints
transform_rule(rule(Head, Body)) :-
    debug_msg('Analyzing rule: ~w :- ~w', [Head, Body]),
    collect_ebnf_variables(Head, HeadVars),
    collect_ebnf_variables(Body, BodyVars),
    append(HeadVars, BodyVars, AllVars),
    (AllVars \= [] ->
        debug_msg('Found EBNF variables: ~w', [AllVars]),
        generate_ebnf_constraints(AllVars, Constraints),
        (Constraints \= [] ->
            debug_msg('Generated constraints: ~w', [Constraints]),
            list_to_conjunction(Constraints, ConstraintConj),
            NewBody = (Body, ConstraintConj),
            debug_msg('Transformed rule: ~w :- ~w', [Head, NewBody]),
            % Retract old rule and assert new one with constraints
            retract_and_assert_rule(Head, Body, NewBody)
        ;
            debug_msg('No constraints needed')
        )
    ;
        debug_msg('No EBNF variables found')
    ).

% Collect variables that match EBNF non-terminals
collect_ebnf_variables(Term, Vars) :-
    collect_ebnf_vars_helper(Term, [], Vars).

collect_ebnf_vars_helper(Term, Acc, Vars) :-
    (var(Term) ->
        Vars = Acc
    ; Term = '$VAR'(VarName) ->
        (ebnf_nonterminal(VarName, _) ->
            (member(Term, Acc) -> Vars = Acc ; Vars = [Term|Acc])
        ;
            Vars = Acc
        )
    ; atom(Term) ->
        (ebnf_nonterminal(Term, _) ->
            AtomVar = '$VAR'(Term),
            (member(AtomVar, Acc) -> Vars = Acc ; Vars = [AtomVar|Acc])
        ;
            Vars = Acc
        )
    ; compound(Term) ->
        Term =.. [_|Args],
        collect_ebnf_vars_list(Args, Acc, Vars)
    ;
        Vars = Acc
    ).

collect_ebnf_vars_list([], Vars, Vars).
collect_ebnf_vars_list([Arg|Args], Acc, Vars) :-
    collect_ebnf_vars_helper(Arg, Acc, Acc1),
    collect_ebnf_vars_list(Args, Acc1, Vars).

% Generate EBNF constraints for variables
generate_ebnf_constraints([], []).
generate_ebnf_constraints([Var|Vars], [Constraint|Constraints]) :-
    generate_ebnf_constraint(Var, Constraint),
    generate_ebnf_constraints(Vars, Constraints).

generate_ebnf_constraint('$VAR'(VarName), Constraint) :-
    ebnf_nonterminal(VarName, UC),
    Constraint =.. [UC, '$VAR'(VarName)].
generate_ebnf_constraint(Var, Constraint) :-
    atom(Var),
    ebnf_nonterminal(Var, UC),
    Constraint =.. [UC, Var].

% Safely retract and assert transformed rule
retract_and_assert_rule(Head, OldBody, NewBody) :-
    (retract((Head :- OldBody)) ->
        canonical2(Head, HeadC),
        canonical2(NewBody, NewBodyC),
        varnumbers_names((HeadC :- NewBodyC), Term, _),
        assert(Term),
        debug_msg('Successfully transformed rule')
    ;
        debug_msg('Could not retract original rule')
    ).

% Add EBNF constraints during rule processing (before canonical transformation)
add_ebnf_constraints_to_rule(Head, Body, NewBody) :-
    collect_ebnf_variables_pre_canonical(Head, HeadVars),
    collect_ebnf_variables_pre_canonical(Body, BodyVars),
    append(HeadVars, BodyVars, AllVars),
    (AllVars \= [] ->
        debug_msg('Adding EBNF constraints to rule: ~w :- ~w', [Head, Body]),
        debug_msg('Found EBNF variables: ~w', [AllVars]),
        generate_ebnf_constraints_pre_canonical(AllVars, Constraints),
        (Constraints \= [] ->
            debug_msg('Generated constraints: ~w', [Constraints]),
            list_to_conjunction(Constraints, ConstraintConj),
            NewBody = (Body, ConstraintConj),
            debug_msg('Rule with constraints: ~w :- ~w', [Head, NewBody])
        ;
            NewBody = Body
        )
    ;
        NewBody = Body
    ).

% Collect EBNF variables before canonical transformation
collect_ebnf_variables_pre_canonical(Term, Vars) :-
    collect_ebnf_vars_pre_helper(Term, [], Vars).

collect_ebnf_vars_pre_helper(Term, Acc, Vars) :-
    (var(Term) ->
        Vars = Acc
    ; Term = '$VAR'(VarName) ->
        (ebnf_nonterminal(VarName, _) ->
            (member(Term, Acc) -> Vars = Acc ; Vars = [Term|Acc])
        ;
            Vars = Acc
        )
    ; atom(Term) ->
        (ebnf_nonterminal(Term, _) ->
            EbnfVar = '$VAR'(Term),
            (member(EbnfVar, Acc) -> Vars = Acc ; Vars = [EbnfVar|Acc])
        ;
            Vars = Acc
        )
    ; compound(Term) ->
        Term =.. [_|Args],
        collect_ebnf_vars_pre_list(Args, Acc, Vars)
    ;
        Vars = Acc
    ).

collect_ebnf_vars_pre_list([], Vars, Vars).
collect_ebnf_vars_pre_list([Arg|Args], Acc, Vars) :-
    collect_ebnf_vars_pre_helper(Arg, Acc, Acc1),
    collect_ebnf_vars_pre_list(Args, Acc1, Vars).

% Generate constraints before canonical transformation
generate_ebnf_constraints_pre_canonical([], []).
generate_ebnf_constraints_pre_canonical([Var|Vars], [Constraint|Constraints]) :-
    generate_ebnf_constraint_pre_canonical(Var, Constraint),
    generate_ebnf_constraints_pre_canonical(Vars, Constraints).

generate_ebnf_constraint_pre_canonical('$VAR'(VarName), Constraint) :-
    ebnf_nonterminal(VarName, UC),
    % Use direct _UC predicate
    atom_concat('_', UC, DirectUC),
    Constraint =.. [DirectUC, '$VAR'(VarName)].
generate_ebnf_constraint_pre_canonical(Var, Constraint) :-
    atom(Var),
    ebnf_nonterminal(Var, UC),
    % Use direct _UC predicate
    atom_concat('_', UC, DirectUC),
    Constraint =.. [DirectUC, Var].

% Remove duplicates from variable list based on variable names
remove_var_duplicates([], []).
remove_var_duplicates([Var|Rest], Result) :-
    (Var = '$VAR'(Name) ->
        filter_var_name(Rest, Name, Filtered),
        remove_var_duplicates(Filtered, RestResult),
        Result = [Var|RestResult]
    ;
        remove_var_duplicates(Rest, RestResult),
        Result = [Var|RestResult]
    ).

filter_var_name([], _, []).
filter_var_name([Var|Rest], Name, Result) :-
    (Var = '$VAR'(Name) ->
        filter_var_name(Rest, Name, Result)
    ;
        filter_var_name(Rest, Name, RestResult),
        Result = [Var|RestResult]
    ).

% Add EBNF constraints after canonical transformation
add_ebnf_constraints_to_canonical_rule(HeadC, BodyC, NewBodyC) :-
    collect_canonical_ebnf_variables(HeadC, HeadVars),
    collect_canonical_ebnf_variables(BodyC, BodyVars),
    append(HeadVars, BodyVars, AllVarsList),
    remove_var_duplicates(AllVarsList, AllVars),
    (AllVars \= [] ->
        debug_msg('Adding EBNF constraints to canonical rule: ~w :- ~w', [HeadC, BodyC]),
        debug_msg('Found canonical EBNF variables: ~w', [AllVars]),
        generate_canonical_ebnf_constraints(AllVars, Constraints),
        (Constraints \= [] ->
            debug_msg('Generated canonical constraints: ~w', [Constraints]),
            list_to_conjunction(Constraints, ConstraintConj),
            NewBodyC = (BodyC, ConstraintConj),
            debug_msg('Canonical rule with constraints: ~w :- ~w', [HeadC, NewBodyC])
        ;
            NewBodyC = BodyC
        )
    ;
        NewBodyC = BodyC
    ).

% Check if a variable name is an EBNF meta variable (non-terminal + digits or apostrophes)
is_ebnf_meta_variable(VarName) :-
    atom(VarName),
    atom_chars(VarName, Chars),
    Chars = [FirstChar|RestChars],
    % First character should be a lowercase letter that is a non-terminal
    code_type(FirstChar, alpha),
    code_type(FirstChar, lower),
    atom_chars(NTAtom, [FirstChar]),
    ebnf_nonterminal(NTAtom, _),
    % Rest should be either digits or apostrophes (or combination)
    (all_digits(RestChars) ; all_apostrophes(RestChars) ; digits_and_apostrophes(RestChars)),
    RestChars \= [].  % Must have at least one suffix character

% Check if all characters in a list are digits
all_digits([]).
all_digits([C|Cs]) :-
    code_type(C, digit),
    all_digits(Cs).

% Check if all characters in a list are apostrophes
all_apostrophes([]).
all_apostrophes([''''|Cs]) :-
    all_apostrophes(Cs).

% Check if all characters are either digits or apostrophes
digits_and_apostrophes([]).
digits_and_apostrophes([C|Cs]) :-
    (code_type(C, digit) ; C = ''''),
    digits_and_apostrophes(Cs).

% Collect EBNF variables from canonical terms
collect_canonical_ebnf_variables(Term, Vars) :-
    collect_canonical_ebnf_vars_helper(Term, [], Vars).

collect_canonical_ebnf_vars_helper(Term, Acc, Vars) :-
    (var(Term) ->
        Vars = Acc
    ; Term = '$VAR'(VarName) ->
        % Check if this is an EBNF variable after canonical transformation
        (ebnf_nonterminal(VarName, _) ->
            (member(Term, Acc) -> Vars = Acc ; Vars = [Term|Acc])
        ; is_ebnf_meta_variable(VarName) ->
            % Handle meta variables like t1, t2 where t is a non-terminal
            (member(Term, Acc) -> Vars = Acc ; Vars = [Term|Acc])
        ;
            Vars = Acc
        )
    ; atom(Term) ->
        % Check if this is an EBNF non-terminal atom
        (ebnf_nonterminal(Term, _) ->
            EbnfVar = '$VAR'(Term),
            (member(EbnfVar, Acc) -> Vars = Acc ; Vars = [EbnfVar|Acc])
        ; is_ebnf_meta_variable(Term) ->
            % Handle meta variable atoms like t1, t2
            EbnfVar = '$VAR'(Term),
            (member(EbnfVar, Acc) -> Vars = Acc ; Vars = [EbnfVar|Acc])
        ;
            Vars = Acc
        )
    ; compound(Term) ->
        Term =.. [_|Args],
        collect_canonical_ebnf_vars_list(Args, Acc, Vars)
    ;
        Vars = Acc
    ).

collect_canonical_ebnf_vars_list([], Vars, Vars).
collect_canonical_ebnf_vars_list([Arg|Args], Acc, Vars) :-
    collect_canonical_ebnf_vars_helper(Arg, Acc, Acc1),
    collect_canonical_ebnf_vars_list(Args, Acc1, Vars).

% Generate EBNF constraints for canonical variables
generate_canonical_ebnf_constraints([], []).
generate_canonical_ebnf_constraints([Var|Vars], [Constraint|Constraints]) :-
    generate_canonical_ebnf_constraint(Var, Constraint),
    generate_canonical_ebnf_constraints(Vars, Constraints).

generate_canonical_ebnf_constraint('$VAR'(VarName), Constraint) :-
    (ebnf_nonterminal(VarName, UC) ->
        % Direct non-terminal
        atom_concat('_', UC, DirectUC),
        Constraint =.. [DirectUC, '$VAR'(VarName)]
    ; is_ebnf_meta_variable(VarName) ->
        % Meta variable like t1, t2 - extract base non-terminal
        atom_chars(VarName, [FirstChar|_]),
        atom_chars(BaseNT, [FirstChar]),
        ebnf_nonterminal(BaseNT, UC),
        atom_concat('_', UC, DirectUC),
        Constraint =.. [DirectUC, '$VAR'(VarName)]
    ).
generate_canonical_ebnf_constraint(Var, Constraint) :-
    atom(Var),
    (ebnf_nonterminal(Var, UC) ->
        % Direct non-terminal
        atom_concat('_', UC, DirectUC),
        Constraint =.. [DirectUC, Var]
    ; is_ebnf_meta_variable(Var) ->
        % Meta variable like t1, t2 - extract base non-terminal
        atom_chars(Var, [FirstChar|_]),
        atom_chars(BaseNT, [FirstChar]),
        ebnf_nonterminal(BaseNT, UC),
        atom_concat('_', UC, DirectUC),
        Constraint =.. [DirectUC, Var]
    ).

get_nt_char(NT, C) :-
    (atom(NT) -> atom_chars(NT, [C])
    ; NT = '$VAR'(V) -> atom_chars(V, [C])).

get_nt_char_upper(NT, UC) :-
    get_nt_char(NT, C),
    char_code(C, Code),
    UpperCode is Code - 32,
    char_code(UC, UpperCode).

% Generate rules for a non-terminal
generate_nt_rules(UC, Productions) :-
    maplist(generate_production_rule(UC), Productions).

% Create direct _UC predicate rules that can be called directly
create_direct_nt_rules(UC, Productions) :-
    atom_concat('_', UC, DirectUC),
    maplist(create_direct_nt_rule(DirectUC), Productions),
    debug_msg('Created direct predicate: ~w', [DirectUC]).

create_direct_nt_rule(DirectUC, Production) :-
    (atom(Production) ->
        % Simple terminal: _T(true) :- true
        Fact =.. [DirectUC, Production],
        assert(Fact),
        debug_msg('  Added fact: ~w', [Fact])
    ; Production = '$VAR'(Var) ->
        % Variable: _T(X) :- _V(X)
        ebnf_nonterminal(Var, VarUC),
        atom_concat('_', VarUC, VarDirectUC),
        Head =.. [DirectUC, X],  % Use fresh variable
        Body =.. [VarDirectUC, X],  % Same variable
        Rule = (Head :- Body),
        store_debug_rule_form(Rule),
        assert(Rule),
        debug_msg('  Added rule: ~w', [Rule])
    ; compound(Production) ->
        % Compound: _T(if(X,Y,Z)) :- _T(X), _T(Y), _T(Z)
        % First canonicalize the production to handle mixfix operators
        canonical(Production, CanonicalProduction),
        CanonicalProduction =.. [Functor|Args],
        process_compound_args_direct(Args, NewArgs, BodyCalls, 1, DirectUC),
        HeadTerm =.. [Functor|NewArgs],
        Head =.. [DirectUC, HeadTerm],
        list_to_conjunction(BodyCalls, Body),
        Rule = (Head :- Body),
        store_debug_rule_form(Rule),
        assert(Rule),
        debug_msg('  Added compound rule: ~w', [Rule])
    ;
        debug_msg('  Skipped production: ~w', [Production])
    ).

% Process arguments for direct rule creation
process_compound_args_direct([], [], [], _, _).
process_compound_args_direct([Arg|Args], [NewArg|NewArgs], AllCalls, VarNum, DirectUC) :-
    (Arg = '$VAR'(V), atom_chars(V, [C]), code_type(C, lower) ->
        NewArg = _,  % Use fresh Prolog variable
        BodyCall =.. [DirectUC, NewArg],
        ArgCalls = [BodyCall]
    ; atom(Arg), atom_chars(Arg, [C]), code_type(C, lower) ->
        NewArg = _,  % Use fresh Prolog variable
        get_nt_char_upper(Arg, NTC),
        atom_concat('_', NTC, DirectNTC),
        BodyCall =.. [DirectNTC, NewArg],
        ArgCalls = [BodyCall]
    ;
        NewArg = Arg,
        ArgCalls = []
    ),
    VarNum1 is VarNum + 1,
    process_compound_args_direct(Args, NewArgs, RestCalls, VarNum1, DirectUC),
    append(ArgCalls, RestCalls, AllCalls).

% Generate rule for a single production
generate_production_rule(UC, Production) :-
    debug_msg('  Generating rule for production: ~w', [Production]),
    create_production_rule(UC, Production, Rule),
    debug_msg('  Created rule: ~w', [Rule]),
    (Rule = (Head :- true) ->
        % For rules with body 'true', create a fact instead
        canonical(Head, HeadC),
        varnumbers_names(HeadC, Term, _),
        debug_msg('  Canonical fact: ~w', [Term]),
        assert(Term)
    ;
        % For other rules, use normal canonical transformation
        canonical(Rule, RuleC),
        varnumbers_names(RuleC, Term, _),
        debug_msg('  Canonical rule: ~w', [Term]),
        assert(Term)
    ).

% Create a production rule based on the pattern  
create_production_rule(UC, Production, Rule) :-
    (is_list(Production) ->
        % Handle list of terms: [term(true)] -> extract the single term
        (Production = [term(Term)] ->
            create_production_rule(UC, Term, Rule)
        ; Production = [Term] ->
            create_production_rule(UC, Term, Rule)
        ; 
            % Multiple terms in production - create compound rule  
            debug_msg('  Multiple terms in production: ~w', [Production]),
            Head =.. [UC, '$VAR'(v1)],
            Rule = (Head :- true)
        )
    ; atom(Production) ->
        % Simple terminal: V true  
        Head =.. [UC, Production],
        Rule = (Head :- true)
    ; Production = '$VAR'(Var) ->
        % Variable reference: T v1 { V v1 }
        atom_chars(Var, [C]),
        char_code(C, Code),
        UpperCode is Code - 32,
        char_code(NTC, UpperCode),
        Head =.. [UC, '$VAR'(v1)],
        Body =.. [NTC, '$VAR'(v1)],
        Rule = (Head :- Body)
    ; compound(Production), Production \= '$VAR'(_) ->
        % Complex expression like ifthenelse(t,t,t) but not variables
        create_compound_rule(UC, Production, Rule)
    ;
        % Fallback for other patterns
        Head =.. [UC, '$VAR'(v1)],
        Rule = (Head :- true)
    ).

% Create rule for compound expressions like ifthenelse(t,t,t)
create_compound_rule(UC, CompoundExpr, Rule) :-
    % For ifthenelse(t,t,t), create T(ifthenelse(v1,v2,v3)) :- T(v1), T(v2), T(v3)
    CompoundExpr =.. [Functor|Args],
    % Generate variables and body calls based on arguments
    process_compound_args(Args, NewArgs, BodyCalls, 1),
    HeadExpr =.. [Functor|NewArgs],
    Head =.. [UC, HeadExpr],
    list_to_conjunction(BodyCalls, Body),
    Rule = (Head :- Body).

% Process arguments of compound expression
process_compound_args([], [], [], _).
process_compound_args([Arg|Args], [NewArg|NewArgs], AllCalls, VarNum) :-
    (Arg = '$VAR'(V), atom_chars(V, [C]), code_type(C, lower) ->
        % Non-terminal variable like $VAR(t): replace with new variable and add body call  
        atom_concat(v, VarNum, VarName),
        NewArg = '$VAR'(VarName),
        get_nt_char_upper(V, NTC),
        BodyCall =.. [NTC, '$VAR'(VarName)],
        ArgCalls = [BodyCall]
    ; atom(Arg), atom_chars(Arg, [C]), code_type(C, lower) ->
        % Non-terminal atom: replace with variable and add body call  
        atom_concat(v, VarNum, VarName),
        NewArg = '$VAR'(VarName),
        get_nt_char_upper(Arg, NTC),
        BodyCall =.. [NTC, '$VAR'(VarName)],
        ArgCalls = [BodyCall]
    ;
        % Terminal or other: keep as is
        NewArg = Arg,
        ArgCalls = []
    ),
    VarNum1 is VarNum + 1,
    process_compound_args(Args, NewArgs, RestCalls, VarNum1),
    append(ArgCalls, RestCalls, AllCalls).

% Check if production has both terminals and non-terminals  
has_terminals_and_nts(Production) :-
    member(term(_), Production),
    member(nt(_), Production).

% Create a mixfix rule for complex productions
create_mixfix_rule(UC, Production, (Head :- Body)) :-
    build_mixfix_head(UC, Production, Head),
    build_mixfix_body(Production, Body).

build_mixfix_head(UC, Production, Head) :-
    production_to_head_args(Production, 1, Args),
    Head =.. [UC|Args].

production_to_head_args([], _, []).
production_to_head_args([term(T)|Rest], N, [T|Args]) :-
    production_to_head_args(Rest, N, Args).
production_to_head_args([nt(_)|Rest], N, ['$VAR'(VarName)|Args]) :-
    atom_concat(t, N, VarName),
    N1 is N + 1,
    production_to_head_args(Rest, N1, Args).

build_mixfix_body(Production, Body) :-
    production_to_body_calls(Production, 1, Calls),
    list_to_conjunction(Calls, Body).

production_to_body_calls([], _, []).
production_to_body_calls([term(_)|Rest], N, Calls) :-
    production_to_body_calls(Rest, N, Calls).
production_to_body_calls([nt(NT)|Rest], N, [Call|Calls]) :-
    get_nt_char_upper(NT, NTC),
    atom_concat(t, N, VarName),
    Call =.. [NTC, '$VAR'(VarName)],
    N1 is N + 1,
    production_to_body_calls(Rest, N1, Calls).

list_to_conjunction([], true).
list_to_conjunction([C], C).
list_to_conjunction([C|Cs], (C, Rest)) :-
    list_to_conjunction(Cs, Rest).

% 問い合わせ
query(C) :-
    debug_msg('Querying: ~w', [C]),
    clause(C, B),
    debug_msg('Found clause: ~w :- ~w', [C, B]),
    varnumbers_names(B, T, P), !,
    debug_msg('After varnumbers_names: ~w, vars: ~w', [T, P]),
    (use(id) -> mi_id(T) ; mi(T)),
    (P \= [] -> unparseAnswers(P) ; true).

query_string(S) :-
    code_pred_canonical(S, C),
    !,
    query(C).

unparseAnswers([X = A|Ps]) :-
    str(A, F), atomics_to_string([X, =, F], ' ', UA), writeln(UA),
    unparseAnswers(Ps).
unparseAnswers([]).

str(A, A) :- atom(A); number(A).
str(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    %ops(Op, _, _, As),
    atom_concat('_', Op1, Op), 
    ops(a(Op1), _, _, As),
    str_each(Op1, As, Terms, Strs, false),
    atomic_list_concat(Strs, ' ', Str).

str_each(_, [], _, [], _).
str_each(Op, [A|As], [Term|Ts], [Str1|Rest], Paren) :-
    number(A),
    str(Term, Op, Str1, Paren),
    str_each(Op, As, Ts, Rest, true).
str_each(Op, [A|As], Ts, [A|Rest], Paren) :-
    not(number(A)),
    str_each(Op, As, Ts, Rest, Paren).

str(A, _, A, _) :- atom(A); number(A).
str(E, Op1, Str, Paren) :-
    functor(E, _, _, compound),
    E =.. [Op | _],
    atom_concat('_', Op2, Op),
    ops(a(Op2), P2, _, _),
    ops(a(Op1), P1, _, _),
    ((P1 > P2 ; Paren, Op1 = Op2) -> str(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; str(E, Str)).

code_tokens(Code, Tokens) :-
    phrase(tokens(Tokens), Code).

code_rules(Code, Rules) :-
    code_tokens(Code, Tokens),
    phrase(rules(Rules), Tokens).

code_pred(Code, Pred) :-
    code_tokens(Code, Tokens),
    phrase(pred(Pred), Tokens).

code_pred_canonical(Code, C) :-
    code_pred(Code, Pred),
    canonical(Pred, C).
    
code_mi(Code) :-
    code_rules(Code, _),
    !,
    % Show registered rules before main query if debug is enabled
    (debug_syntax -> show_registered_rules ; true),
    query(main).

% デフォルトのステップ制限
:- dynamic max_steps/1.
:- assert(max_steps(10000)).

% ステップ制限の設定
set_max_steps(N) :-
    retractall(max_steps(_)),
    assert(max_steps(N)).

% 現在の制限値を取得
get_max_steps(N) :-
    max_steps(N).

mi(Goal) :- 
    max_steps(Max),
    mi_with_limit(Goal, Max).

mi_with_limit(true, _).
mi_with_limit((A,B), N) :-
    mi_with_limit(A, N),
    mi_with_limit(B, N).
mi_with_limit(Goal, N) :-
    N > 0,
    predicate_property(Goal,built_in), !, 
    call(Goal).
mi_with_limit(Goal, N0) :-
    N0 > 0,
    Goal \= true,
    Goal \= (_,_),
    N1 is N0 - 1,
    (N1 =< 0 -> 
        (write('Execution limit reached ('), 
         max_steps(Max),
         Steps is Max - N1,
         write(Steps), writeln(' steps)'), 
         halt(2))
    ;
        (clause(Goal, Body), mi_with_limit(Body, N1))
    ).

% Add step counting version
mi_with_counter(Goal, Counter) :-
    (Goal = true -> 
        true
    ; Goal = (_,_) ->
        mi_with_counter(Goal, Counter)
    ; predicate_property(Goal, built_in) ->
        call(Goal)
    ; 
        succ(Counter, NewCounter),
        (NewCounter > 10000 ->
            format('Steps exceeded 10000 at goal: ~w~n', [Goal]),
            halt(2)
        ;
            (clause(Goal, Body), mi_with_counter(Body, NewCounter))
        )
    ).

% 元のmi関数も残す（後方互換性）
mi_original(true).
mi_original((A,B)) :-
    mi_original(A),
    mi_original(B).
mi_original(Goal) :-
    predicate_property(Goal,built_in), !, 
    call(Goal).
mi_original(Goal) :-
        Goal \= true,
        Goal \= (_,_),
        clause(Goal, Body),
        mi_original(Body).

mi_limit(Goal, Max) :-
    mi_limit(Goal, Max, _).

mi_limit(true, N, N).
mi_limit((A,B), N0, N) :-
    mi_limit(A, N0, N1),
    mi_limit(B, N1, N).
mi_limit(G, N, N) :-
    G \= true,
    G \= (_, _),
    predicate_property(G, built_in),
    call(G).
mi_limit(G, N0, N) :- 
    G \= true,
    G \= (_, _),
    predicate_property(G, interpreted),
    N0 > 0,
    N1 = N0 - 1,
    %writeln(G),
    %sleep(0.1),
    clause(G, Body),
    mi_limit(Body, N1, N), !.

mi_id(Goal) :-
    length(_, N),
    mi_limit(Goal, N).

run :-
    current_prolog_flag(argv, Argv),
    check_debug_flag(Argv),
    (get_file_path(Argv, FilePath) ->
        (catch(
            read_file(FilePath),
            Exception,
            (
                write('error: '), write(Exception), nl,
                halt(1)
            )
        ), halt(0))
    ; 
        (write('No file specified'), nl, halt(1))
    ).

% Check for --debug flag
check_debug_flag(Argv) :-
    (member('--debug', Argv) ->
        enable_syntax_debug
    ;
        true
    ).

% Get file path from argv (ignoring flags)
get_file_path(Argv, FilePath) :-
    filter_flags(Argv, Files),
    (Files = [FilePath|_] ->
        true
    ;
        fail
    ).

filter_flags([], []).
filter_flags([Arg|Rest], Files) :-
    (atom_concat('--', _, Arg) ->
        filter_flags(Rest, Files)
    ;
        Files = [Arg|RestFiles],
        filter_flags(Rest, RestFiles)
    ).

read_file(FilePath) :-
    read_file_to_string(FilePath, String, []),
    string_chars(String, Chars),
    code_mi(Chars).

% ファイル実行時のみrun実行（テスト時は実行しない）
main_execution :-
    run.

% コンパイル時の自動実行（consultやloading時は実行しない）
:- (current_prolog_flag(argv, [_|_]) -> main_execution ; true).

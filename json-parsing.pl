%  Progetto Prolog e Common Lisp 2018-01-15 (E1P)
%
%  -*- Mode : Prolog -*-
%  -- json-parsing.pl --


%  --SEZIONE PARSING--
%  json_parse/2

json_parse(JSONString, Object) :-
    atom_chars(JSONString, Chars),
    phrase(parse_object(Object), Chars), !.
json_parse(JSONString, Object) :-
    atom_chars(JSONString, Chars),
    phrase(parse_array(Object), Chars), !.
json_parse(JSONString, Object) :-
    atom_chars(JSONString, Chars),
    phrase(parse_string(Object), Chars), !.
json_parse(JSONString, Object) :-
    atom_chars(JSONString, Chars),
    phrase(parse_number(Object), Chars), !.

parse_object(json_obj([])) -->
    ws,
    ['{}'],
    ws, !.
parse_object(Term) -->
    ws,
    ['{'],
    parse_object_aux(Term).

parse_object_aux(json_obj(Members)) -->
    parse_members(Members),
    ['}'],
    ws,
    !.
parse_object_aux(json_obj([])) -->
    ws,
    ['}'],
    ws,
    !.
parse_object_aux() -->
    ws,
    [']'],
    ws,
    {fail}.

parse_members([Pair | Members]) -->
    parse_pair(Pair),
    [','],
    !,
    parse_members(Members).
parse_members([Pair]) -->
    parse_pair(Pair).

parse_pair((Key, Value)) -->
    ws,
    parse_key(Key),
    ws,
    [':'],
    ws,
    parse_value(Value),
    ws.

parse_key(Key) -->
    parse_string(String),
    {
	atom(String),
	atom_string(String, Key);
	term_string(String, Key) },
    !.

parse_value(Value) -->
    parse_string(String),
    {
	atom(String),
	atom_string(String, Value);
	term_string(String, Value) },
    !.
parse_value(Value) -->
    parse_number(Value),
    !.
parse_value(Value) -->
    parse_symbol(Value),
    !.
parse_value(Value) -->
    parse_object(Value),
    !.
parse_value(Value) -->
    parse_array(Value),
    !.

parse_string(Value) -->
    ['"'],
    parse_string_aux(Value).

parse_string_aux(Value) -->
    parse_chars(Value),
    ['"'],
    !.

parse_array(json_array([])) -->
    ws,
    ['[]'],
    ws, !.
parse_array(json_array(Array)) -->
    ['['],
    ws,
    parse_array_aux(Array).

parse_array_aux(Array) -->
    parse_values(Array),
    ws,
    [']'], !.
parse_array_aux([]) -->
    ws,
    [']'],
    !.
parse_array_aux() -->
    ws,
    ['}'],
    ws,
    {fail}.

parse_values([Value|Values]) -->
    parse_value(Value),
    ws,
    [','],
    !,
    ws,
    parse_values(Values).
parse_values([Value]) -->
    parse_value(Value).

parse_symbol(+true)  --> [t,r,u,e], !.
parse_symbol(+false) --> [f,a,l,s,e], !.
parse_symbol(+null)  --> [n,u,l,l], !.


parse_number(Number) -->
    parse_float(Number),
    !.
parse_number(Number) -->
    parse_integer(Number).

parse_float(Float) -->
    parse_optional_minus(Chars, Chars1),
    parse_digits_for_integer(Chars1, ['.' | Chars0]),
    ['.'],
    parse_digits_for_integer(Chars0, []),
    { number_chars(Float, Chars) }.


parse_digits_for_integer([Digit | Digits], Digits0) -->
    parse_digit_nonzero(Digit),
    !,
    parse_optional_digits(Digits, Digits0).
parse_digits_for_integer([Digit | T], T) -->
    parse_digit(Digit).

parse_digit_nonzero(Digit) -->
    parse_digit(Digit),
    { Digit \== '0' }.

parse_optional_digits([Digit | Digits], T) -->
    parse_digit(Digit),
    !,
    parse_optional_digits(Digits, T).
parse_optional_digits(T, T) --> [].

parse_digits([Digit | Digits], T) -->
    parse_digit(Digit),
    parse_optional_digits(Digits, T).

parse_digit(Digit) -->
    [Digit],
    { char_type(Digit, digit) }.

parse_integer(Integer) -->
    parse_optional_minus(Chars, Chars1),
    parse_digits_for_integer(Chars1, []),
    { number_chars(Integer, Chars) }.

parse_optional_minus(['-' | T], T) -->
    ['-'], !.
parse_optional_minus(T, T) -->
    [], !.

parse_chars(Atom) -->
    parse_chars_aux(Chars),
    { atom_chars(Atom, Chars) }.

parse_chars_aux([Char | Chars]) -->
    ['\\'],
    !,
    parse_escape_sequence(Char),
    parse_chars_aux(Chars).
parse_chars_aux([Char | Chars]) -->
    parse_char(Char),
    !,
    parse_chars_aux(Chars).
parse_chars_aux([]) --> [].

parse_escape_sequence(RealChar) -->
    [Char],
    { valid_escape_char(Char, RealChar) },
    !.
parse_escape_sequence(Char) -->
    parse_hex_sequence(Char).

parse_hex_sequence(Char) -->
    ['u',Hex1,Hex2,Hex3,Hex4],
    { atomic_list_concat(['0x', Hex1, Hex2, Hex3, Hex4], HexAtom) },
    { atom_number(HexAtom, Code) },
    { atom_codes(Char, [Code]) }.

parse_char(Char) -->
    [Char],
    { valid_char(Char) }.

ws -->
    ws_char,
    !,
    ws.
ws --> [].

ws_char -->
    [Char],
    { char_type(Char, space) }.

valid_escape_char('"',  '"').
valid_escape_char('\\', '\\').
valid_escape_char('/',  '/').
valid_escape_char('b',  '\b').
valid_escape_char('f',  '\f').
valid_escape_char('n',  '\n').
valid_escape_char('r',  '\r').
valid_escape_char('t',  '\t').

valid_char(Char) :-
    Char \== '"'.


%  --SEZIONE SEARCH--
%  Funzione json_get/3


json_get(JSON, Path, Result) :-
     findall(Res, json_get_aux(JSON, [Path], Res), [Result | _]), !.
json_get(JSON, Path, Result) :-
    findall(Res, json_get_aux(JSON, Path, Res), [Result | _]), !.

json_get_aux(It, [], It).

json_get_aux(json_obj(Fields), [Name | Rest], It) :-
    member((Name, Sub), Fields),
    json_get_aux(Sub, Rest, It).

json_get_aux(json_array(Array), [Point | Rest], It) :-
    nth(Point, Array, Sub),
    json_get_aux(Sub, Rest, It).
json_get_aux(json_array(Array), [Point | Rest], It) :-
    nth(Point, Array, Sub),
    json_get_aux(Sub, Rest, It).

nth(Point, Array, It) :-
    number(Point),
    length(Before, Point),
    append(Before, [It | _], Array).
nth([Point], Array, It) :-
    number(Point),
    length(Before, Point),
    append(Before, [It | _], Array).


%  --SEZIONE I/O--


% json_load/2

json_load(Filename, JSON) :-
    exists_file(Filename),
    open(Filename, read, InStream),
    read_string(InStream, _, JsonString),
    close(InStream),
    json_parse(JsonString, JSON).
    

%  json_write/3


json_write(JSON, FileName) :-
    exists_file(FileName),
    delete_file(FileName),
    open(FileName, write, Out),
    write_json_parse(JSON, String), !,
    write(Out, String),
    close(Out).

json_write(JSON, FileName) :-
    open(FileName, write, Out),
    write_json_parse(JSON, String), !,
    write(Out, String),
    close(Out).

write_json_parse(Term, JSON) :-
    phrase(write_object(Term), JsonString),
    atom_chars(JSON, JsonString), !.
write_json_parse(Term, JSON) :-
    phrase(write_array(Term), JsonString),
    atom_chars(JSON, JsonString), !.

write_object(json_obj([])) -->
    !,
    ['{'],
    ['}'].

write_object(json_obj(Members)) -->
    ['{'],
    write_members(Members),
    ['}'].


write_members([Pair]) -->
     !,
     write_pair(Pair).
write_members([Pair | Pairs]) -->
     write_pair(Pair),
     [','],
     write_members(Pairs).

write_pair((Key, Value)) -->
    write_key(Key),
    [':'],
    write_value(Value).

write_key(Key) -->
    %% { term_to_atom(Key, Atom) },
    write_atom(Key).

write_value(json_array(Value)) -->
    { looks_like_list(Value)},
    !,
    write_array(json_array(Value)).
write_value(json_obj(Value)) -->
    !,
    write_object(json_obj(Value)).
write_value(+Value) -->
    !,
    write_symbol(Value).
write_value(Value) -->
    { number(Value) },
    !,
    write_number(Value).
write_value(Value) -->
    %% { term_to_atom(Value, Atom) },
    %% !,
    write_atom(Value).


write_atom(Atom) -->
    ['"'],
    { atom_chars(Atom, Chars) },
    write_string_chars(Chars),
    ['"'].

write_string_chars([]) --> !, [].
write_string_chars(Chars) -->
    write_special_chars(Chars, Chars1),
    !,
    write_string_chars(Chars1).
write_string_chars([Char | Chars]) -->
    [Char],
    write_string_chars(Chars).

write_special_chars(['\\' | Chars], Chars1) -->
    ['\\'],
    !,
    write_escape_sequence(Chars, Chars1).
write_special_chars([Char | Chars], Chars) -->
    { single_special_char(Char, EscapedChar) },
    ['\\', EscapedChar].

single_special_char('"',  '"').
single_special_char('/',  '/').
single_special_char('\b', 'b').
single_special_char('\f', 'f').
single_special_char('\n', 'n').
single_special_char('\r', 'r').

write_escape_sequence(['u',Hex1,Hex2,Hex3,Hex4|Chars], Chars) -->
    ['u',Hex1,Hex2,Hex3,Hex4],
    !.
write_escape_sequence(Chars, Chars) -->
    ['\\'].

write_number(Number) -->
    { number_chars(Number, Chars) },
    Chars.

write_array(json_array([])) -->
    !,
    ['['],
    [']'].
write_array(json_array(Values)) -->
    ['['],
    write_array_values(Values),
    [']'].

write_array_values([Value | Values]) -->
    write_value(Value),
    [','],
    write_array_values(Values).
write_array_values([Value]) -->
    !,
    write_value(Value).


write_symbol(true)  --> !, [t,r,u,e].
write_symbol(false) --> !, [f,a,l,s,e].
write_symbol(null)  --> !, [n,u,l,l].


looks_like_list([]).
looks_like_list([_ | _]).

%  --end_of_file--
%  --json_parsing.pl--









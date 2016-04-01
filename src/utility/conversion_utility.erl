-module(conversion_utility).

-export([to_binary/1, to_list/1, list_to_hex/1, to_integer/1, binary_to_lower/1, urlencode/1, term_to_string/1, string_to_term/1]).

%% Convert everything to binary(just enough version right now)
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float);
to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(_) ->
    <<"UNSUPPORTED BINARY">>.

urlencode(Int) when is_integer(Int) ->
  integer_to_list(Int);
urlencode(Float) when is_float(Float) ->
  float_to_list(Float);
urlencode(L) when is_list(L) ->
  binary_to_list(cow_qs:urlencode(conversion_utility:to_binary(L)));
urlencode(B) when is_binary(B) ->
  binary_to_list(cow_qs:urlencode(B)).

to_list(Key) when is_atom(Key) ->
    atom_to_list(Key);
to_list(Key) when is_integer(Key) ->
    integer_to_list(Key);
to_list(Key) when is_binary(Key) ->
    binary_to_list(Key);
to_list(Key) when is_list(Key) ->
    Key.

%% 转换成16进制
list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

%% 整形转换成16进制,返回值举例：[49,69]--> "1E"
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

%% 16进制
hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $A + (N-10).

%% 转换为整形
to_integer(Value) when is_binary(Value) ->
   binary_to_integer(Value);
to_integer(Value) when is_list(Value) ->
   list_to_integer(Value);
to_integer(Value) when is_integer(Value) ->
   Value.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
   binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
  case erl_scan:string(String++".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, Term} -> Term;
        _Err -> undefined
      end;
    _Error ->
      undefined
  end.


%% 例如：<<"ABcD">> -> <<"abcd">>
binary_to_lower(Key) ->
  KeyStr = conversion_utility:to_list(Key),
  LowerStr = string:to_lower(KeyStr),
  conversion_utility:to_binary(LowerStr).


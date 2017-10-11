There are 3 shift/reduce conflicts
```
State 1 conflicts: 1 shift/reduce
State 79 conflicts: 1 shift/reduce
State 81 conflicts: 1 shift/reduce
```

```
State 79

   51 type_dec: namety_list .
   54 namety_list: namety_list . namety

    TYPE  shift, and go to state 73

    TYPE      [reduce using rule 51 (type_dec)]
    $default  reduce using rule 51 (type_dec)

    namety  go to state 111

...

State 81

   48 function_dec: fundec_list .
   70 fundec_list: fundec_list . fundec

    FUNCTION  shift, and go to state 71

    FUNCTION  [reduce using rule 48 (function_dec)]
    $default  reduce using rule 48 (function_dec)

    fundec  go to state 112
```

The default behavior is to shift.

```
State 1

   21 call_exp: ID . LPAREN arg_list RPAREN
   33 record_exp: ID . LBRACE efield_list RBRACE
   44 array_exp: ID . LBRACK exp RBRACK OF exp
   82 lvalue: ID .
   85       | ID . LBRACK exp RBRACK

    LPAREN  shift, and go to state 30
    LBRACK  shift, and go to state 31
    LBRACE  shift, and go to state 32

    LBRACK    [reduce using rule 82 (lvalue)]
    $default  reduce using rule 82 (lvalue)
```

This conflict of `lvalue -> id` and `lvalue-> lvalue[exp]`,
the default behavior is not what I want,
so I add a new rule `lvalue -> id[exp]`.
The conflict still exists, but default behavior is new what I want.

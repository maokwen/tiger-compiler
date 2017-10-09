```
State 1 conflicts: 1 shift/reduce

...

State 1

   17 exp_assign: ID . LBRACE RBRACE
   18           | ID . LBRACE assign_list RBRACE
   19           | ID . LBRACK exp RBRACK OF exp
   22 exp_call: ID . LPAREN RPAREN
   23         | ID . LPAREN exp_list RPAREN
   63 lvalue: ID .

    LPAREN  shift, and go to state 21
    LBRACK  shift, and go to state 22
    LBRACE  shift, and go to state 23

    LBRACK    [reduce using rule 63 (lvalue)]
    $default  reduce using rule 63 (lvalue)

```

The default behavior is to shift.

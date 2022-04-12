# michelson_typecheck_test

```
% dune exec ./main.exe
{ parameter address ;
  storage unit ;
  code { CAR
         /* [ @parameter address ] */ ;
         CONTRACT unit
         /* [ @parameter.contract option (contract unit) ] */ ;
         IF_NONE
           { FAIL }
           { PUSH mutez 1
             /* [ mutez : @parameter.contract.some contract unit ] */ ;
             UNIT
             /* [ unit : mutez : @parameter.contract.some contract unit ] */ ;
             TRANSFER_TOKENS
             /* [ operation ] */ ;
             NIL operation
             /* [ list operation : operation ] */ ;
             SWAP
             /* [ operation : list operation ] */ ;
             CONS
             /* [ list operation ] */ ;
             UNIT
             /* [ unit : list operation ] */ ;
             SWAP
             /* [ list operation : unit ] */ ;
             PAIR
             /* [ pair (list operation) unit ] */ } } }
```

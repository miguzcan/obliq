;=========================================================================
;; BNF grammar fo the language Obliq
;; 
;;
;;<programa> ::= <expresion>
;;<expresion> ::= <identificador>
;; ::= <numero>
;; ::= <caracter>
;; ::= <cadena>
;; ::= true
;; ::= false
;; ::= ok
;; ::= var {<identificador> = <expresioni>}*(,) in <expresion> end
;; ::= let {<identificador> = <expresioni>}*(,) in <expresion> end
;; ::= let rec {<identificador> ({<identificadori>}*(,)) = <expresion>}* in <expresion> end
;; ::= set <identificador> := <expresion>
;; ::= begin <expresion> {<expresion>}*(;) end
;; ::= <primitiva> ({<expresion>}*(,))
;; ::= < bool-oper > ({<expresion>}*(,))
;; ::= < bool-primitiva > ({<expresion>}*(,))
;; ::= if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion> }* else <expresion> end
;; ::= proc ({<identificador>}*(,)) <expresion> end
;; ::= apply <identificador> ({<expresion>}*(,))
;; ::= meth (<identificador>, {<identificador>}*(,)) <expresion> end
;; ::= for <identificador> = <expresion> to <expresion> do <expresion> end
;; ::= object {<identificador> => <expresion> }* end
;; ::= get <identificador>.<identificador>
;; ::= send <identificador>.<identificador>({<identificador>}*(,))
;; ::= update <identificador>.<identificador> := <expresion>
;; ::= clone (<identificador>)
;; ::= <bool-primitiva>({<expresion>}*(,))
;; ::= <bool-oper>({<expresion>}*(,))
;;
;;<bool-expresion> ::= <bool-primitiva>({<expresion>}*(,))
;; ::= <bool-oper>({<expresion>}*(,))
;; ::= true
;; ::= false
;;
;;<primitiva> ::= + |- |* |/ | % | & |
;;<bool-primitiva> ::= < | > | <= | >= |is
;;<bool-oper> ::= not |and |or
;;
;;
;;
;=========================================================================

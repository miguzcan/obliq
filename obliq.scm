;;Fundamentos de Lenguajes de programacion            
;;            Proyecto del curso
;;            
;;
;;                 OBLIQ
;;
;;Michell Guzman Cancimance   --- 0910016



;=========================================================================
;; Gramatica del BNF para el lenguaje obliq
;;
;;
;;<programa>                       ::= <expresion>
;;<expresion>                      ::= <identificador>
;;                                 ::= <numero>
;;                                 ::= <caracter>
;;                                 ::= <cadena>
;;                                 ::= true
;;                                 ::= false
;;                                 ::= ok  
;;                                 ::= var {<identificador> = <expresioni>}*(,) in <expresion> end
;;                                 ::= let {<identificador> = <expresioni>}*(,) in <expresion> end
;;                                 ::= let rec {<identificador> ({<identificadori>}*(,)) = <expresion>}* in <expresion> end
;;                                 ::= set <identificador> := <expresion>
;;                                 ::= begin <expresion> {<expresion>}*(;) end
;;                                 ::= <primitiva> ({<expresion>}*(,))
;;                                 ::= < bool-oper > ({<expresion>}*(,))
;;                                 ::= < bool-primitiva > ({<expresion>}*(,))
;;                                 ::= if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion> }* else <expresion> end
;;                                 ::= proc ({<identificador>}*(,)) <expresion> end
;;                                 ::= apply <identificador> ({<expresion>}*(,))
;;                                 ::= meth (<identificador>, {<identificador>}*(,)) <expresion> end
;;                                 ::= for <identificador> = <expresion> to <expresion> do <expresion> end
;;                                 ::= object {<identificador> => <expresion> }* end
;;                                 ::= get <identificador>.<identificador>
;;                                 ::= send <identificador>.<identificador>({<identificador>}*(,))
;;                                 ::= update <identificador>.<identificador> := <expresion>
;;                                 ::= clone (<identificador>)
;;                                 ::= <bool-primitiva>({<expresion>}*(,))
;;                                 ::= <bool-oper>({<expresion>}*(,))
;;
;;<bool-expresion>                 ::= <bool-primitiva>({<expresion>}*(,))
;;                                           ::= <bool-oper>({<expresion>}*(,))
;;                                           ::= true
;;                                           ::= false
;;                     
;;<primitiva>                      ::= + |- |* |/ | % | & |
;;<bool-primitiva>                 ::= < | > | <= | >= |is 
;;<bool-oper>                      ::= not |and |or 
;;
;;
;;
;=========================================================================

; Las especificaciones lexica y sintactica del interpretador simple son:
; Especificacion lexica
(define scanner-spec-simple-interpreter
  '((whitespace (whitespace) skip)                       ; Espacios en blanco
    (comment ("(*" (arbno (not #\newline)) "*)") skip)   ; Comentarios
    (identifier                                          ; Identificador
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)                ; Numero
    (string 
     ("\"" (arbno (or letter digit whitespace "_" "-" "?" "." ",")) "\"") string) ; Cadenas (String)
    (char
     ("'" (or letter digit) "'") symbol)                 ; Caracter  
    ))


; Especificacion de la gramatica del lenguaje
(define grammar-simple-interpreter
  '(
    ;; Especificacion de la gramatica del programa  a-program
    (program (expression) a-program)
    ;; Especificacion de la gramatica de la expresion var-exp
    (expression (identifier) var-exp)
    ;; Especificacion de la gramatica de la expresion lit-exp (para el caso en que la expresion sea un numero)
    (expression (number) lit-exp)  
    ;; Especificacion de la gramatica de la expresion char-exp
    (expression (char) char-exp)    
    ;; Especificacion de la gramatica de la expresion string-exp
    (expression (string) string-exp)    
    ;; Especificacion de la gramatica de la expresion true-exp
    (expression ("true") true-exp)
    ;; Especificacion de la gramatica de la expresion false-exp
    (expression ("false") false-exp)
    ;; Especificacion de la gramatica de la expresion ok-exp
    (expression ("ok") ok-exp)  
    ;; Especificacion de la gramatica de la expresion vari-exp
    (expression ("var" (arbno identifier "=" expression) "in" expression "end") vari-exp)
    ;; Especificacion de la gramatica de la expresion let-exp
    (expression ("let" (arbno identifier "=" expression) "in" expression "end") let-exp)
    ;; Especificacion de la gramatica de la expresion letrec-exp
    (expression                         
      ("let rec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression "end")
      letrec-exp)
    ;; Especificacion de la gramatica de la expresion set-exp
    (expression ("set" identifier ":=" expression) set-exp)
    ;; Especificacion de la gramatica de la expresion begin-exp
    (expression
      ("begin" expression ";" expression "end")
      begin-exp)
    ;; Especificacion de la gramatica de la expresion primapp-exp
    (expression 
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp) 
    ;; Especificacion de la gramatica de la expresion operapp-exp
    (expression
      (bool-oper "(" (separated-list expression ",") ")" )
      operapp-exp)   
    ;; Especificacion de la gramatica de la expresion expboolapp-exp
    (expression
      (bool-prim "(" (separated-list expression ",") ")" )
      expboolapp-exp)
    ;; Especificacion de la gramatica de la expresion if-exp
    (expression
     ("if" bool-exp "then" expression (arbno "elseif" bool-exp "then" expression) "else" expression "end")
     if-exp)    
    ;; Especificacion de la gramatica de la expresion proc-exp
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression "end")
      proc-exp)
    ;; Especificacion de la gramatica de la expresion apply-exp
    (expression
      ("apply" expression "(" (arbno expression) ")")
      apply-exp)
    ;; Especificacion de la gramatica de la expresion method-exp    
    (expression     
     ("meth" "(" identifier "," (separated-list identifier ",") ")" expression "end")
     method-exp)
    ;; Especificacion de la gramatica de la expresion for-exp
    (expression
     ("for" identifier "=" expression "to" expression "do" expression "end")
     for-exp)
    ;; Especificacion de la gramatica de la expresion object-exp    
    (expression
     ("object" "{" (arbno identifier "=>" expression) "}" "end")
     object-exp)
    ;; Especificacion de la gramatica de la expresion get-exp    
    (expression
      ("get" identifier "." identifier)        
      get-exp)
    ;; Especificacion de la gramatica de la expresion send-exp
    (expression
      ("send" identifier "." identifier
        "("  (separated-list expression ",") ")")
      send-exp)
    ;; Especificacion de la gramatica de la expresion update-exp
    (expression
      ("update" identifier "." identifier ":=" expression)        
      update-exp)
    ;; Especificacion de la gramatica de la expresion clone-exp
    (expression     
     ("clone" "("identifier")") 
     clone-exp)
    ;; Especificacion de la gramatica de la expresion boolapp-exp    
    (bool-exp
      (bool-prim "(" (separated-list expression ",") ")" )
      boolapp-exp)
    ;; Especificacion de la gramatica de la expresion booloperapp-exp
    (bool-exp
      (bool-oper "(" (separated-list expression ",") ")" )
      booloperapp-exp)   
    ;; Especificacion de la gramatica de la expresion bool-true-exp
    (bool-exp ("true") bool-true-exp)
    ;; Especificacion de la gramatica de la expresion bool-false-exp
    (bool-exp ("false") bool-false-exp)
    ;; Especificacion de la gramatica de la expresion add-prim
    (primitive ("+")     add-prim)
    ;; Especificacion de la gramatica de la expresion substract-prim
    (primitive ("-")     subtract-prim)
    ;; Especificacion de la gramatica de la expresion mult-prim
    (primitive ("*")     mult-prim)
    ;; Especificacion de la gramatica de la expresion div-prim
    (primitive ("/")     div-prim)
    ;; Especificacion de la gramatica de la expresion mod-prim
    (primitive ("%")     mod-prim)
    ;; Especificacion de la gramatica de la expresion concat-prim
    (primitive ("&")     concat-prim)  
    ;; Especificacion de la gramatica de la expresion equal?-prim       
    (bool-prim ("=") equal?-prim)
    ;; Especificacion de la gramatica de la expresion zero?-prim 
    (bool-prim ("zero?") zero?-prim)
    ;; Especificacion de la gramatica de la expresion mayor?-prim 
    (bool-prim (">") mayor?-prim)
    ;; Especificacion de la gramatica de la expresion menor?-prim 
    (bool-prim ("<") menor?-prim)
    ;; Especificacion de la gramatica de la expresion mayorigual?-prim 
    (bool-prim (">=") mayorigual?-prim)
    ;; Especificacion de la gramatica de la expresion menorigual?-prim 
    (bool-prim ("<=") menorigual?-prim)
    ;; Especificacion de la gramatica de la expresion is?-prim 
    (bool-prim ("is") is?-prim)
    ;; Especificacion de la gramatica de la expresion bool-oper-not 
    (bool-oper ("not") bool-oper-not)
    ;; Especificacion de la gramatica de la expresion bool-oper-and 
    (bool-oper ("and") bool-oper-and)
    ;; Especificacion de la gramatica de la expresion bool-oper-or 
    (bool-oper ("or") bool-oper-or)

  ))


;*********************** Llamado al Sllgen ********************

;; Se construyen los tipos de dato automaticamente por medio del sllgen
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;; Procedimiento para mostrar los datatypes generados
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;; El FrontEnd (Analisis lexico (scanner) y sintactico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;; El Analizador Lexico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;; Mensaje en la consola
(display "Proyecto FLP \n\n")

;; Se ejecuta el interpretador
(define interpretador
  (sllgen:make-rep-loop  "obliq-> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))



;*********************** Tipos de Datos ********************

;; se crea el tipo de dato objeto
(define-datatype object object?
  (an-object
   (ids (list-of symbol?))
   (exps vector?)))

;; se crea el tipo de dato metodo
(define-datatype method method?
  (a-method
   (id-obj symbol?)
   (ids (list-of symbol?))
   (body expression?)))

;; se crea el tipo de dato bool-expression
(define-datatype bool-exp bool-exp?
  (boolapp-exp
    (prim bool-prim?)
    (rands (list-of expression?)))
  (booloperapp-exp
    (prim bool-oper?)
    (rands (list-of expression?)))
  (bool-true-exp)
  (bool-false-exp)
  )


;; se crea el tipo de dato bool-primitive
(define-datatype bool-prim bool-prim?
  (equal?-prim)
  (zero?-prim)
  (mayor?-prim)
  (menor?-prim)
  (mayorigual?-prim)
  (menorigual?-prim)
  (is?-prim)
  )

;; se crea el tipo de dato bool-primitive
(define-datatype bool-oper bool-oper?
  (bool-oper-and)
  (bool-oper-or)
  (bool-oper-not)  
  )

;; Se define el tipo de dato procval
(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

;; Se crea el tipo de dato environment
(define-datatype environment environment?
  (empty-env-record)           
  (extended-env-record         
    (syms (list-of symbol?))   
    (vec vector?)              
    (env environment?))        
(extended-mutable-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?))
  )


;; Tipo de dato referencia
(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

;;Implementacion retorna el valor almacenado en la referencia
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos)))))

;;Implementacion cambia el valor almacenado por la referencia
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val)))))

;;Interfaz retorna el valor almacenado en la referencia
(define deref 
  (lambda (ref)
    (primitive-deref ref))) 

;;Interfaz cambia el valor almacenado por la referencia
(define setref! 
  (lambda (ref val)
    (primitive-setref! ref val)))

;*********************** El Interprete ********************

;; eval-program: <programa> -> numero
;; funcion que evalua un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)                        
        (eval-expression body (init-env))        
        )))) 


;; eval-expression: <expression> <enviroment> -> numero
;; evalua la expresion en el ambiente de entrada
(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      ;; Evaluacion de la expresion var-exp
      (var-exp (id) (apply-env-aux-let env id))
      ;; Evaluacion de la expresion lit-exp
      (lit-exp (datum) datum)     
      ;; Evaluacion de la expresion char-exp
      (char-exp (char) char) 
      ;; Evaluacion de la expresion string-exp
      (string-exp (string) string)
      ;; Evaluacion de la expresion true-exp
      (true-exp () 'true)
      ;; Evaluacion de la expresion false-exp
      (false-exp () 'false) 
      ;; Evaluacion de la expresion ok-exp
      (ok-exp () (let ((x 4)) (set! x 1)))
      ;; Evaluacion de la expresion vari-exp
      (vari-exp (ids rands body)
                (let ((args (eval-rands rands env)))
                  (eval-expression body (extend-mutable-env ids args env))))
      ;; Evaluacion de la expresion let-exp
      (let-exp (ids rands body)
                (let ((args (eval-rands rands env)))
                  (eval-expression body (extend-env ids args env))))
      ;; Evaluacion de la expresion letrec-exp           
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      ;; Evaluacion de la expresion set-exp  
      (set-exp (id rhs-exp)  
        (begin
          (setref! 
            (apply-env env id)
            (eval-expression rhs-exp env))
          1))           
      ;; Evaluacion de la expresion begin-exp 
      (begin-exp (exp1 exp2)
         (let ((acc (eval-expression exp1 env))
               (exp2 exp2))
           (if (null? exp2) acc
               (eval-expression exp2 env))))       
      ;; Evaluacion de la expresion primapp-exp 
      (primapp-exp (prim rands)              
        (let ((args (eval-rands rands env))) 
          (apply-primitive prim args)))      
      ;; Evaluacion de la expresion operapp-exp 
      (operapp-exp (prim rands)
        (let ((args (eval-rands rands env))) 
          (apply-bool-oper prim args)))
      ;; Evaluacion de la expresion expboolapp-exp 
      (expboolapp-exp (prim rands)
         (let ((args (eval-rands rands env))) 
          (apply-bool-prim prim args)))              
      ;; Evaluacion de la expresion if-exp 
      (if-exp (prueb-exp resp-exp test-exps conseq-exps else-exp)
       (if (eqv? 'true (eval-bool-exp prueb-exp env))
          (eval-expression resp-exp env)
          (eval-elseif test-exps conseq-exps else-exp env)))       
      ;; Evaluacion de la expresion proc-exp 
      (proc-exp (ids body) (closure ids body env))           
      ;; Evaluacion de la expresion apply-exp 
      (apply-exp (rator rands) 
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env))) 
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      ;; Evaluacion de la expresion method-exp 
      (method-exp (object-name ids body)                  
                  (a-method object-name ids bd))   
      ;; Evaluacion de la expresion for-exp 
       (for-exp (id expi expf body)
                (let ((expini (eval-expression expi env))
                      (expfin (+ 1(eval-expression expf env))))
             (eval-for id (- expfin expini) body (extend-mutable-env (list id) (list expini) env))))    
                  
                            
;*********************** Objetos ********************
      
      ;; Evaluacion de la expresion object-exp 
      (object-exp (ids exps)
                  (let ((args (eval-rands exps env)))
                    (an-object ids (lista-a-vector args))))   
      ;; Evaluacion de la expresion get-exp 
      (get-exp (object-name field-name)
               (let ((obj (apply-env-aux-let env object-name)))
                     (aux-get obj object-name field-name)))      
      ;; Evaluacion de la expresion update-exp  
      (update-exp (object-name field-name exp)
                  (let ((obj (apply-env-aux-let env object-name))
                        (new-exp (eval-expression exp env)))
                     (aux-update obj object-name field-name new-exp)))
       ;; Evaluacion de la expresion clone-exp 
       (clone-exp (object-name)
                 (let ((obj (apply-env-aux-let env object-name)))
                   (aux-clone obj object-name)))
      ;; Evaluacion de la expresion send-exp 
      (send-exp (object-name method-name exps)
                (let ((obj (deref (apply-env-aux-let env object-name))) 
                      (args (eval-rands exps env))) 
                  (if (object? obj)
                      (cases object obj
                       (an-object (ids exps)
                                 (let ((pos (rib-find-position method-name ids))) 
                                       (if (number? pos)
                                           (let ((met (vector-ref exps pos))) 
                                               (if (method? met) 
                                                   (cases method met
                                                    (a-method (object-name ids body)
                                                               (eval-expression body (extend-env ids args env)))) 
                                                   (eopl:error 'send-exp "~s no es un metodo" method-name))) 
               (eopl:error 'send-exp "El metodo ~s no se encuentra definido en este object ~s" method-name object-name)))))
                               (eopl:error 'send-exp "~s no es un objeto" object-name))))    
       
      )))  






;*********************** Aplicacion de Primitivas ********************

;; apply-bool-prim: <bool-prim> <list-of-expression> -> bool
(define apply-bool-prim
  (lambda (prim args)
    (cases bool-prim prim
      (equal?-prim () (if (= (car args) (cadr args))
                        (true-value)
                        (false-value)))
      (zero?-prim () (if (= (car args) 0) (true-value) (false-value)))
      (mayor?-prim () (if (> (car args) (cadr args))
                          (true-value)
                          (false-value)))
      (menor?-prim () (if (< (car args) (cadr args))
                       (true-value)
                       (false-value)))
      (mayorigual?-prim () (if (>= (car args) (cadr args)) (true-value) (false-value)))
      (menorigual?-prim () (if (<= (car args) (cadr args)) (true-value) (false-value)))
      (is?-prim () (if (eqv? (car args) (cadr args)) (true-value) (false-value)))
      )))


;; apply-bool-oper: <bool-oper> <list-of-expression> -> bool
(define apply-bool-oper
  (lambda (prim args)
    (cases bool-oper prim
      (bool-oper-and () (if (and (eqv? 'true (car args))
                                (eqv? 'true (cadr args)))
                           (true-value)
                           (false-value)))
      (bool-oper-or () (if (and (not (eqv? 'true (car args)))
                                (not (eqv? 'true (cadr args))))
                           (false-value)
                           (true-value)))
      (bool-oper-not () (if (eqv? 'true (car args))
                          (false-value)
                          (true-value)))
      )))

   

;; apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args)  (cadr args))) 
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (div-prim  () (/ (car args) (cadr args)))
      (mod-prim  () (modulo (car args) (cadr args)))
      (concat-prim () (string-append (car args) (cadr args))))))

;; Toma la clausura e invoca la clausura en el ambiente extendido
;; apply-procval: <proc> <list-of-expression> -> numero
(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-mutable-env ids args env))))))

   
;*********************** Funciones Auxiliares ********************

;; eval-bool-exp: expression env -> bool
;; Funcion auxiliar utilizada para evaluar una expresion booleana
(define eval-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
      (boolapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-bool-prim prim args)))
      (booloperapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-bool-oper prim args)))   
      (bool-true-exp () 'true)
      (bool-false-exp () 'false)
      )))

;; eval-elseif: list-of-exp list-of-exp exp env -> valor expresado
;; Funcion auxiliar utilizada para evaluar una expresion if
(define eval-elseif
        (lambda (test-exps conseq-exps else-exp env)
          (if (null? test-exps)
              (eval-expression else-exp env)
              (if (eqv? 'true (eval-bool-exp (car test-exps) env))
                  (eval-expression (car conseq-exps) env)
                  (eval-elseif (cdr test-exps) (cdr conseq-exps) else-exp)))))

;; evaluar-for: symbol exp1 exp2 env -> list
;; Funcion auxiliar utilizada para evaluar una expresion for
(define eval-for
    (lambda (id exp body env)
       (if (not (zero? exp))
           (let ((val (deref(apply-env-let env id))))                 
           (cons (eval-expression body env) 
                 (eval-for id (- exp 1) body (extend-env (list id) (list(+ 1 val)) env))))
           '())))

;;aux-get: <object> <symbol> <symbol> -> number
;;Funcion auxiliar utilizada para saber el valor de un campo de un objeto
(define aux-get 
        (lambda(obj object-name field-name)
          (if (object? obj)
              (cases object obj
                (an-object (ids exps)
                     (let ((pos (rib-find-position field-name ids)))
                       (if (number? pos)
                          (vector-ref exps pos)
                          (eopl:error 'aux-get "El campo ~s no se encontro en el objeto ~s" field-name object-name)))))
              (eopl:error 'aux-get "El parametro ~s no es un objeto" object-name ))))

;;aux-update: <object> <symbol> <symbol> exp -> vector
;;Funcion auxiliar utilizada para actualizar el valor de un campo
(define aux-update 
        (lambda(obj object-name field-name new-exp)
          (if (object? obj)
              (cases object obj
                (an-object (ids exps)
                           (let ((pos (rib-find-position field-name ids)))
                             (if (number? pos)
                               (vector-set! exps pos new-exp)
                               (eopl:error 'aux-update "El campo ~s no se encontro en el objeto ~s" field-name object-name)
                                 ))))
              (eopl:error 'aux-update "El parametro ~s no es un objeto"))))

;;aux-clone: <object> <symbol> -> object
;;Funcion auxiliar utilizada para clonar un objeto
(define aux-clone
        (lambda (obj object-name)
          (if (object? obj)
              (cases object obj
                (an-object (ids exps)
                           (let ((vect (make-vector (vector-length exps))))
                             (begin (aux-vect (vector->list exps) vect 0)
                                    (an-object ids vect)))))))) 

;; aux-vect: list vector number -> vector
;; Funcion auxiliar utilizada para llenar un vector
(define aux-vect
  (lambda (list-exps vect pos)
    (if (null? (cdr list-exps))
        (vector-set! vect pos (car list-exps))
        (begin
          (vector-set! vect pos (car list-exps))
          (aux-vect (cdr list-exps) vect (+ pos 1))))))

;; lista-a-vector : list -> vector
;; Funcion para convertir una lista en un vector
(define lista-a-vector
  (lambda (args)
    (list->vector args)))

;; funcion auxiliar para aplicar eval-expression a cada elemento de una 
;; lista de operandos (expresiones)
;; eval-rands : <list-of expressions> <enviroment> -> (lista de numeros)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;; funcion auxiliar para aplicar eval-expression una expresion 
;; eval-rand : <expression> <enviroment> -> numero
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;; true-value :  -> true
(define true-value (lambda () 'true))
;; false-value :  -> false
(define false-value (lambda () 'false))

;; true-value? : <symbol> -> bool
;; funcion auxiliar que verifia si una variable es igual a diferente de cero
(define true-value?
           (lambda (x)
             (not (zero? x)))) 

;; Ambiente Inicial
(define init-env 
  (lambda ()
        (let ((env (extended-env-record '() (list->vector '()) (empty-env))))
             env)))








;*********************** Ambientes ********************


;; empty-env:  -> enviroment
;; funcion que crea un ambiente vacio
(define empty-env  
  (lambda ()
    (empty-env-record)))       

;; eextend-env:  -> enviroment
;; funcion que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;; extend-mutable-env:  -> enviroment
;; funcion que crea un ambiente mutable
(define extend-mutable-env
  (lambda (syms vals env)
    (extended-mutable-env-record syms (list->vector vals) env)))

;; extend-env-recursively:  -> enviroment
;; procedimiento para extender un ambiente recursivamente
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))


;; procedimiento para aplicar un ambiente, utilizado en la expresion let
(define apply-env
  (lambda (env sym)
    (apply-env-ref env sym)))

;; procedimiento para aplicar un ambiente, utilizado para las demas expresiones diferentes al let
(define apply-env-aux-let
  (lambda (env sym)
    (deref (apply-env-let env sym))))

;; procedimiento para aplicar un ambiente, utilizado en la expresion let
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              ;(a-ref pos vals)
              (eopl:error 'apply-env-ref "~s no se puede modificar" sym)
              (apply-env-ref env sym))))
      (extended-mutable-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (a-ref pos vals)
            (apply-env-ref env sym))))
      )))

;; procedimiento para aplicar un ambiente, utilizado para las demas expresiones diferentes al let
(define apply-env-let
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-let "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              ;(eopl:error 'apply-env-ref "~s is immutable" sym)
              (apply-env-let env sym))))
      (extended-mutable-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (a-ref pos vals)
            (apply-env-let env sym))))
      )))




;*********************** Otras Funciones Auxiliares ********************

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;*********************** Llamado al interpretador ********************
(interpretador)



;*********************** Pruebas ********************

obliq-> a
apply-env-let: No binding for a
obliq-> 3
3
obliq-> 'a'
|'a'|
obliq-> "acascas2123ascsa acsc"
"\"acascas2123ascsa acsc\""
obliq-> ok
#<void>
obliq-> var x=10 y=20 in +(x,y) end
30
obliq-> let x=10 in set x:=20 end
apply-env-ref: x no se puede modificar
obliq-> let rec fact(n) = if is(n,0) then 1 else *(n, apply fact(-(n,1))) end in apply fact(5) end
120
obliq-> var x=10 in begin set x:=20; x end end
20
obliq-> +(-(40,30),*(2,2),/(10,2),%(5,2))
14
obliq-> &("aacsca","bcasca")
"\"aacsca\"\"bcasca\""
obliq-> and(<(1,10),>(1,10))
false
obliq-> or(<=(1,1),>=(1,10))
true
obliq-> var x=10 in if is(10,0) then 100 else 200 end end
200
obliq-> var x=10 in if is(10,0) then 100 elseif is(50,50) then 500 else 200 end end
500
obliq-> let sumaResta = proc(x,y,z) +(-(x,y),z) end in apply sumaResta(20 10 50) end 
60
obliq-> var r=0 x=0
in begin for x=1 to 5 do set r:=+(r,x) end; x end end
0
obliq-> var r=0 in for x = 1 to 5 do begin set r := +(r,x) ; x end end end
(1 2 3 4 5)
obliq-> let o1 = object { o1 => 6 } end 
           in get o1.o1 end 
6
obliq-> let o1 = object { o1 => 6 } end 
           in begin update o1.o1 := +(5,6) ; get o1.o1 end end
11





3

'a'

"acascas2123ascsa acsc"

ok

var x=10 y=20 in +(x,y) end

let rec fact(n) = if is(n,0) then 1 else *(n, apply fact(-(n,1))) end in apply fact(5) end

var x=10 in begin set x:=20; x end end

+(-(40,30),*(2,2),/(10,2),%(5,2))

&("aacsca","bcasca")

and(<(1,10),>(1,10))

or(<=(1,1),>=(1,10))

var x=10 in if is(10,0) then 100 else 200 end end

var x=10 in if is(10,0) then 100 elseif is(50,50) then 500 else 200 end end

let sumaResta = proc(x,y,z) +(-(x,y),z) end in apply sumaResta(20 10 50) end 

var r=0 x=0
in begin for x=1 to 5 do set r:=+(r,x) end; x end end

var r=0 in for x = 1 to 5 do begin set r := +(r,x) ; x end end end

let o1 = object { o1 => 6 } end in begin update o1.o1 := +(5,6) ; get o1.o1 end end

let o1 = object { o1 => 6 } end in get o1.o1 end

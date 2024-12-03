#lang eopl

;***********************************************************************************************************************
;***********************************************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos, 
;;;;; procedimientos recursivos y type checker

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<optional-type-exp> <identificador>}*(,)) <expression>
;;                      <proc-exp (arg-texps ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {<optional-type-exp> identifier ({<optional-type-exp> identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp result-texps proc-names arg-texpss idss bodies bodyletrec>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;**********************************************    Especificación Léxica   *********************************************
;***********************************************************************************************************************

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment("%" (arbno (not #\newline))) skip)
    (identifier("@" letter (arbno (or letter digit "?"))) symbol)
    (float(digit (arbno digit) "." digit (arbno digit)) number)
    (float("-" digit (arbno digit) "." digit (arbno digit)) number)
    (inmutable-identifier("const" "@" letter (arbno (or letter digit "?"))) symbol)
    (numero(digit (arbno digit)) number)
    (numero("-" digit (arbno digit)) number)
    
    (text(letter (arbno (or letter digit "_" ":" "." "?"))) string)
    ))

;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '((program (expression) a-program)
    
    (expression (numero) ent-exp)
    (expression (float) float-exp)
    (expression (identifier) var-exp)
    (expression("\"" text "\"" )text-exp)
    (expression ("GLOBALS" "{"(arbno type-exp assing)"}"
                           "PROGRAM" "{" type-exp "main()"
                           "{" expression "}" "}"
                           )global-exp)
    (expression("LOCALS" "{" (arbno type-exp assing) "}""{" expression (arbno expression)"}")locals-exp)

    (expression ("BLOCK" "{" expression (arbno expression )"}")block-exp)
    (expression ("print" "(" expression ")" ) print-exp)
    (expression ("letrec" (arbno type-exp identifier
                                 "(" (separated-list type-exp identifier ",") ")"
                                 "=" expression) "in" expression)letrec-exp)
    (expression (primitive-una "(" (arbno expression) ")") primun-exp)
    (expression("fun" "("(separated-list type-exp identifier ",") ")" expression )proc-exp)
    (expression("call" expression "("(separated-list expression ",")")") call-exp)
    (expression( "apply-bin" "(" expression primitive-bin (arbno expression) ")")primbi-exp)
    (expression("if" expression "{" expression "}" "else" "{" expression "}")if-exp)
    (expression("dec" "(" (arbno type-exp assing) ")""{"expression "}")decLocal-exp)
    (expression ("set" var "=" expression)set-exp)
    (expression ("while" "(" expression ")" "do" "(" expression (arbno expression) ")")while-exp)
    (expression ( "for" "(" identifier "=" expression ";" expression ";" expression ")"
                        "{"expression (arbno expression)"}")for-exp)
    (expression ( "switch" "(" expression ")" "{" (arbno "case" expression ":" expression ) "default:" expression "}")
                switch-exp)
    ;soporte listas
    (expression("(" (separated-list expression ",") ")")list-exp)
    ;soporte vectores
    (expression ("["(separated-list expression ",")"]" ) vect-exp)

    ;soporte doccionarios
    (expression ("{"(separated-list  expression ":" expression ",") 
                    "}") dict-exp)
    ;soporte grafos
    (expression ( "edge" "(" identifier "," identifier ")") edge-gra-exp)
    (expression ( "edges" "(" (arbno expression ) ")" ) edges-gra-exp)
    (expression ( "vrt" "(" (separated-list identifier ",") ")") vertices-g-exp)
    (expression ( "graph" "(" expression "," expression ")") graph-g-exp)
    (primitive ("zero?") zero-t-prim)
    (var (identifier) mutable-var)
    (var ("const" identifier) inmutable-var)
    (assing (var "=" expression ";") assing-exp)
    
    ; binary-Primitive-exp
    (primitive-bin ("+") suma-prim)
    (primitive-bin  ("-") resta-prim)
    (primitive-bin ("/") div-prim)
    (primitive-bin  ("*") multi-prim)
    (primitive-bin  ("concat")concat-prim)
    (primitive-bin  ("mod") mod-prim)
    (primitive-bin  (">")mayor-prim)
    (primitive-bin  ("<") menor-prim)
    (primitive-bin  (">=") mayorIgual-prim)
    (primitive-bin  ("<=") menorIgual-prim)
    (primitive-bin  ("!=") dif-prim)
    (primitive-bin  ("==") comparador-prim)
    (primitive-bin ("append") append-prim)
    (primitive-bin ("add-edge") addEdge-prim)
    (primitive-bin ("append_V")appendVector-prim)
    (primitive-bin ("delete")deleteV-prim)
    (primitive-bin ("ref_V")refVector-prim)
    (primitive-bin ("set_V")setVector-prim)
    (primitive-bin ("ref_D")refDict-prim)
    (primitive-bin ("set_D")setDict-prim)
    (primitive-bin ("incoming-n") vecinosEn-prim)
    (primitive-bin ("outgoing-n") vecinosSa-prim)
    
    ;;---------------------------

    
    ;unary-primitive-exp
    (primitive-una ("len") lenght-prim)
    (primitive-una ("add_n") addn-prim)
    (primitive-una ("sub_n") subn-prim)
    (primitive-una ("neg") negBoolean-prim)
    (primitive-una("list?")list-prim)
    (primitive-una("values_D")valuesD-prim)
    (primitive-una("keys_d")keysDict-prim)
    (primitive-una("make_L") makeL-prim)
    (primitive-una("make_V") makeV-prim)
    (primitive-una("make_D") makeD-prim)
    (primitive-una("make_g") makeG-prim)
    (primitive-una("edges_G") edges-prim)
    (primitive-una("vertices") vertices-prim)
    (primitive-una("head") head-prim)
    (primitive-una("tail") tail-prim)
    (primitive-una("empty?") empty-prim)
    
    ;caracteristicas adicionales
    (type-exp ("list" "<" type-exp ">") list-type-exp)
    (type-exp ("int") int-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("string") String-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("void") void-type)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")") proc-type-exp)
    (type-exp ("edge") edge-type-exp)
    (type-exp ("edges")  edges-type-exp)
    (type-exp ("vertices") vertices-type-exp)
    (type-exp ("graph")graph-type-exp)
    (type-exp ("list<generic>") generic-list-type)
    (type-exp ("list<int>") list-int-type)
    (type-exp ("list<float>") list-float-type)
    (type-exp ("list<bool>") list-bool-type)
    (type-exp ("list<string>") list-string-type)
    (type-exp ("vector<int>") vector-int-type)
    (type-exp ("vector<float>") vector-float-type)
    (type-exp ("vector<bool>") vector-bool-type)
    (type-exp ("vector<string>") vector-string-type)
    (type-exp("dict< string, int>") dict-int-type)
    (type-exp("dict< string, float>") dict-float-type)
    (type-exp("dict< string, bool>") dict-bool-type)
    (type-exp("dict< string, string>") dict-string-type)

    
   
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("list") emptyList-exp)
    
    ))




;***********************************************************************************************************************
;***********************************************************************************************************************




;***********************************************************************************************************************
;************************       Tipos de datos para la sintaxis abstracta de la gramática      *************************
;***********************************************************************************************************************

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (arg-texps (list-of type-exp?))
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (result-texps (list-of type-exp?))
;   (proc-names (list-of symbol?))
;   (arg-texpss (list-of type-exp?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?)))

;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim)
;  (zero-test-prim))


;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;*******************************************    Parser, Scanner, Interfaz     ******************************************
;***********************************************************************************************************************

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;-----------------------------------------------------------------------------------------------
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))


;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador-sin-tipos
  (sllgen:make-rep-loop  "--> "
                         (lambda (pgm) (eval-program pgm)) 
                         (sllgen:make-stream-parser 
                          scanner-spec-simple-interpreter
                          grammar-simple-interpreter)))

(define interpretador
  (sllgen:make-rep-loop  "--> "
                         (lambda (pgm) (begin (type-of-program pgm)
                                              (eval-program pgm))) 
                         (sllgen:make-stream-parser 
                          scanner-spec-simple-interpreter
                          grammar-simple-interpreter)))



;***********************************************************************************************************************
;************************************************    El Interprete      ************************************************
;***********************************************************************************************************************
;eval-program: <programa> -> numero
;función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))


;ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@w @x @y @z @k @f)
     '(1 2 3 "hola" "FLP" (non-empty-pair 'a 'b) )
     (empty-env))))


;eval-expression: <expression> <enviroment> -> numero
;evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (ent-exp (datum) datum)
      (float-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (text-exp (text) text)
      (true-exp () "true")
      (false-exp () "false")
      (emptyList-exp () (empty-list))
      (global-exp(types exps type-body main-body)
                 (let*(
                       (ids (map eval-var (map id-var exps)))
                       (ids-bodies (eval-rands (map exp-var exps) env))
                       (global-env (extend-recursive-env ids ids-bodies env))
                       )
                   (begin
                   (eval-expression main-body global-env))
                   )
                 )
      (locals-exp(ids-types exps first-instruct instructions)
                 (let*(
                       (ids (map eval-var (map id-var exps)))
                       (exps (map exp-var exps))                      
                       (env (progresive-env ids exps env))
                       )
                   
                   (let loop ((acc (eval-expression first-instruct env))
                              (exps instructions))                    
                     (if (null? exps) 
                         acc
                         (loop (eval-expression (car exps) env)
                               (cdr exps)))))
                 )
      (block-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) env)
                             (cdr exps)))))
      (print-exp (exp)
                 (begin
                 (display (eval-expression exp env))
                 (newline)
                 "print"
                  ))
       (letrec-exp (type-proc proc-names arg-types args proc-bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-recursively-ite proc-names args proc-bodies env))
                  )
       (primun-exp (prim rand)
                      (let ((arg (if (> (length rand) 1)
                                     (eval-rands rand env)
                                     (eval-expression (car rand) env)
                                     )))
                        (apply-primitive-u prim arg)))
      (proc-exp (ids-types ids body)
                (closure ids body env))
      (call-exp (rator rands)
               (let (
                     (proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     proc
                     )))
      (decLocal-exp(types exps body)
                   (let ((args (eval-rands (map exp-var exps) env))
                         (ids (map eval-var (map id-var exps))))
                     (eval-expression body
                                      (extend-env ids args env))))
       (set-exp (id rhs-exp)
               (let((id (eval-var id)))
                 (if (number? (find-position id (symbols-env env)))
                     (begin 
                       (setref!
                        (apply-env-ent env id)
                        (eval-expression rhs-exp env))1)
                     (eopl:error  "Attempt to change the value of an immutable variable"))))
      (primbi-exp (exp1 prim exp2)
                       (let ((arg1 (eval-expression exp1 env))
                             (arg2 (if (equal? (length exp2) 2)
                                           (non-empty-list (list (eval-expression (car exp2) env)
                                                             (eval-expression (cadr exp2) env)))
                                       (if (es-primitiva-de-vecinos? prim)
                                           (get-var-id (car exp2))
                                           (eval-expression (car exp2) env))))
                             )(apply-primitive-b prim (list arg1 arg2))))
      (if-exp(test-exp true-exp false-exp)
             (if(equal? (eval-expression test-exp env) "true")
                (eval-expression true-exp env)
                (eval-expression false-exp env)))
      (while-exp (test-exp first-exp exps)
                 (let while ((t-exp (eval-expression test-exp env)))
                   (if (equal? t-exp "true")
                       (begin
                         (let loop ((acc (eval-expression first-exp env))
                                    (exps exps))                    
                           (if (null? exps) 
                               acc
                               (loop (eval-expression (car exps) env)
                                     (cdr exps))))
                         (while (eval-expression test-exp env)))
                       "fin while")))
      (for-exp (id beginning stop-cond sumator first-exp exps)
               (let*((vec (list->vector (list (eval-expression beginning env))))
                     (env (extended-env-record (list id) vec env)))
                 (let ciclo()                 
                   (let ((stop (eval-expression stop-cond env)))
                     (if (equal? stop "true")
                         (begin
                           (let loop ((acc (eval-expression first-exp env))
                                      (exps exps))                    
                             (if (null? exps) 
                                 acc
                                 (loop (eval-expression (car exps) env)
                                       (cdr exps))))
                           (eval-expression sumator env)
                           (ciclo))
                         "fin del for"
                         )))))
      (switch-exp (option coincidences coincidences-exps default-exp)
                  (let((coincidence-exp (coincidence-case
                                         (eval-expression option env)
                                         (eval-rands coincidences env)
                                         coincidences-exps)))
                    (if (string? coincidence-exp)
                           (eval-expression default-exp env)
                           (eval-expression coincidence-exp env))))
      (list-exp (args)
                (if (null? (eval-rands args env))
                    (empty-list)
                    (non-empty-list (eval-rands args env))))
      (vect-exp (args)
                (if (null? (eval-rands args env))
                    (empty-vec)
                    (non-empty-vec (list->vector (eval-rands args env)))))
      (dict-exp (keys values)                
                (non-empty-dict (eval-rands keys env)
                                (list->vector (eval-rands values env))))
      (edge-gra-exp (v1 v2) (edge-gra-exp v1 v2))
      (edges-gra-exp (edges) (edges-gra-exp (eval-rands edges env)))
      (vertices-g-exp (vers) (vertices-g-exp vers))
      (graph-g-exp (vers edges) (graph-g-exp vers edges))
      (else "faltan casos eval-expression")
      )    
    ))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;funcion que aplica eval-expression a un solo elemento
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;Funcion que define los valores de verdad, 0 = false y diferente de 0 = true
;true-value?: numero -> boolean
(define true-value?
  (lambda (x)
    (not (zero? x))))



;----------------------------contructores-----------------------------------

;define-datatype de los procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))


;Funcion que extiende un ambiente y evalua un procedimiento en ese nuevo ambiente extendido
;apply-procedure: <procedure> <list-of expression> -> numero
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))


;------------------------------ENVIRONMENTS-FUNTIONS-----------------------------


;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vect vector?)
   (env environment?)))

;definicion de scheme-value
;cualquier cosa es un scheme-value
(define scheme-value? (lambda (v) #t))

;empty-env: -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)));llamado al constructor de ambiente vacío 

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-recursively-ite
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (positions ids body)
             (vector-set! vec positions (closure ids body env)))
           (generate-list len) idss bodies)
          env)))))
 
;extend-recursive-env (lisf-of symbols) <list-of expressions> <environment> -> environment
;Funcion que crea un ambiente extendido, si el identificador es una variable guarda la variable
;si es un procedimiento crea una closure y la guarda
(define extend-recursive-env
  (lambda (ids exps old-env)
    (let* ((len (length ids))
           (vec (make-vector len))
           (env (extended-env-record ids vec old-env)))
      (for-each
       (lambda (pos body)
         (if (procval? body)
             (let((ids-bodies (cases procval body (closure (ids body env)(list ids body)))))
               (vector-set! vec pos (closure (car ids-bodies) (cadr ids-bodies) env))
               )                  
             (vector-set! vec pos body))
         )            
       (generate-list len) exps)         
      env
      )
    )
  )
;función que genera una  lista de los números desde 0 hasta end
(define generate-list
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ent env sym))))

;(apply-env-ent env sym)))

(define apply-env-ent
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ent "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let* ((sym (if (inmutable-var? syms sym)
                                           (inmutable-var-convert sym)
                                           sym))
                                  (pos (find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ent env sym)))))))

;***************************ENVIRONMENT-TYPE**************************************
(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (syms (list-of symbol?))
   (vals (list-of type?))
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

;apply-tenv <environment> <symbol> -> type
;Funcion que busca el tipo de una variable en un ambiente de tipos 
(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
                            (let* ((sym (if (inmutable-var? syms sym)
                                            (inmutable-var-convert sym)
                                            sym))
                                   (pos (list-find-position sym syms)))
                              (if (number? pos)
                                  (list-ref vals pos)
                                  (apply-tenv env sym)))))))
;---------------------------AUXILIAR-FUNTIONS------------------------------------------

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente
(define find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (equal? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))


;convierte de string a int

(define convert-to-string
  (lambda (a)
    (cond
      ((number? a) (number->string a))
      ((symbol? a) (symbol->string a))
      (else a)
      )
    )
  )

;comparar-strings? <string> <string> -> bool
;Funcion que compara 2 strings de acuerdo con un indentificador dado
(define comparar-cadena?
  (lambda (val1 val2 evaluar)
    (cond
      [(equal? evaluar '<) (string<? val1 val2)]
      [(equal? evaluar '>) (string>? val1 val2)]
      [(equal? evaluar '>=) (string>=? val1 val2)]
      [(equal? evaluar '<=) (string<=? val1 val2)]
      [else (eopl:error "no existen o no se pueden comparar la cadena de caracteres")])))

;comparar-strings? <number> <number> -> bool
;Funcion que compara 2 numeros de acuerdo con un indentificador dado
(define comparar-int?
  (lambda (num1 num2 evaluar)
    (cond
      [(equal? evaluar '<) (< num1 num2)]
      [(equal? evaluar '>) (> num1 num2)]
      [(equal? evaluar '>=) (>= num1 num2)]
      [(equal? evaluar '<=) (<= num1 num2)]
      [else (eopl:error "no se pueden comparar los numeros")])))        
    

;comparar <scheme-value> <scheme-value> -> boolç
;Funcion que recibe 2 valores y si ambos son ò strings ò numeros los compara
(define comparar-strings-int
  (lambda (e1 e2 evaluar)
    (cond
      [(and (string? e1) (string? e2)) (comparar-cadena? e1 e2 evaluar)]
      [(and (number? e1) (number? e2)) (comparar-int? e1 e2 evaluar)]
      [else (eopl:error "valor esperado, dos strings o dos flotantes")])))
       


;Funcione que retorna true si una operacion binaria es verdadera, o false si
;la exprecion es falsa
;valor_verdad?: primitiva-binaria x expression x expression => number
;usage:(valor-true? prim exp1 exp2) => 1 si al aplicar la primitiva a
;los 2 argumentos el valor es diferente a 0, 0 de caso contrario
(define valor-true?
  (lambda (bool)
    (if bool "true" "false")))

;real-mod <number> <number> -> number
;Funcion modulo para numeros reales
(define real-mod
  (lambda (x y)
          (- x (* (truncate (/ x y)) y))))

;progresive-env <list-of symbols> <list-of expression> <environment> -> environment
;Funcion que retorna un ambiente extendido en el cual la declaracion de una variable
;conoce a todas las variables predecesoras
(define progresive-env
  (lambda (ids exps env)
    (let* ((len (length ids))
           (vec (make-vector len))
           (env (extended-env-record ids vec env)))
      (for-each
       (lambda (pos exp)
        (vector-set! vec pos (eval-expression exps env)))
       (generate-list len) exps)
      env)))
;-------------------------LISTAS---------------------------------------
;datatype para representar las listas
(define-datatype lista lista?
  (empty-list)
  (non-empty-list (values list?))
  )


;Funcion que extrae la list de valores de un datatype lista
(define extract-list-valores
  (lambda (ls)
    (cases lista ls
      (empty-list()(list))
      (non-empty-list (values) values))
    )
  )
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,FUNCIONES SOPORTE VECTORES,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;datatype  vectores
(define-datatype vect vect?
  (empty-vec)
  (non-empty-vec (values vector?))
  )


;extraer los valores de un vector, verificando si el vector está vacío o no.
(define extract-vector-val
  (lambda (vector)
    (cases vect vector
      (empty-vec ()( #() ))
      (non-empty-vec (values) values))
    )
  )


;agregar un valor al final de un vector, extendiéndolo con un nuevo elemento

(define extend-vector
  (lambda (vec value)
    (let((lista1 (vector->list (extract-vector-val vec)))
         (lista2 (list value)))
      (list->vector (append lista1 lista2))
      )
    )
  )


;eliminar un valor de un vector en una posición específica.
;La función trabaja transformando temporalmente el vector en una lista, eliminando el elemento correspondiente

(define remove-at
  (lambda (vector pos)
    (let((delete-val (vector-ref (extract-vector-val vector) pos))
         (ls (vector->list(extract-vector-val vector))))
      (list->vector (remove-from-list ls delete-val))
      )
    )
  )


;funcion auxiliar que elimina todas las ocurrencias consecutivas de un valor (value) de una lista (lista),
;devolviendo una nueva lista
(define remove-from-list
  (lambda (ls val)
    (cond
      ((null? ls) empty)
      ((not(= (car ls) val))
       (cons (car ls) (remove-from-list (cdr ls) val)))
      ((and (= (car ls) val) (not(null? (cdr ls))))
       (cons (cadr ls) (remove-from-list (cddr ls) val))
       )
      (else empty)
      )
    )
  )
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,SOPORTE DICCIONARIOS,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;datatype para representar diccionarios
(define-datatype dictionary dictionary?
  (empty-dict)
  (non-empty-dict (keys list)
                  (values vector?))
  )


;crea un diccionario a partir de una lista de claves (llave) y un valor o conjunto de valores (val),
;manejando correctamente casos donde el diccionario pueda ser vacío o no.

(define make-dict
  (lambda (llave val)
    (let((llave1 (extract-list-valores llave))
         (val1 (if (lista? val)
                      (list->vector (extract-list-valores val))
                      (extract-vector-val val)
                      ))
         )
      
      (if (and (null? llave1) (= (vector-length val1)) 0)
          (empty-dict) 
          (non-empty-dict llave1 val1))
      )
    )
  )



;devuelve las claves de un diccionario (dict). Si el diccionario está vacío, devuelve "{}"
(define dict-keys
  (lambda (dict)
    (cases dictionary dict
      (empty-dict() "{}")
      (non-empty-dict (claves val)
                      claves)
      )
    )
  )


;Fdevuelve los valores asociados a las claves de un diccionario (dict).
;Si el diccionario está vacío, devuelve "{}" como representación del diccionario vacío. Si no está vacío,
;devuelve los valores almacenados en el diccionario.
(define dict-val
  (lambda (dict)
    (cases dictionary dict
      (empty-dict() "{}")
      (non-empty-dict (claves val)
                      val)
      )
    )
  )


;La función dict-ref permite recuperar el valor asociado a una clave específica (clave) en un diccionario (dict).
(define dict-ref
  (lambda (dict clave)    
    (let ((pos (list-find-position clave (dict-keys dict))))
      (if (not pos)
          (eopl:error "La clave no existe en el diccionario")
          (vector-ref (dict-val dict) pos)))))




;La función dict-set actualiza el valor asociado a una clave específica en un diccionario (dict).
;El nuevo valor se proporciona como un par clave-valor (val), y la función devuelve la cadena "cambio se ralizado"
(define dict-set
  (lambda (dict val)
    (let((pos (list-find-position (car (extract-list-valores val)) (dict-keys dict)))
         (val-vector (dict-val dict))
         )
      (vector-set! val-vector pos (cadr (extract-list-valores val)))
      "cambio se ralizado")))

;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,SOPORTE grafos,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;datatypes para definir los grafos dirigidos
(define-datatype grafos grafos?
 (graph-exp (v vertices?)
        (g aristas?)))

(define-datatype vertices vertices?
  (vertices-exp (v (list-of symbol?))))

(define-datatype aristas aristas?
  (edges-exp (e (list-of arista?))))

(define-datatype arista arista?
  (edge-exp (sym-left symbol?)
            (sym-right symbol?)))



;La función remove-dups elimina todos los duplicados de una lista,
;dejando solo la primera aparición de cada elemento
(define remove-dups
  (lambda (l)
    (if (null? l)
        (list)
        (let((index-list (remove-dups (cdr l))))
          (if (member (car l) index-list)
              index-list
              (cons (car l) index-list)
              )))))


;La función make-graph construye un grafo con vértices (vertices) y aristas (aristas) si estas son válidas. De lo contrario,
;genera un error indicando que algunas aristas no están definidas dentro del conjunto de vértices.

(define make-graph
  (lambda (vertices aristas)
    (if (edges-valid? vertices aristas)
        (graph-exp (vertices-exp vertices)
                   (edges-exp (create-edges  aristas)))
        (eopl:error "Algunas aristas no están en los vértices")
        )
    )
  )
;La función edges-valid? verifica si todas las aristas de una lista (aristas)
;están compuestas únicamente por vértices presentes en otra lista (vertices).
;Devuelve #t si todas las aristas son válidas, y #f si alguna no lo es.

(define edges-valid?
  (lambda (vertices aristas)
    (cond((null? aristas) #t)
        ((and (member (caar aristas)vertices)
              (member (cadr (car aristas)) vertices))
         (edges-valid? vertices (cdr aristas)))
        (else #f)
        )
    )
  )

;valid-rands-for-graph  <scheme-value> <scheme-value> -> bool
;Funcion que retorna 2 argumentos son validos para crear un grafo
(define valid-graph-data?
  (lambda  (vts ars)
    (if (or (and (vertices? vts) (aristas? ars)) (and (lista? vts) (lista? ars) ))
        #t
        #f
        )
  )
)

;La función es-primitiva-de-vecinos? determina si una operación (denotada por prim)
;es una de dos primitivas específicas relacionadas con los vecinos en un grafo

(define es-primitiva-de-vecinos?
  (lambda (primitiva)
    (cases primitive-bin primitiva
      (vecinosEn-prim()#t)
      (vecinosSa-prim()#t)
      (else #f)
      )
    )
  )

;La función get-vertices extrae los vértices de una estructura vertices, que parece representar un conjunto de vértices en un grafo.
;Esta estructura está definida por el tipo vrs-exp, y el caso específico que maneja es el constructor vertices-exp
(define get-vertices
  (lambda (vrs)
        (cases vertices vrs
          (vertices-exp (v) v))
        (eopl:error "Entrada no válida: debe ser del tipo vrs-exp")))

;La función get-edges extrae todas las aristas de una estructura aristas, que parece estar definida por un tipo ars-exp.
;Cada arista en la estructura se procesa mediante una función llamada get-edge-ends


(define get-edges
  (lambda (ars)
        (cases aristas ars
          (edges-exp (a) (map get-edge-ends a)))
        (eopl:error "Entrada no válida: debe ser del tipo es-exp")))

;La función get-edge-ends extrae los extremos (vértices) de una arista arista, que está definida como una estructura del tipo edg-exp.
;Cada arista se representa mediante el constructor edge-exp y contiene dos componentes,
;iz (vértice izquierdo) y dr (vértice derecho).

(define get-edge-ends
  (lambda (ars)
        (cases arista ars
          (edge-exp (iz dr) (list iz dr)))
        (eopl:error "Entrada no válida: debe ser del tipo e-exp")))

;La función create-edges transforma una lista de pares de vértices (edges)
;en una lista de aristas representadas como estructuras del tipo edge-exp.
;Esto es útil para estandarizar el formato de las aristas dentro de un grafo.


(define create-edges
  (lambda (aristas)    
  (map
   (lambda (ars)
    (edge-exp (car ars) (cadr ars))) aristas)

  )
)

;La función get-var-id extrae el valor asociado a una expresión del tipo var-exp.
;Si la expresión no es de este tipo, devuelve un mensaje indicando que no es una expression valida var-exp

(define get-var-id
  (lambda (exp)
    (cases expression exp
      (var-exp(id)id)
      (else "no es una expression valida var-exp"))
    )
  )
;crea una estructura llamada arista-graph, que representa un conjunto de aristas.
;Utiliza los constructores edges-exp y edge-exp para estructurar las aristas
;en un formato estándar.
(define arista-graph
  (edges-exp
   (list (edge-exp 'a 'b)
         (edge-exp 'b 'c)
    )
   )
  )
; La función symbol-a-strings convierte una lista de símbolos (lista-sym) en una lista de cadenas de texto (strings).
;Utiliza la función symbol->string de esta libreria racket, que transforma cada símbolo en su representación textual.

(define symbol-a-strings
  (lambda (lista-sym)
    (map symbol->string lista-sym))
)

;toma como entrada una lista l, donde cada elemento de la lista es a su vez una lista de símbolos.
;La función aplica symbol-a-strings a cada sublista de l,
;transformando los símbolos en cadenas de texto para todas las sublistas

(define symbol-list-strings
  (lambda (lista-sym)
    (map symbol-a-strings lista-sym)
  )
)

(define strings-a-symbols
  (lambda (lista-sym)
    (map string->symbol lista-sym)
  )
)

(define string-list-symbols
(lambda (lista-sym)
  (map strings-a-symbols lista-sym))
)
;;La función insert-edge-graph agrega una arista de tipo arista a un grafo grafo. Si la arista ya existe en el grafo,
;no realiza ningún cambio. El resultado es un nuevo grafo que incluye la arista especificada
;si no estaba previamente presente.
(define insert-edge-graph
  (lambda (grafo arista)
    (let*(
          (nueva-arista (edge-exp (car arista) (cadr arista)))
          (nuevos-vertices (cases grafos grafo (graph-exp (vrs ars) vrs)))
          (nuevas-aristas (cases grafos grafo (graph-exp (vrs ars) ars)))
          (lista-aristas (cases aristas nuevas-aristas ( edges-exp (lista-sym) lista-sym)))
          (ls-nueva-aristas (if (not (arista-in-list? lista-aristas nueva-arista))
                             (agregar-arista nueva-arista lista-aristas)
                             lista-aristas
                             )
                         )
          (new-graph (graph-exp nuevos-vertices (edges-exp ls-nueva-aristas)))
          )new-graph
      )
    )
  )

;;La función arista-in-list? verifica si una arista edge está presente en una lista de aristas aristas. Devuelve:
;#t si la arista está en la lista.
;#f si no está.


(define arista-in-list?
  (lambda (ars edge)
    (if(null? ars)
       #f
       (let(
            (a (cases arista edge (edge-exp (a b) a)))
            (b (cases arista edge (edge-exp (a b) b)))
            (c (cases arista (car ars) (edge-exp (c d) c)))
            (d (cases arista (car ars) (edge-exp (c d) d)))
            )(or (and (eqv? a c) (eqv? b d))
                 (arista-in-list? (cdr ars) edge))
         ))
    )
  )

;;La función agregar-arista inserta una arista arista al final de una lista de aristas aristas.
;Si la lista está vacía, simplemente devuelve una lista con la arista.

(define agregar-arista
  (lambda(arista aristas)
    (if (null? aristas)
        (list arista)
        (cons (car aristas) (agregar-arista arista (cdr aristas)))
        )
    )
  )




;;funcion auxiliar

;;La función get-vecinos-entrantes toma un grafo y un vértice a y devuelve una lista de todos los vértices que tienen
;aristas entrantes hacia a. Es decir, busca todas las aristas del grafo donde a es el vértice final.

(define get-vecinos-entrantes
  (lambda (grafo vrt )
    (cases grafos grafo
      (graph-exp (ver ars)
                 (letrec(

                         (lista-aristas (cases aristas ars (edges-exp (lista-sym) lista-sym)))
                         (find-vecinos
                          (lambda (vertices lista-ars)
                            (if (null? lista-ars)
                                empty
                                (let*(
                                    (x (cases arista (car lista-ars) (edge-exp (a b) a)))
                                    (y (cases arista (car lista-ars) (edge-exp (a b) b)))
                                    (resultado(if (eqv? vertices y) 
                                    (cons x (find-vecinos vertices (cdr lista-ars)))
                                    (find-vecinos vertices (cdr lista-ars))

                                    ))
                                    )
                                  resultado
                                  )))))
                   (find-vecinos vrt lista-aristas))))))

;La función vecinos-salientes toma un grafo y un vértice vrt y devuelve una lista de todos los vértices que son
;destinos de aristas salientes desde vrt. En otras palabras, encuentra todos los vértices hacia los cuales vrt
;tiene conexiones.

(define get-vecinos-salientes
  (lambda (grafo vrt )
    (cases grafos grafo
      (graph-exp (ver ars)
                 (letrec(
                         (lista-aristas (cases aristas ars (edges-exp (lista-sym) lista-sym)))
                         (find-vecinos
                          (lambda (vertice lista-ars)
                            (if (null? lista-ars)
                                empty
                                (let*(
                                    (x (cases arista (car lista-ars) (edge-exp (a b) a)))
                                    (y (cases arista (car lista-ars) (edge-exp (a b) b)))
                                    (resultado

                                    (if (eqv? vertice x) 
                                    (cons y (find-vecinos vertice (cdr lista-ars)))
                                    (find-vecinos vertice (cdr lista-ars))
                                    ))
                                    )
                                  resultado
                                  )))))
                   (find-vecinos vrt lista-aristas))))))
;----------------BLOCK-AND-SET-FUNTIONS--------------------------------
;id-var <assing-exp> -> symbol
;Funcion que retorna la id de una exprecion de asignacion
(define id-var
  (lambda(exp)
    (cases assing exp
      (assing-exp (id exp)  id)
      )
    )
  )

;exp-var <assing-exp> -> expression
;Funcion que retorna la expression de un exprecion de asignacion
(define exp-var
  (lambda(exp)
    (cases assing exp
      (assing-exp (id exp)  exp)
      )
    )
  )


;eval-var <var-exp> -> symbol
;Funcion que recibe un var-exp y retorna la representacion, si es mutable retorna l id
;si no es mutable retorna la id antecedida con la palabra conts
(define eval-var
  (lambda (variable)
    (cases var variable
      (mutable-var (id) id)
      (inmutable-var (id) (string->symbol(string-append "conts" (symbol->string id))))
      ))
  )

;caracter-index <string> -> bool or number
;Funcion que recibe un string y retorna la posicion en la cual se encuentra el caracter
;@, si no lo encuentra retorna #f
(define caracter-index
  (lambda (string)
    (let loop ((index 0))
      (cond
        ((= index (string-length string)) #f)
        ((char=? (string-ref string index) #\@) index)
        (else (loop (+ index 1)))
        )
      )
    )
  )

;comparar-symbol <symbol> <symbol> -> bool
;Funcion que recibe 2 symbolos y los compara, ambos deben contener el caracter @
;si el primer symbol inicia con la palabra conts verifica si son iguales despues de la @
;si no inicia con conts retorna #f
(define comparar-symbol
  (lambda (simbolo1 simbolo2)
    (let* ((cadena1 (symbol->string simbolo1))
           (cadena2 (symbol->string simbolo2))
           (indice1 (caracter-index cadena1))
           (indice2 (caracter-index cadena2)))
      (if (and indice1 
               indice2
               (string-prefix? "conts" cadena1))
          (string=? (substring cadena1 (+ indice1 1))
                    (substring cadena2 (+ indice2 1)))
          #f)
      )
    )
  )

;inmutable-var? <list-of symbol> <symbol> -> bool
;Funcion que recibe un symbolo y le añade la palabra conts al inicio
;y verifica si el nuevo symbolo esta en una lista
(define inmutable-var?
  (lambda (list sym)
    (let((const-sym (string->symbol(string-append "conts" (symbol->string sym)))))
      (if (list? (member const-sym list))
          #t
          #f
          )
      )
    )
  )

;inmutable-var-conver <symbol> -> symbol
;Funcion que recibe un symbol y le añade la palabra conts al inicio
(define inmutable-var-convert
  (lambda (sym)
    (string->symbol(string-append "conts" (symbol->string sym)))
    )
  )

;inmutable-var-id <symbol> -> symbol
;Funcion que recibe una inmutable-var y retorna su id
(define inmutable-var-id
  (lambda (sym)
    (let*((cadena (symbol->string sym))
          (indice (caracter-index cadena))
          )
      (string->symbol(substring cadena indice)))
    )
  )

;string-prefix? <string> <string> -> bool
;Funcion que recibe un prefijo(string) y una cadena(string) y verifica
;si la cadena inicia con el prefijo dado
(define string-prefix?
  (lambda (prefijo string)
    (and (>= (string-length string) (string-length prefijo)) 
         (string=? (substring string 0 (string-length prefijo)) prefijo))
    )
  )


;symbols-env <environment> -> <list-of symbols>
;Funcion que extrae la list de symbols de un environment
(define symbols-env
  (lambda (env)
    (cases environment env
      (empty-env-record ()(list))
      (extended-env-record (syms vals env) (append syms (symbols-env env)))
      )
    )
  )
;----------------EXPRESSION-EVALUATOR----------------------------------

;*****************References****************************************
;datatypes para las referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

















;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive-b
  (lambda (prim-b args)
    (cases primitive-bin prim-b
      (suma-prim () (+ (car args) (cadr args)))
      (resta-prim () (- (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (multi-prim () (* (car args) (cadr args)))
      (mod-prim () (if (and (integer? (car args)) (integer? (cadr args)))
                            (modulo (car args) (cadr args))
                            (real-mod (car args) (cadr args))
                            ))
      (concat-prim () (string-append (car args) (cadr args)))
      (mayor-prim () (valor-true? (comparar-strings-int (car args) (cadr args) '>)))
      (menor-prim () (valor-true? (comparar-strings-int (car args) (cadr args) '<)))
      (mayorIgual-prim () (valor-true? (comparar-strings-int (car args) (cadr args) '>=)))
      (menorIgual-prim () (valor-true? (comparar-strings-int (car args) (cadr args) '<=)))
      (dif-prim () (valor-true? (not (eqv? (car args) (cadr args)))))
      (comparador-prim () (valor-true? (equal? (car args) (cadr args))))
      (append-prim() (cond((and (vertices? (car args)) (vertices? (cadr args)))
                               (non-empty-list(append (get-vertices (car args))
                                                      (get-vertices (cadr args)))))
                              ((and (aristas? (car args)) (aristas? (cadr args)))
                               (non-empty-list(append (get-edges (car args))
                                                      (get-edges (cadr args))))
                               )
                              ((not(and (list? (car args)) (list? (cadr args))))
                              (non-empty-list (append (extract-list-valores (car args))
                                                 (extract-list-valores (cadr args)))))(else
                               (eopl:error "Valores no validos para appens ~s y ~s"
                                           (car args)
                                           (cadr args)))))
      (addEdge-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (insert-edge-graph (car args) (cases arista (cadr args)
                                                       (edge-exp (e1 e2)(list e1 e2))))
                                )
                         )
      (appendVector-prim()(non-empty-vec (extend-vector (car args) (cadr args))))
      (deleteV-prim() (non-empty-vec(remove-at (car args)(cadr args))))
      (refVector-prim()(if (vector? (car args))
                                 (vector-ref (car args) (cadr args))
                                 (vector-ref (extract-vector-val (car args))
                                             (cadr args))))
      (setVector-prim()(if (vector? (car args))
                                 (vector-set! (car args) (cadr args))
                                 (let((vec (extract-vector-val (car args)))
                                      (pos&val (extract-list-valores (cadr args)))
                                      )
                                   (vector-set! vec (car pos&val) (cadr pos&val))
                                   "ok"
                                   )
                                 ))
      (refDict-prim() (dict-ref (car args) (cadr args)))
      (setDict-prim() (dict-set (car args) (cadr args)))
      (vecinosEn-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (non-empty-list
                                 (symbol-a-strings (get-vecinos-entrantes (car args) (cadr args))))
                                )
                           )
      
      (vecinosSa-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (non-empty-list
                                 (symbol-a-strings (get-vecinos-salientes (car args) (cadr args))))))
      (else "no hay mas ")                 
      )))


;Funcion que resuelve las operaciones de cada primitiva unaria
;apply-unary-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive-u
  (lambda (prim-u arg)
    (cases primitive-una prim-u
      (lenght-prim () (string-length arg))
      (addn-prim () (+ 1 arg))
      (subn-prim () (- arg 1))
      (negBoolean-prim () (if (equal? arg "true") "false" "true"))
      (list-prim () (cases lista arg (empty-list()"false")
                           (non-empty-list (vals) "true")(else "is not a list")))
      (valuesD-prim()(non-empty-vec(dict-val arg)))
      (keysDict-prim() (non-empty-list (dict-keys arg)))
      (makeL-prim () (cond
                          ((list? arg) (non-empty-list arg))
                          ((lista? arg)
                           (cases lista arg
                           (empty-list ()empty-list)
                           (non-empty-list (l) (non-empty-list (list l))))
                           )
                          ((or (number? arg) (string? arg) (symbol? arg))
                           (non-empty-list (list arg)))
                          (else "not valid argument")
                          ))
      ( makeV-prim()(if (lista? arg)
                           (non-empty-vec (list->vector (extract-list-valores arg)))
                           (if (= (length arg) 2)
                               (non-empty-vec (make-vector (car arg) (cadr arg)))
                               (non-empty-vec (list->vector arg))
                               )))
      (makeD-prim ()(cond
                          ((not(equal? (length arg) 2)) "Not valid aplicattion for make-dict")
                          ((and(null? (car arg)) (null? (cadr arg)))
                           (make-dict (list) (list)))
                          ((and (not(null? (car arg)))
                                (not(null? (cadr arg))))
                           (make-dict (car arg) (cadr arg))
                           )))
            (makeG-prim()
                       (if (> (length arg) 2)
                           (eopl:error "the expected number of arguments does not ~%
                                        match the given number~%
                                        expectend: 2~% given: ~s" (length arg))
                           (if (valid-graph-data? (car arg)(cadr arg))
                               (if (and (lista? (car arg)) (lista? (cadr arg)))
                                   (make-graph (remove-dups (extract-list-valores (car arg)))
                                                 (remove-dups (extract-list-valores (cadr arg))))
                               
                                   (make-graph (remove-dups (get-vertices (car arg)))
                                                 (remove-dups (get-edges (cadr arg)))))
                               "Not valid aplication for make-graph"
                               )))
      (edges-prim () (if (grafos?  arg)
                              (cases grafos arg
                                (graph-exp (vs es) (non-empty-list (get-edges es))))
                                                    
                              (if (list?  arg)
                                  (caddr arg)
                                  "Is not a graph"
                                  )))
      (vertices-prim () (if (grafos?  arg)
                              (cases grafos arg
                                (graph-exp (vs es) (non-empty-list (get-vertices vs))))
                              (if (list?  arg)
                                  (cadr arg)
                                  "Is not a graph"
                                  )))
      (head-prim ()
                      (if (list? arg)
                          (car arg)
                          (car (extract-list-valores arg))
                          ))
      (tail-prim ()(if (lista? arg)
                          (if (= (length (extract-list-valores arg)) 1)
                              (empty-list)
                              (non-empty-list (cdr (extract-list-valores arg)))
                              )         
                          (if (list? arg)
                              (if (= (length arg) 1)
                                  (list)
                                  (cdr arg)
                                  )
                              "Argument not is a list"
                              )))
      (empty-prim () (cases lista arg (empty-list() "true")
                            (non-empty-list (vals) "false")(empty-list "true")))
      (else "faltan casos unario")
      )))




;-----------------------------------------------------------------------------------------------
;----------------------------------TIPOS--------------------------------------------------------
;-----------------------------------------------------------------------------------------------


;*********************************DEFINITION******************************************************
;*************************************************************************************************
(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?))
  (structure-type
   (name symbol?)
   (types (list-of symbol?)))
  )

(define int-type
  (atomic-type 'int))

(define bool-type
  (atomic-type 'bool))

(define float-type
  (atomic-type 'float))

(define string-type
  (atomic-type 'string))

(define edge-type
  (structure-type 'edge '(string)))

(define edges-type
  (structure-type 'edges '(string)))

(define vertices-type
  (structure-type 'vertices '(string)))

(define graph-type
  (structure-type 'graph '(string string)))

(define empty-type
  (atomic-type 'empty))

(define empty-list-type
  (structure-type 'list '(empty)))

(define int-list-type
  (structure-type 'list '(int)))

(define float-list-type
  (structure-type 'list '(float)))

(define bool-list-type
  (structure-type 'list '(bool)))

(define string-list-type
  (structure-type 'list '(string)))

(define int-vect-type
  (structure-type 'vect '(int)))

(define float-vect-type
  (structure-type 'vect '(float)))

(define bool-vect-type
  (structure-type 'vect '(bool)))

(define string-vect-type
  (structure-type 'vect '(string)))

(define int-dict-type
  (structure-type 'dict '(string int)))

(define float-dict-type
  (structure-type 'dict '(string float)))

(define bool-dict-type
  (structure-type 'dict '(string bool)))

(define string-dict-type
  (structure-type 'dict '(string string)))

(define generic-dict-type
  (structure-type 'dict '(string generic)))

(define generic-structure-type
  (structure-type 'struct '(generic)))

;--------------------------Variables para los  tipos de expreciones que----------------
;--------------------------tienen varios tipos-----------------------------------------
(define proc-types-list
  (list
   (proc-type (list generic-structure-type) generic-structure-type)
   (proc-type (list int-list-type) bool-type)
   (proc-type (list float-list-type) bool-type)
   (proc-type (list bool-list-type) bool-type)
   (proc-type (list string-list-type) bool-type))
  )

(define basic-aritmetic-types
  (list
   (proc-type (list int-type int-type) int-type)
   (proc-type (list int-type float-type) float-type)
   (proc-type (list float-type int-type) float-type)
   (proc-type (list float-type float-type) float-type))
  )

(define basic-logical-types
  (list
   (proc-type (list string-type string-type) bool-type)
   (proc-type (list bool-type bool-type) bool-type)
   (proc-type (list int-type int-type) bool-type)
   (proc-type (list int-type float-type) bool-type)
   (proc-type (list float-type int-type) bool-type)
   (proc-type (list float-type float-type) bool-type))
  )


;Funcion que convierte los type-exp en types
(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (String-type-exp()string-type)
      (float-type-exp ()float-type)
      (void-type() (atomic-type 'void))
      (edge-type-exp () edge-type)
      (edges-type-exp () edges-type)
      (vertices-type-exp () vertices-type)
      (graph-type-exp () graph-type)
      (generic-list-type() generic-structure-type)
      (list-int-type () int-list-type)
      (list-float-type () float-list-type)
      (list-bool-type () bool-list-type)
      (list-string-type () string-list-type)
      (vector-int-type () int-vect-type)
      (vector-float-type () float-vect-type)
      (vector-bool-type () bool-vect-type)
      (vector-string-type () string-vect-type)
      (dict-int-type() int-dict-type)
      (dict-float-type() float-dict-type)
      (dict-bool-type() bool-dict-type)
      (dict-string-type() string-dict-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp)))
      (else "faltan casos de tipos")
      )
    )
  )

;Funcion que mapea expand-type-expression a una lista de tipos
(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))


;********************************CHECKER-OF-PROGRAM-TYPE************************************************
;type-of-program: <programa> -> type
;función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-ent-exp (number)
                   int-type)
      (lit-float-exp (number)
                     float-type)
      (text-exp(text)
               string-type)
      
      (true-exp ()
                bool-type)
      (false-exp ()
                 bool-type)

      (empty-list-exp() empty-list-type)
      
      (var-exp (id)
               (apply-tenv tenv id)
               )

      (edge-g-exp (v1 v2) edge-type)
      
      (edges-g-exp (edges) edges-type)
      
      (vertices-g-exp (vers) vertices-type)
      
      (graph-g-exp (vers edges) (if (and (check-equal-type! (type-of-expression vers)
                                                             vertices-type vers)
                                         (check-equal-type! (type-of-expression edges)
                                                            edges-type edges))
                                    (graph-type)
                                    (eopl:error "Bab values types por endges and vertices")
                                    )
                   )

      (primapp-un-exp(prim rand)
                     (let* ((rand-type (if (> (length rand) 1)
                                           (types-of-expressions rand tenv)
                                           (type-of-expression (car rand) tenv)))
                            (cases (type-of-un-primitive prim))
                            (primitive  (cond((and (list? rand-type) (equal? (length rand-type) 2)
                                                   (or (equal? (unary-exeption-prim? prim) "vector-prim")
                                                       (equal? (unary-exeption-prim? prim) "dict-prim")))
                                              (match-binary-prim-type (car rand-type) (cadr rand-type) cases))
                                             
                                             ((and (list? rand-type) (> (length rand-type) 2))
                                              (match-unary-prim-type (type-of-structure rand-type)cases))

                                             (else
                                              (match-unary-prim-type rand-type cases))
                                             ))
                            (args-types-length (extrac-args-types-proc primitive))
                            )
                      
                       (cond((equal? (length args-types-length) 2)
                             (type-of-application
                              primitive
                              (list (car rand-type) (cadr rand-type))
                              prim (list (car rand) (cadr rand)) exp)
                             )

                            ((and (list? rand-type)(equal? (length args-types-length) 1))
                             (type-of-application
                              primitive
                              (list (car rand-type))
                              prim (list (car rand)) exp))

                            (else
                             (type-of-application
                              primitive
                              (list  rand-type)
                              prim (list (car rand)) exp))
                            )                
                       )                    
                     )

      (primapp-bin-exp (rand1 prim rand2)
                       (let*((rand-type1 (type-of-expression rand1 tenv))
                             (rand-type2 (if (equal? (length rand2) 2)
                                             (list (type-of-expression (car rand2) tenv)
                                                   (type-of-expression (cadr rand2) tenv))
                                             (type-of-expression (car rand2) tenv)))
                             (cases (type-of-bin-primitive prim))
                             (primitiva (if (and (list? rand-type2) (equal? (length rand-type2) 2))
                                            (match-binary-prim-exeption-type
                                             rand-type1 (car rand-type2) (cadr rand-type2) cases)
                                            (match-binary-prim-type
                                             rand-type1 rand-type2 cases))))

                         (if (equal? (length rand2) 2)
                             (type-of-application
                              primitiva
                              (types-of-expressions (list rand1 (car rand2) (cadr rand2)) tenv)
                              prim (list rand1 (car rand2) (cadr rand2)) exp)
                             
                             (type-of-application
                              primitiva
                              (types-of-expressions (list rand1 (car rand2)) tenv)
                              prim (list rand1 rand2) exp)
                             )
                         ) 
                       )
      
      (list-exp (args)
                (let*((types-list (types-of-expressions args tenv))
                      (type-of-struct (type-of-structure types-list)))
                  (if (cases type type-of-struct
                        (atomic-type(n)#t)
                        (else #f))
                      (structure-type 'list (list (atomic-type-value type-of-struct)))
                      type-of-struct
                      )
                  
                  )               
                )

      (vect-exp(args)
               (let*((types-list (types-of-expressions args tenv))
                     (type-of-struct (type-of-structure types-list)))
                 (structure-type 'vect (list (atomic-type-value type-of-struct)))
                 )
               )

      (dict-exp(keys values)
               (let*(
                     (keys-types (types-of-expressions keys tenv))
                     (values-types (types-of-expressions values tenv))
                     (keys-type-checked-converted
                      (atomic-type-value (type-of-structure keys-types)))
                     (type-struct (type-of-structure values-types))
                     (values-type-checked-converted
                      (if (cases type type-struct
                          (atomic-type(n)#t)
                          (else #f))
                          (atomic-type-value (type-of-structure values-types))
                          type-struct
                          )
                      )
                     )
                 (if (cases type type-struct
                        (atomic-type(n)#t)
                        (else #f))
                     (structure-type 'dict (list keys-type-checked-converted
                                             values-type-checked-converted))
                     generic-dict-type
                     )
                 
                 )
               )
      
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))

      
      (localVar-exp (ids-types exps body)
                    (let*(
                          (ids (map eval-var (map id-var exps)))
                          (exps (map exp-var exps))
                          (defined-types (eval-defined-types ids-types))                       
                          (tenv-extend (extended-tenv-record ids defined-types tenv))
                          (exps-types (types-of-expressions exps tenv-extend))
                          )
                      (begin
                        (for-each
                         check-equal-type!
                         defined-types exps-types ids)
                        (type-of-expression body tenv-extend))
                     
                      )
                    )
      
      (LOCALS-exp (ids-types exps first-instruct instructs)
                  (let*(
                        (ids (map eval-var (map id-var exps)))
                        (exps (map exp-var exps))
                        (defined-types (eval-defined-types ids-types))                       
                        (tenv-extend (extended-tenv-record ids defined-types tenv))
                        (exps-types (types-of-expressions exps tenv-extend))
                        )
                    (begin
                      (for-each
                       check-equal-type!
                       defined-types exps-types exps)
                      (return-last-type first-instruct instructs tenv-extend))
                    )
                  )


      (global-exp (ids-types exps type-body main-body)
                  (let*(
                        (ids (map eval-var (map id-var exps)))
                        (exps (map exp-var exps))
                        (defined-types (eval-defined-types ids-types))                       
                        (tenv-extend (extended-tenv-record ids defined-types tenv))
                        (exps-types (types-of-expressions exps tenv-extend))
                        (main-type (type-of-expression main-body tenv-extend))
                        (defined-main-type (expand-type-expression type-body))
                        )                   
                    (begin
                      (for-each
                       check-equal-type!
                       defined-types exps-types exps)
                      (check-equal-type! defined-main-type main-type main-body)
                       main-type
                      )
                    )
                  )
      
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))

      
      (app-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
                 
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv))
      
      (set-exp (id new-exp)
               (let((id (eval-var id)))
                 (if (check-equal-type! (apply-tenv tenv id)
                                        (type-of-expression new-exp tenv)
                                        new-exp)
                     (apply-tenv tenv id)
                     "Types don't macth in set-exp"
                     )
                 )
               )

      (BLOCK-exp (first-instruct instructs)
                 (return-last-type first-instruct instructs tenv)                  
                 )
      
      (while-exp (test-exp first-exp exps)
                 (let ((test-type (type-of-expression test-exp tenv)))
                   (begin (check-equal-type! test-type bool-type test-exp)
                          (return-last-type first-exp exps tenv)
                          )
                   )       
                 )
      
      (for-exp (id beginning stop-cond sumator first-exp exps)
               (if (and (correct-beginning? id beginning)
                        (correct-stop-cond? stop-cond)
                        (correct-sumator? sumator))
                   (return-last-type first-exp exps tenv)
                   (eopl:error "bab value for inicializators in for-exp")
                   )
               )

      (switch-exp (option coincidences coincidence-exps default-exp)
                  (if (valid-option? option)
                      (let*(
                            (op-type (type-of-expression option tenv))
                            (cases (types-of-expressions coincidences tenv))
                            (cases-type (type-of-structure cases))
                            (coincidence-exp(coincidence-case
                                             (eval-expression option (empty-env))
                                             (eval-rands coincidences (empty-env))
                                             coincidence-exps)))
      
                        (if (check-equal-type! op-type cases-type option)
                            (if (string? coincidence-exp)
                                (type-of-expression default-exp tenv)
                                (type-of-expression coincidence-exp tenv)    
                                )
                            "type of option and cases didn't match"
                            )
                        )
                      "Is not a valid option for switch"
                      )
                  )
      (print-exp (exp)
                 (atomic-type 'void))
      
      (else "NO HAY TIPO")
      )
    )
  )

;***************************FUNTIONS-FOR-TYPES****************************************
;*************************************************************************************

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type! 
                    "Types didn’t match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp
                    )
        #t)))

;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type))))
      (structure-type (name args)
                      (if (equal? name 'dict)
                          (string-append (symbol->string name) "<"(symbol->string (car args))
                                         "," (symbol->string (cadr args)) ">")
                          (string-append (symbol->string name) "<"(symbol->string (car args))">")
                          )
                      )
      (else "falta external form")
      )
    )
  )

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;type-of-proc-exp: (list-of <type-exp>) (list-of <symbol>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión de creación de procedimiento
(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)
        ))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
;función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))
    )
  )

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-un-primitive
  (lambda (prim)
    (cases unary-primitive prim
      (primitive-lenght ()
                        (proc-type (list string-type) int-type))
      
      (primitive-add1 ()
                      (list
                       (proc-type (list int-type) int-type)
                       (proc-type (list float-type) float-type)
                       ))
      
      (primitive-sub1 ()
                      (list
                       (proc-type (list int-type) int-type)
                       (proc-type (list float-type) float-type)
                       ))
      
      (primitive-neg-boolean()
                            (proc-type (list bool-type) bool-type))
      
      (primitive-empty() proc-types-list )

      (make-g-primitive() (proc-type (list vertices-type edges-type) graph-type))

      (primitiva-edges() (proc-type (list graph-type) generic-structure-type))

      (primitiva-vertices() (proc-type (list graph-type) vertices-type))

      (make-l-primitive() (list (proc-type (list generic-structure-type) generic-structure-type)
                                (proc-type (list empty-list-type) empty-list-type)
                                (proc-type (list int-type) int-list-type)
                                (proc-type (list float-type) float-list-type)
                                (proc-type (list bool-type) bool-list-type)
                                (proc-type (list string-type) string-list-type)))

      (make-v-primitive() (list (proc-type (list int-type int-type) int-vect-type)
                                (proc-type (list int-type float-type) float-vect-type)
                                (proc-type (list int-type bool-type) bool-vect-type)
                                (proc-type (list int-type string-type) string-vect-type)
                                (proc-type (list int-type) int-vect-type)
                                (proc-type (list float-type) float-vect-type)
                                (proc-type (list bool-type) bool-vect-type)
                                (proc-type (list string-type) string-vect-type)))

      (make-d-primitive() (list (proc-type (list string-list-type generic-structure-type) generic-dict-type)
                                (proc-type (list string-list-type int-vect-type) int-dict-type)
                                (proc-type (list string-list-type float-vect-type) float-dict-type)
                                (proc-type (list string-list-type bool-vect-type) bool-dict-type)
                                (proc-type (list string-list-type string-vect-type) string-dict-type)))
      
      (primitive-list() proc-types-list)
      
      (primitive-head() (list
                         (proc-type (list int-list-type) int-type)
                         (proc-type (list float-list-type) float-type)
                         (proc-type (list bool-list-type) bool-type)
                         (proc-type (list string-list-type) string-type)))
      
      (primitive-tail() (list
                         (proc-type (list int-list-type) int-list-type)
                         (proc-type (list float-list-type) float-list-type)
                         (proc-type (list bool-list-type) bool-list-type)
                         (proc-type (list string-list-type) string-list-type)))
      
      (primitive-keys-dict()(list
                             (proc-type (list (structure-type 'dict '(string int))) string-list-type)
                             (proc-type (list (structure-type 'dict '(string float))) string-list-type)
                             (proc-type (list (structure-type 'dict '(string bool))) string-list-type)
                             (proc-type (list (structure-type 'dict '(string string))) string-list-type)))
      
      (primitive-values-dict()(list
                               (proc-type (list (structure-type 'dict '(string int))) int-vect-type)
                               (proc-type (list (structure-type 'dict '(string float))) float-vect-type)
                               (proc-type (list (structure-type 'dict '(string bool))) bool-vect-type)
                               (proc-type (list (structure-type 'dict '(string string))) string-vect-type)))

      (else "faltan tipos unarios")
      )
    )
  )

;type-of-primitive: <primitive binaria> -> <type>
;función auxiliar para determinar el tipo de una primitiva binaria
(define type-of-bin-primitive
  (lambda (prim)
    (cases primitiva-binaria prim
      (primitiva-suma () basic-aritmetic-types)
      
      (primitiva-resta () basic-aritmetic-types)
      
      (primitiva-div () basic-aritmetic-types)
      
      (primitiva-multi () basic-aritmetic-types)

      (primitiva-mod () basic-aritmetic-types)
      
      (primitiva-concat()
                       (proc-type (list string-type string-type) string-type))
      
      (primitiva-mayor() basic-logical-types)
      
      (primitiva-menor() basic-logical-types)
      
      (primitiva-mayor-igual() basic-logical-types)
      
      (primitiva-menor-igual() basic-logical-types)
      
      (primitiva-diferente() basic-logical-types)
      
      (primitiva-comparador-igual() basic-logical-types)

      (primitiva-add-edge () graph-type)
      
      (primitiva-vecinos-en() string-list-type)

      (primitiva-vecinos-sal() string-list-type)
      
      (primitiva-append ()
                        (list
                         (proc-type (list string-list-type generic-structure-type) generic-structure-type)
                         (proc-type (list vertices-type vertices-type) generic-structure-type)
                         (proc-type (list edges-type edges-type) edges-type)
                         (proc-type (list empty-list-type int-list-type) int-list-type)
                         (proc-type (list empty-list-type float-list-type) float-list-type)
                         (proc-type (list empty-list-type bool-list-type) bool-list-type)
                         (proc-type (list empty-list-type string-list-type) string-list-type)
                         (proc-type (list int-list-type int-list-type) int-list-type)
                         (proc-type (list float-list-type float-list-type) float-list-type)
                         (proc-type (list bool-list-type bool-list-type) bool-list-type)
                         (proc-type (list string-list-type  string-list-type) string-list-type)))
      
      (primitiva-ref-vector ()
                            (list (proc-type (list int-vect-type int-type) int-type)
                                  (proc-type (list float-vect-type int-type) float-type)
                                  (proc-type (list bool-vect-type int-type) bool-type)
                                  (proc-type (list string-vect-type int-type) string-type))
                            )
      
      (primitiva-set-vector()
                           (list (proc-type (list int-vect-type int-type int-type) bool-type)
                                 (proc-type (list float-vect-type int-type float-type) bool-type)
                                 (proc-type (list bool-vect-type int-type bool-type) bool-type)
                                 (proc-type (list string-vect-type int-type string-type) bool-type)))
      
      (primitiva-append-vector ()
                               (list (proc-type (list int-vect-type int-type) int-vect-type)
                                     (proc-type (list float-vect-type float-type) float-vect-type)
                                     (proc-type (list bool-vect-type bool-type) bool-vect-type)
                                     (proc-type (list string-vect-type string-type) string-vect-type)))
      
      (primitiva-delete-val-pos ()
                                (list (proc-type (list int-vect-type int-type) int-vect-type)
                                      (proc-type (list float-vect-type int-type) float-vect-type)
                                      (proc-type (list bool-vect-type int-type) bool-vect-type)
                                      (proc-type (list string-vect-type int-type) string-vect-type)))
      (primitiva-ref-dict ()
                          (list
                           (proc-type (list (structure-type 'dict '(string int)) string-type) int-type)
                           (proc-type (list (structure-type 'dict '(string float)) string-type) float-type)
                           (proc-type (list (structure-type 'dict '(string bool)) string-type) bool-type)
                           (proc-type (list (structure-type 'dict '(string string)) string-type) string-type)))
      
      (primitiva-set-dict ()
                          (list
                       
                           (proc-type (list (structure-type 'dict '(string int)) string-type int-type) bool-type)
                           (proc-type (list (structure-type 'dict '(string float)) string-type float-type) bool-type)
                           (proc-type (list (structure-type 'dict '(string bool)) string-type bool-type) bool-type)
                           (proc-type (list (structure-type 'dict '(string string)) string-type string-type) bool-type)))

      (else "falta tipo binario")
      )
    )
  )


;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
;función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

;type-of-primitive: (list-of <symbol>) (list-of <expression>) <expression> <tenv> -> <type>
;función auxiliar para determinar el tipo de una expresión let
(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))

;type-of-primitive: (list-of <type-exp>) (list-of <symbol>) (list-of (list-of <type-exp>)) (list-of (list-of <symbol>)) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión letrec
(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-type-expressions texps))
                           texpss))
          (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))

;type-of-estructure <list> -> type
;funcion que retorna el atomic type de una estructura, si todos sus elementos son
;del mismo tipo
(define type-of-structure
  (lambda (lst)
    (cond
      ((null? lst) (atomic-type 'list<empty>)) 

      ((null? (cdr lst))
       (car lst))
   
      ((equal? (car lst) (type-of-structure (cdr lst)))
       (car lst))

      (else generic-structure-type))))

;extractor para atomic-type
(define atomic-type-value
  (lambda (t)
    (cases type t
      (atomic-type (name) name)
      (else "not atomic-type"))
    )
  )

#|
Funcion que convierte el identificador de tipo de una lista de tipos
;a una expression de tipo type

NOTA: identificador de tipo se refiere a cuando escribimos
int @a = 2, int es el identificador de tipo
|#
(define eval-defined-types
  (lambda (types)
    (map expand-type-expression types)
    )
  )
;return-last-type <expression> <list-of expressions> <tenvironment>-> type 
;Funcion que ejecuta un serie de instrucciones y retorna el tipo de la ultima
;que ejecuta.
(define return-last-type
  (lambda (first-instruct instructs tenv)
    (let loop ((first-type (type-of-expression first-instruct tenv))
               (instructs instructs))
      (if (null? instructs) 
          first-type
          (loop (type-of-expression (car instructs) tenv)
                (cdr instructs)))
      )
    )
  )

;match-unary-prim-type <list-of type> <list-of type> -> type
;Funcion recibe un lista 2 listas de tipos, extrae el typo de la primera list y retorna
;con cual de los tipos de la segunda list coincide.
;NOTA: usado para las primitivas unarias
(define match-unary-prim-type
  (lambda (arg-type cases)
    (let((arg-type (if (list? arg-type)
                       (type-of-structure arg-type)
                       arg-type
                       )))
      (if (list? cases)
          (cond
            ((null? cases) (eopl:error 'match-unary-prim-type
                                       "~%Type ~s not found in ~s" arg-type cases))
      
            ((equal? arg-type (extrac-arg-type-proc (car cases)))
             (car cases))
      
            (else
             (match-unary-prim-type arg-type (cdr cases)))
            )
          cases
          )
      
      )
    )
  )


;match-binary-prim-type <type> <type> <list-of types> -> type
;Funcion que recibe 2 types y una list de proc-types y verifica si los 2 types
;coinciden con alguno de los proc-types de la list de proc-types

;NOTA:la funcion se uso principalmente para las primitivas binarias, pero por cuestiosnes de
;incluir dentro de las primitivas unarias las primitivas makes(vector,list,dict y graph)
;se uso tambien para verificar los 4 casos de las primitivas make mencionadas

(define match-binary-prim-type
  (lambda (arg-type1 arg-type2 cases)
    (if (list? cases)
        (cond
          ((null? cases)(eopl:error 'match-binary-prim-type
                                    "~%Types ~s, ~s not found in ~s"
                                    arg-type1 arg-type2 cases))
          ((and (equal? arg-type1 (car (extrac-args-types-proc (car cases))))
                (equal? arg-type2 (cadr (extrac-args-types-proc (car cases))))
                )(car cases))
          (else
           (match-binary-prim-type arg-type1 arg-type2 (cdr cases)))
          )
        cases
        )
    )
  )

;match-binary-prim-exption-type <type> <type> <type> <list-of types>
;Funcion que retorna el caso de coincidencia del tipo de las primitivas
;set-vect y set-dict

;NOTA:Esta funcion es igual a match-binary-prim-type solo que recibe un type mas,
;se realizo este ajuste, para las primitivas binarias set-vector y set-dict
;ya que estas se trabajaron asi: app-b(@miVec set-vector (1 , 2.4)) en este caso
;se usaba una lista para pasarle la posicion y el valor, por lo tanto en algunos
;casos como el de un vector de flotantes quedaba un lista con un entero(posicion) y un
;flotante(nuevo valor), lo cual no era una lista valida para la definicion del proyecto,
;la cual no aceptaba listas de valores mixtos

(define match-binary-prim-exeption-type
  (lambda (arg-type1 arg-type2 arg-type3 cases)
    (if (list? cases)
        (cond
          ((null? cases)(eopl:error 'match-binary-prim-exeption-type
                                    "~%Types ~s, ~s, ~s not found in ~s"
                                    arg-type1 arg-type2 arg-type3 cases))
          ((and (equal? arg-type1 (car (extrac-args-types-proc (car cases))))
                (equal? arg-type2 (cadr (extrac-args-types-proc (car cases))))
                (equal? arg-type3 (caddr (extrac-args-types-proc (car cases))))
                )(car cases))
          (else
           (match-binary-prim-exeption-type arg-type1 arg-type2 arg-type3 (cdr cases)))
          )
        cases
        )
    )
  )

;extrac-arg-type-proc <proc-type> -> type
;Funcion que extrae el primer typo de un proc-type
(define extrac-arg-type-proc
  (lambda (proc)
    (cases type proc
      (proc-type (args-types result-type)
                 (car args-types))
      (else (eopl:error 'extrac-args-type-proc
                        "~s No is a proc-type"
                        proc)))
    )
  )

;extrac-args-types-proc <proc-type> -> <list-of type>
;Funcion que extrae la list de types de un proc-type
(define extrac-args-types-proc
  (lambda (proc)
    (cases type proc
      (proc-type (args-types result-type)
                 args-types)
      (else (eopl:error 'extrac-args-type-proc
                        "~s No is a proc-type"
                        proc)))
    )
  )

;unary-exeption-prim?  <unary-primitive> -> string
;Funcion que retorna un string con el nombre de la primitiva unaria que tiene exepciones
;en su construccion, vector-prim y dict-prim, por el mismo motivo de las listas mistax no aceptadas
(define unary-exeption-prim?
  (lambda (un-prim)
    (cases unary-primitive un-prim
      (make-v-primitive() "vector-prim")
      (make-d-primitive() "dict-prim")
      (else "Is no a unary-exeption-prim")
      )
    )
  )
;corect-stop-cond? <type>-> bool
;Funcion que determina si la expression de la condicion de parada de un for
;es correcta, es correcta si es una expression que retorne un booleano
(define correct-stop-cond?
  (lambda (type-stop-cond)
    (cases expression type-stop-cond
      (primapp-bin-exp(e1 prim e2)
                      (cases primitiva-binaria prim
                        (primitiva-mayor()#t)
                        (primitiva-menor()#t)
                        (primitiva-mayor-igual()#t)
                        (primitiva-menor-igual()#t)
                        (primitiva-comparador-igual()#t)
                        (else #f)))
      (else #f))
    )
  )

;correct-beginning? <symbol> <expression> -> string
;Funcion que determina si la expression de inicio de un for es correcta
;es correcta si el symbol es un id y la expression es o un entero o flotante
(define correct-beginning?
  (lambda (id beginning)
    (if (and
         (is-id? id)
         (cases expression beginning
           (lit-ent-exp(n) #t)
           (lit-float-exp(n)#t)
           (else #f))
         )#t #f)
    )
  )

;correct-sumator? <expression> -> bool
;Funcion que determina si la expression de aumento del for es correcta
;es correcta si es una exprecion de seteo y la aplicacion del seteo es una
;exprecion de incremento(suma o multiplicacion) o una exprecion de decremento
;(resta)
(define correct-sumator?
  (lambda(sumator)
    (cases expression sumator
      (set-exp (id exp) (cases expression exp
                          (primapp-bin-exp (e1 prim e2)
                                           (cases primitiva-binaria prim
                                             (primitiva-suma()#t)
                                             (primitiva-resta()#t)
                                             (primitiva-multi()#t)
                                             (else #f)))
                          (else "bad value for sumator(prim)")
                          ))
      (else "bad value for sumator(set)")
      )
    )
  )

;is-id? <id> -> bool
;funcion que determina si una expression es una id
(define is-id?
  (lambda (id)
    (let* ((cadena (symbol->string id))
           (indice (caracter-index cadena)))
      (if (= indice 0) #t #f)
      )
    )
  )

;valid-option? <expression> -> bool
;Funcion que determina si la opcion a comparar en un switch es correcta
;es correcta si es ò un entero, un flotante, string
(define valid-option?
  (lambda (op)
    (cases expression op
      (lit-ent-exp(n) #t)
      (lit-float-exp(n)#t)
      (text-exp(t)#t)
      (else #f))
  )
)

;coincidence-case <scheme-value> <list-of scheme-value> <list-of expression> -> expression
;Funcion que recibe un valor y una lista de casos y retorna la expression del caso de coincidencia del valor
(define coincidence-case
  (lambda (op cases exps)
    (cond
      ((null? cases) "default")
      ((equal? op (car cases)) (car exps))
      (else (coincidence-case op (cdr cases) (cdr exps)))
      )
  )
)
(interpretador)







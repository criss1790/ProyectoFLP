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
    (inmutable-identifier("conts" "@" letter (arbno (or letter digit "?"))) symbol)
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

    (expression ("BLOCK" "{" expression (arbno expression )"}")BLOCK-exp)
     (expression ("print" "(" expression ")" ) print-exp)
    (expression ("letrec" (arbno type-exp identifier
                                 "(" (separated-list type-exp identifier ",") ")"
                                 "=" expression) "in" expression)letrec-exp)
    (expression (un-primitive "(" (arbno expression) ")") prim-un-exp)
    (expression("fun" "("(separated-list type-exp identifier ",") ")" expression )proc-exp)
    (expression("call" expression "("(separated-list expression ",")")") app-exp)
    (expression( "apply" "(" expression primitive-bin (arbno expression) ")")prim-bin-exp)
    (expression("if" expression "{" expression "}" "else" "{" expression "}")if-exp
    (expression("dec" "(" (arbno type-exp assing) ")""{"expression "}")decl-exp)
    (expression ("set" var "=" expression)set-exp)
    (expression ("while" "(" expression ")" "do" "(" expression (arbno expression) ")")while-exp)
    (expression ( "for" "(" identifier "=" expression ";" expression ";" expression ")"
                        "{"expression (arbno expression)"}")for-exp)
    (expression ( "switch" "(" expression ")" "{" (arbno "case" expression ":" expression ) "default:" expression "}")
                switch-exp )

   
    
                
  
    ;soporte listas
    (expression("(" (separated-list expression ",") ")")list-exp)
    
    ;soporte vectores
    (expression ("["(separated-list expression ",")"]" ) vect-exp)

    ;soporte doccionarios
    (expression ("{"(separated-list  expression ":" expression ",") 
                    "}") dict-exp)

    ;soporte grafos
    (expression ( "edge" "(" identifier "," identifier ")") edge-exp)
    (expression ( "edges" "(" (arbno expression ) ")" ) edges-exp)
    (expression ( "vertices" "(" (separated-list identifier ",") ")") vertices-exp)
    (expression ( "graph" "(" expression "," expression ")") graph-exp)


    

    

   
    
    (expression (unary-primitive "(" (arbno expression) ")") primapp-un-exp)
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
    
    )))


;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))


;***********************************************************************************************************************
;***********************************************************************************************************************

(define apply-primitive-b
  (lambda (prim-b args)
    (cases primitiva-bin prim-b
      (suma-prim () (+ (car args) (cadr args)))
      (resta-prim () (- (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (multi-prim () (* (car args) (cadr args)))
      (mod-prim () (if (and (integer? (car args)) (integer? (cadr args)))
                            (modulo (car args) (cadr args))
                            (real-modulo (car args) (cadr args))
                            ))
      (concat-prim () (string-append (car args) (cadr args)))
      (mayor-prim () (valor-verdad? (comparar (car args) (cadr args) '>)))
      (menor-prim () (valor-verdad? (comparar (car args) (cadr args) '<)))
      (mayorIgual-prim () (valor-verdad? (comparar (car args) (cadr args) '>=)))
      (menorIgual-prim () (valor-verdad? (comparar (car args) (cadr args) '<=)))
      (dif-prim () (valor-verdad? (not (eqv? (car args) (cadr args)))))
      (comparador-prim () (valor-verdad? (equal? (car args) (cadr args))))
      (append-prim() (cond((and (vs-exp? (car args)) (vs-exp? (cadr args)))
                               (non-empty-list(append (extract-vertices (car args))
                                                      (extract-vertices (cadr args)))))
                              ((and (es-exp? (car args)) (es-exp? (cadr args)))
                               (non-empty-list(append (extract-edges (car args))
                                                      (extract-edges (cadr args))))
                               )
                              ((not(and (list? (car args)) (list? (cadr args))))
                              (non-empty-list (append (extract-values-list (car args))
                                                 (extract-values-list (cadr args)))))(else
                               (eopl:error "Valores no validos para appens ~s y ~s"
                                           (car args)
                                           (cadr args)))))
      (addEdge-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (add-edge (car args) (cases e-exp (cadr args)
                                                       (edge-exp (e1 e2)(list e1 e2))))
                                )
                         )
      (appendVector-prim()(non-empty-vec (vector-append (car args) (cadr args))))
      (deleteV-prim() (non-empty-vec(delete-val-pos (car args)(cadr args))))
      (refVector-prim()(if (vector? (car args))
                                 (vector-ref (car args) (cadr args))
                                 (vector-ref (extract-values-vec (car args))
                                             (cadr args))))
      (setVector-prim()(if (vector? (car args))
                                 (vector-set! (car args) (cadr args))
                                 (let((vec (extract-values-vec (car args)))
                                      (pos&val (extract-values-list (cadr args)))
                                      )
                                   (vector-set! vec (car pos&val) (cadr pos&val))
                                   "ok"
                                   )
                                 ))
      (refDict-prim() (ref-dict (car args) (cadr args)))
      (setDict-prim() (set-dict (car args) (cadr args)))
      (vecinosEn-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (non-empty-list
                                 (symbols->strings (vecinos-entrantes (car args) (cadr args))))
                                )
                           )
      
      (vecinosSa-prim() (if (> (length args)2)
                                (eopl:error "the expected number of arguments does not ~%
                                              match the given number~%
                                              expectend: 2~% given: ~s" (length args))
                                (non-empty-list
                                 (symbols->strings (vecinos-salientes (car args) (cadr args))))))
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
      (valuesD-prim()(non-empty-vec(values-dict arg)))
      (keysDict-prim() (non-empty-list (keys-dict arg)))
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
                          )
                        )
      ( makeV-prim()(if (lista? arg)
                           (non-empty-vec (list->vector (extract-values-list arg)))
                           (if (= (length arg) 2)
                               (non-empty-vec (make-vector (car arg) (cadr arg)))
                               (non-empty-vec (list->vector arg))
                               )                           
                           )
                       )
      (makeD-prim ()(cond
                          ((not(equal? (length arg) 2)) "Not valid aplicattion for make-dict")
                          ((and(null? (car arg)) (null? (cadr arg)))
                           (create-dict (list) (list)))
                          ((and (not(null? (car arg)))
                                (not(null? (cadr arg))))
                           (create-dict (car arg) (cadr arg))
                           )
                          )
                        )
            (makeG-prim()
                       (if (> (length arg) 2)
                           (eopl:error "the expected number of arguments does not ~%
                                        match the given number~%
                                        expectend: 2~% given: ~s" (length arg))
                           (if (valid-rands-for-graph? (car arg)(cadr arg))
                               (if (and (lista? (car arg)) (lista? (cadr arg)))
                                   (create-graph (delete-duplicates (extract-values-list (car arg)))
                                                 (delete-duplicates (extract-values-list (cadr arg))))
                               
                                   (create-graph (delete-duplicates (extract-vertices (car arg)))
                                                 (delete-duplicates (extract-edges (cadr arg))))
                                   )

                               "Not valid aplication for make-graph"
                               )
                           )
                       )
      (edges-prim () (if (g-exp?  arg)
                              (cases g-exp arg
                                (graph-exp (vs es) (non-empty-list (extract-edges es))))
                                                    
                              (if (list?  arg)
                                  (caddr arg)
                                  "Is not a graph"
                                  )))
      (vertices-prim () (if (g-exp?  arg)
                              (cases g-exp arg
                                (graph-exp (vs es) (non-empty-list (extract-vertices vs))))
                              (if (list?  arg)
                                  (cadr arg)
                                  "Is not a graph"
                                  )))
      (head-prim ()
                      (if (list? arg)
                          (car arg)
                          (car (extract-values-list arg))
                          ))
      (tail-prim ()(if (lista? arg)
                          (if (= (length (extract-values-list arg)) 1)
                              (empty-list)
                              (non-empty-list (cdr (extract-values-list arg)))
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
      (lit-ent-exp (datum) datum)
      (lit-float-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      ;(conts-exp (id) (apply-env env id))
      (text-exp (text) text)
      (true-exp () "true")
      (false-exp () "false")
      (empty-list-exp () (empty-list))

      (edge-g-exp (v1 v2) (edge-exp v1 v2))
      (edges-g-exp (edges) (edges-exp (eval-rands edges env)))
      (vertices-g-exp (vers) (vertices-exp vers))
      (graph-g-exp (vers edges) (graph-exp vers edges))
      
      (primapp-un-exp (prim rand)
                      (let ((arg (if (> (length rand) 1)
                                     (eval-rands rand env)
                                     (eval-expression (car rand) env)
                                     )))
                        (apply-unary-primitive prim arg)                       
                        ))
      
      (primapp-bin-exp (exp1 prim exp2)
                       (let ((arg1 (eval-expression exp1 env))
                             (arg2 (if (equal? (length exp2) 2)
                                           (non-empty-list (list (eval-expression (car exp2) env)
                                                             (eval-expression (cadr exp2) env)))
                                       (if (vecino-prim? prim)
                                           (extract-value (car exp2))
                                           (eval-expression (car exp2) env))))
                             )
                         (apply-primitive-bin prim (list arg1 arg2))
                         )
                       )

      (if-exp(test-exp true-exp false-exp)
             (if(equal? (eval-expression test-exp env) "true")
                (eval-expression true-exp env)
                (eval-expression false-exp env)))

      (localVar-exp(types exps body)
                   (let ((args (eval-rands (map exp-var exps) env))
                         (ids (map eval-var (map id-var exps))))
                     (eval-expression body
                                      (extend-env ids args env))
                     ))

      (proc-exp (ids-types ids body)
                (closure ids body env))

      (app-exp (rator rands)
               (let (
                     (proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     proc
                     )
                 )
               )
      
      (letrec-exp (type-proc proc-names arg-types args proc-bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names args proc-bodies env))
                  )
      
      (list-exp (args)
                (if (null? (eval-rands args env))
                    (empty-list)
                    (non-empty-list (eval-rands args env))
                    )
                )

      (vect-exp (args)
                (if (null? (eval-rands args env))
                    (empty-vec)
                    (non-empty-vec (list->vector (eval-rands args env)))
                    )
                )

      (dict-exp (keys values)                
                (non-empty-dict (eval-rands keys env)
                                (list->vector (eval-rands values env)))
                )
      
      (set-exp (id rhs-exp)
               (let((id (eval-var id)))
                 (if (number? (rib-find-position id (symbols-env env)))
                     (begin 
                       (setref!
                        (apply-env-ref env id)
                        (eval-expression rhs-exp env))
                       1)
                     (eopl:error  "Attempt to change the value of an immutable variable")
                     )
                 )              
               )

      (BLOCK-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) env)
                             (cdr exps)))))

      (LOCALS-exp(ids-types exps first-instruct instructions)
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
                               (cdr exps))))
                   
                   )
                 )
      
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
                       "fin del bucle"
                       )
                   )
                 )
      
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
                         "fin del ciclo"
                         )
                     )
                   )
                 )
               )

      (switch-exp (option coincidences coincidences-exps default-exp)
                  (let((coincidence-exp (coincidence-case
                                         (eval-expression option env)
                                         (eval-rands coincidences env)
                                         coincidences-exps)))
                    (if (string? coincidence-exp)
                           (eval-expression default-exp env)
                           (eval-expression coincidence-exp env))
                    )
                  )
      (print-exp (exp)
                 (begin
                 (display (eval-expression exp env))
                 (newline)
                 "fin del print"
                  ))
      
      (else "faltan casos eval-expression")
      )    
    ))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;funcion que aplica eval-expression a un solo elemento
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-test-prim () (zero? (car args)))
      (menor-prim () (< (car args) (cadr args)))
      (mayor-prim () (> (car args) (cadr args)))
     )
    )
  )

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*********************************************   Definición tipos     **************************************************
;***********************************************************************************************************************

(define-datatype type type?
  (atomic-type (name symbol?))
  (proc-type
    (arg-types (list-of type?))
    (result-type type?))
  (tvar-type
    (serial-number integer?)
    (container vector?)))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*************************************************   Type Checker     **************************************************
;***********************************************************************************************************************

;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number)
               int-type)
      (true-exp ()
                bool-type)
      (false-exp ()
                 bool-type)
      (var-exp (id)
               (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
                   (type-of-application
                    (type-of-primitive prim)
                    (types-of-expressions rands tenv)
                    prim rands exp))
      (app-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (let-exp (ids rands body)
               (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv)))))

;El unificador

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!         ;;; NUEVO      
  (lambda (t1 t2 exp)
    (cond
      ((eqv? t1 t2)  )  
      ((tvar-type? t1) (check-tvar-equal-type! t1 t2 exp))
      ((tvar-type? t2) (check-tvar-equal-type! t2 t1 exp))
      ((and (atomic-type? t1) (atomic-type? t2))
       (if (not
             (eqv?
               (atomic-type->name t1)
               (atomic-type->name t2)))
         (raise-type-error t1 t2 exp)
         #t))
      ((and (proc-type? t1) (proc-type? t2))
       (let ((arg-types1 (proc-type->arg-types t1))
             (arg-types2 (proc-type->arg-types t2))
             (result-type1 (proc-type->result-type t1))
             (result-type2 (proc-type->result-type t2)))
         (if (not
               (= (length arg-types1) (length arg-types2)))
           (raise-wrong-number-of-arguments t1 t2 exp)
           (begin
             (for-each
               (lambda (t1 t2)
                 (check-equal-type! t1 t2 exp))
               arg-types1 arg-types2)
             (check-equal-type!
               result-type1 result-type2 exp)))))
      (else (raise-type-error t1 t2 exp)))))

;check-tvar-equal-type!
; revisa si una variable de tipo es igual o no contiene un tipo de dado y asigna dicho tipo al contenedor de la variable de tipo
(define check-tvar-equal-type!
  (lambda (tvar ty exp)
    (if (tvar-non-empty? tvar)
      (check-equal-type! (tvar->contents tvar) ty exp)
      (begin
        (check-no-occurrence! tvar ty exp)
        (tvar-set-contents! tvar ty)))))

;check-no-occurrence!
;revisa si una variable de tipo no ocurre dentro de un tipo

(define check-no-occurrence!
  (lambda (tvar ty exp)
    (letrec
      ((loop
         (lambda (ty1)
           (cases type ty1
             (atomic-type (name) #t) 
             (proc-type (arg-types result-type)
               (begin
                 (for-each loop arg-types)
                 (loop result-type)))
             (tvar-type (num vec)
               (if (tvar-non-empty? ty1)
                 (loop (tvar->contents ty1))
                 (if (eqv? tvar ty1)
                   (begin  
                    (display "me salgo") 
                   (raise-occurrence-check tvar ty exp))
                   #t)))))))
      (loop ty))))

;funciones para despliegue de errores
(define raise-type-error
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Type mismatch: ~s doesn't match ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-wrong-number-of-arguments
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Different numbers of arguments ~s and ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-occurrence-check
  (lambda (tvnum t2 exp)
    (eopl:error 'check-equal-type!
      "Can't unify: ~s occurs in type ~s in expression ~s~%" 
      ;tvnum
      (type-to-external-form tvnum)
      (type-to-external-form t2)
      exp)))

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
      (tvar-type (serial-number container) ;;; NUEVO
        (if (tvar-non-empty? ty)
          (type-to-external-form (tvar->contents ty))
          (string->symbol
            (string-append
              "tvar"
              (number->string serial-number))))))))

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
    (let ((arg-types (expand-optional-type-expressions texps tenv)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type actual-types rator rands exp)
    (let ((result-type (fresh-tvar)))
      (check-equal-type!
        rator-type
        (proc-type actual-types result-type)
        exp)
      result-type)))

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim ()
                (proc-type (list int-type int-type) int-type))
      (substract-prim ()
                      (proc-type (list int-type int-type) int-type))
      (mult-prim ()
                 (proc-type (list int-type int-type) int-type))
      (incr-prim ()
                 (proc-type (list int-type) int-type))
      (decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type))
      (menor-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-prim () (proc-type (list int-type int-type) bool-type))
      )))

;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

;type-of-primitive: (list-of <symbol>) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión let
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
  (lambda (result-texps proc-names arg-optional-texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-optional-type-expressions texps tenv))
                           arg-optional-texpss))
          (result-types (expand-optional-type-expressions result-texps tenv)))
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

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*********************************************     Procedimientos     **************************************************
;***********************************************************************************************************************

; procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;***********************************************     Ambientes     *****************************************************
;***********************************************************************************************************************

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;********************************************  Ambientes de tipos  *****************************************************
;***********************************************************************************************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;****************************************************  Tipos  **********************************************************
;***********************************************************************************************************************

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))

; expand-type-expression: <type-exp> -> <type>
; determina el tipo denotado por cada expresión de tipo
(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

;mapea la función expand-type-expression a todos los elementos de la lista texps
(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))


;fresh-tvar: -> type
;crea un nuevo tipo de variable de tipo
(define fresh-tvar
  (let ((serial-number 0))
    (lambda ()
      (set! serial-number (+ 1 serial-number))
      (tvar-type serial-number (vector '())))))

;tvar-non-empty?: type value -> bool
;determina si un tipo de variable de tipo está vacío
(define tvar-non-empty?
  (lambda (ty)
    (not (null? (vector-ref (tvar-type->container ty) 0)))))

;mapea la función expand-optional-type-expression a todos los elementos de la lista otexps
(define expand-optional-type-expressions
  (lambda (otexps tenv)
    (map
      (lambda (otexp)
        (expand-optional-type-expression otexp tenv))
      otexps)))

; expand-optional-type-expression: <type-optional-exp> -> <type>
; determina el tipo denotado por cada expresión de tipo opcional
(define expand-optional-type-expression
  (lambda (otexp tenv)
    (cases optional-type-exp otexp
      (no-type-exp () (fresh-tvar))
      (a-type-exp (texp) (expand-type-expression texp)))))


;Selectores y extractores para el tipo abstracto tipo

;tvar->contents: type -> value
;obtiene el valor almacenado en un tipo de variable de tipo
(define tvar->contents
  (lambda (ty)
    (vector-ref (tvar-type->container ty) 0)))

;tvar-set-contents!: type value -> 0
;modifica el valor almacenado en un tipo de variable de tipo
(define tvar-set-contents!
  (lambda (ty val)
    (vector-set! (tvar-type->container ty) 0 val)))

;atomic-type?: type -> bool
; determina si el argumento corresponde a un tipo atómico
(define atomic-type?
  (lambda (ty)
    (cases type ty
      (atomic-type (name) #t)
      (else #f))))

;proc-type?: type -> bool
; determina si el argumento corresponde a un tipo procedimiento
(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) #t)
      (else #f))))

;tvar-type?: type -> bool
; determina si el argumento corresponde a un tipo de variable de tipo
(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (sn cont) #t)
      (else #f))))

;atomic-type->name: type -> symbol
; retorna el nombre asociado con un tipo atómico
(define atomic-type->name
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (else (eopl:error 'atomic-type->name
              "Not an atomic type: ~s" ty)))))

;proc-type->arg-types: type -> (list-of type)
; retorna la lista de los tipos de los argumentos en un tipo procedimiento
(define proc-type->arg-types
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) arg-types)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

;proc-type->result-type: type -> type
; retorna el tipo del resultado en un tipo procedimiento
(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) result-type)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

;tvar-type->serial-number: type -> integer
; retorna el número serial asociado a un tipo de variable de tipo
(define tvar-type->serial-number
  (lambda (ty)
    (cases type ty
      (tvar-type (sn c) sn)
      (else (eopl:error 'tvar-type->serial-number
              "Not a tvar-type: ~s" ty)))))

;tvar-type->container: type -> vector
; retorna el contenedor asociado a un tipo de variable de tipo
(define tvar-type->container
  (lambda (ty)
    (cases type ty
      (tvar-type (sn vec) vec)
      (else (eopl:error 'tvar-type->container
              "Not a tvar-type: ~s" ty)))))




;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;***************************************************    Pruebas    *****************************************************
;***********************************************************************************************************************

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")


(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))

(interpretador-tipos)
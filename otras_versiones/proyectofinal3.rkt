#lang eopl



;_____________________________________________ FIN basado en _____________________________________________


;_____________________________________________ BNF en el UVProg.rkt _____________________________________________

(define scanner-spec-simple-interpreter
  '((comment ("//" (arbno (not #\newline))) skip)
    (comment ("/*" (arbno (or letter digit #\space #\newline)) "*/") skip)
    (string("'" (arbno (or letter #\space)) "'")  symbol)
    (int(digit (arbno digit)) number)
    (int("-" digit (arbno digit)) number)
    (float(digit (arbno digit) "." digit (arbno digit)) number)
    (white-sp(whitespace) skip)
    (id(letter (arbno (or letter digit "?" "_"))) symbol)))

;;_____________________________________________ FIN ESPECIFICACION LEXICA _____________________________________________

(define grammar-simple-interpreter
  '((programa ( globals "GLOBALS ""{"( arbno procedimiento )"}" "PROGRAM" "{" (arbno expresion) "return" expresion-inter ";" "}" ) un-programa)
    (globals ("GLOBALS" "{" (arbno expresion) "}") globals-block)
    
    (expresion ("if" "(" expresion-inter ")" "{" (arbno expresion) "return" expresion-inter ";" "}" "else" "{" (arbno expresion) "return" expresion-inter ";" "}") condicional-exp)
    (expresion (clase-de-igualdad expresion-inter ";") asignando)
    (expresion ("for" "(" clase-de-igualdad expresion-inter ";" expresion-inter ";" clase-de-igualdad expresion-inter ")" "{" (arbno expresion) "}") iteracion)
    (expresion ("print" "(" expresion-inter ")" ";") print)
    (expresion ("IF" "(" expresion-inter ")" "{" (arbno expresion) "}" "ELSE" "{" (arbno expresion) "}") simple-if)
    ;                                   vec,                pos,               new-value
    

    (clase-de-igualdad ("const" tipo id "=") constante)                   ;; constantes -por valor
    (clase-de-igualdad ("var" tipo  id "=") variable)                    ;; variables  -por valor
    (clase-de-igualdad ("const&" tipo  id "=") constante-ref)              ;; constantes -por ref
    (clase-de-igualdad ("var&" tipo id "=") variable-ref)               ;; variables  -por ref    
    (clase-de-igualdad ("set_var" tipo id "=") asignacion-multiple)         ;; usado para asignar variables
    (clase-de-igualdad ("let" tipo id "=") variable-unica)              ;; usado para crear variables de unica asignacion
    (clase-de-igualdad ("unique_assignment" tipo id "=") ultima-asignacion) ;; usado para asignar variables una unica vez
    ;; no hago las ref para let (variable unica) debido a que haria lo mismo que el const en general

    (tipo ("int") tipo-int)
    (tipo ("bool") tipo-bool)
    (tipo ("float") tipo-float)
    (tipo ("hexa") tipo-hexa)
    (tipo ("oct") tipo-oct)
    (tipo ("b32") tipo-b32)
    (tipo ("string") tipo-string)
    (tipo ("list") tipo-list)
    (tipo ("vector") tipo-vector)
    (tipo ("function") tipo-funcion)
    (tipo ("unknown") tipo-llamado)

    ;;por ahora no crear algo como clase-de-igualdad-con-tipos, ahora solo con id
    (procedimiento ("proc" clase-de-igualdad "(" (separated-list clase-de-igualdad ",") ")" "{" (arbno expresion) "return" expresion-inter ";" "}") procedimiento-recursivo)
    (procedimiento ("No-proc let" clase-de-igualdad "=" "null" ";") proc-no-asignado) ;;para asignar @UVProgVAL que en esta gramatica es "null"

    (oct ("[x8 " (separated-list int "-") "]") octales)
    (hexa ("[x16 " (separated-list int "-") "]") hexales)
    (b32 ("[x32 " (separated-list int "-") "]") b32s)
    (lista ("[" (separated-list expresion-inter ",") "]") listas)
    (bool ("true") true-bool)
    (bool ("false") false-bool)
    (simpleVector ("{" (separated-list expresion-inter ",") "}") vectores)
     
    (primitiva-ent ("+") primi-suma)
    (primitiva-ent ("-") primi-resta)
    (primitiva-ent ("*") primi-multi)
    (primitiva-ent ("%") primi-modulo)
    (primitiva-ent ("/") primi-divi)
    (primitiva-ent ("add1") primi-uno-mas)
    (primitiva-ent ("sub1") primi-uno-menos)
    
    (primitiva-bool ("<") primi-menor)
    (primitiva-bool (">") primi-mayor)
    (primitiva-bool ("<=") primi-menor-igual)
    (primitiva-bool (">=") primi-mayor-igual)
    (primitiva-bool ("==") primi-igual-igual)
    (primitiva-bool ("!=") primi-no-igual)
    (primitiva-bool ("and") primi-and)
    (primitiva-bool ("or") primi-or)
    (primitiva-bool ("not") primi-not)
    
    (primitiva-float ("+_float") primi-suma-float)
    (primitiva-float ("-_float") primi-resta-float)
    (primitiva-float ("*_float") primi-multi-float)
    (primitiva-float ("/_float") primi-divi-float)
    (primitiva-float ("mod_float") primi-mod-float)
    (primitiva-float ("add1_float") primi-uno-mas-float)
    (primitiva-float ("sub1_float") primi-uno-menos-float)

    (primitiva-hexa ("+_hexa") primi-suma-hexa)
    (primitiva-hexa ("-_hexa") primi-resta-hexa)
    (primitiva-hexa ("*_hexa") primi-multi-hexa)
    (primitiva-hexa ("add1_hexa") primi-uno-mas-hexa)
    (primitiva-hexa ("sub1_hexa") primi-uno-menos-hexa)

    (primitiva-oct ("+_oct") primi-suma-oct)
    (primitiva-oct ("-_oct") primi-resta-oct)
    (primitiva-oct ("*_oct") primi-multi-oct)
    (primitiva-oct ("add1_oct") primi-uno-mas-oct)
    (primitiva-oct ("sub1_oct") primi-uno-menos-oct)

    (primitiva-b32 ("+_b32") primi-suma-b32)
    (primitiva-b32 ("-_b32") primi-resta-b32)
    (primitiva-b32 ("*_b32") primi-multi-b32)
    (primitiva-b32 ("add1_b32") primi-uno-mas-b32)
    (primitiva-b32 ("sub1_b32") primi-uno-menos-b32)
    
    (expresion-inter-cadena (string) string-lit-inter)    
    (expresion-inter-cadena (id) identificafor-lit-inter-cadena)
    (expresion-inter-cadena ("concat" "(" expresion-inter "," expresion-inter ")") primi-concat)

    (expresion-inter-lista (lista) list-lit-inter)
    (expresion-inter-lista (id) identificafor-lit-inter-lista)
    (expresion-inter-lista ("empty" "(" ")") empty1)
    (expresion-inter-lista ("cons" "(" expresion-inter ","  expresion-inter ")") cons1);;dejarla asi y lanzar error si no se entra una lista
    (expresion-inter-lista ("restElements" "(" expresion-inter ")") cdr1)    
    (expresion-inter-lista ("append" "(" expresion-inter "," expresion-inter ")") append1)
    
    (expresion-inter-ent (int) numero-lit-inter)
    (expresion-inter-ent ("length" "(" expresion-inter-cadena ")") primi-length)
    (expresion-inter-ent (id) identificador-lit-inter-ent)    
    (expresion-inter-ent ("(" expresion-inter primitiva-ent expresion-inter ")") exp-aritmetica-ent)
    (expresion-inter-ent (primitiva-ent expresion-inter) binaria)
    
    (expresion-inter-bool (int) bool-lit-inter) ; if:  var bool a = bool 3 --> a = true?? --yep
    (expresion-inter-bool ("isList?" "(" expresion-inter ")") isList) 
    (expresion-inter-bool ("isEmpty?" "(" expresion-inter ")") isEmpty)
    (expresion-inter-bool (bool) bool-lit-bool)
    (expresion-inter-bool (id) identificafor-lit-inter-bool)   
    (expresion-inter-bool ("(" expresion-inter primitiva-bool expresion-inter ")") exp-aritmetica-bool)
    (expresion-inter-bool (primitiva-bool expresion-inter) exp-aritmetica-bool-bin)
    (expresion-inter-bool ("vector?" "(" expresion-inter ")" ) es-vector?)
    
    (expresion-inter-float (float) float-lit-inter)   
    (expresion-inter-float (id) identificafor-lit-inter-float)   
    (expresion-inter-float ("(" expresion-inter primitiva-float expresion-inter ")") exp-aritmetica-float)
    (expresion-inter-float (primitiva-float expresion-inter) binaria-float)
    
    (expresion-inter-hexa (hexa) hexa-lit-inter)   
    (expresion-inter-hexa (id) identificafor-lit-inter-hexa)   ;no necesario, dicho en clase
    (expresion-inter-hexa ("(" expresion-inter-hexa primitiva-hexa expresion-inter-hexa ")") exp-aritmetica-hexa)
    (expresion-inter-hexa (primitiva-hexa expresion-inter-hexa) binaria-hexa)
    
    (expresion-inter-oct (oct) oct-lit-inter)   
    (expresion-inter-oct (id) identificafor-lit-inter-oct)     ;no necesario, dicho en clase
    (expresion-inter-oct ("(" expresion-inter-oct primitiva-oct expresion-inter-oct ")") exp-aritmetica-oct)
    (expresion-inter-oct (primitiva-oct expresion-inter-oct) binaria-oct)
    
    (expresion-inter-b32 (b32) b32-lit-inter)    
    (expresion-inter-b32 (id) identificafor-lit-inter-b32)    ;no necesario, dicho en clase
    (expresion-inter-b32 ("(" expresion-inter-b32 primitiva-b32 expresion-inter-b32 ")") exp-aritmetica-b32)
    (expresion-inter-b32 (primitiva-b32 expresion-inter-b32) binaria-b32)

    (expresion-inter-vector (simpleVector) vector-lit-inter)
    (expresion-inter-vector (id) vector-lit-inter-vector)
    (expresion-inter-vector ("make-vector" "(" expresion-inter "," expresion-inter ")" ) exp-make-vector)
    (expresion-inter-vector ("vector-set!" "(" expresion-inter "," expresion-inter "," expresion-inter ")") cambiar-vector)

    (expresion-inter ("null") null-no-asignado)                                 ;;para asignar @UVProgVAL que en esta gramatica es "null"
    (expresion-inter ("int" expresion-inter-ent) una-expresion-inter-ent1)
    (expresion-inter ("bool" expresion-inter-bool) una-expresion-inter-bool1)   
    (expresion-inter ("float" expresion-inter-float) una-expresion-inter-float1)   
    (expresion-inter ("hexa" expresion-inter-hexa) una-expresion-inter-extra1)    
    (expresion-inter ("oct" expresion-inter-oct) una-expresion-inter-oct1)    
    (expresion-inter ("b32" expresion-inter-b32) una-expresion-inter-b321)    
    (expresion-inter ("string" expresion-inter-cadena) una-expresion-inter-cadena1)
    (expresion-inter ("list" expresion-inter-lista) una-expresion-inter-listas1)
    (expresion-inter ("firstElement" "(" expresion-inter-lista ")") primer-elemento);retorna cualquier cosa que tenga una lista
    (expresion-inter ("vector-ref!" "(" expresion-inter "," expresion-inter ")" ) n-elemento-vector)
    (expresion-inter ("vector" expresion-inter-vector) una-expresion-inter-vector1)
    (expresion-inter (id "(" (separated-list expresion-inter ",") ")") proc-evaluacion)
    
    ))

;_____________________________________________ scan&parse y just-scan _____________________________________________

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;_____________El Interpretador (FrontEnd + Evaluación + señal para lectura ) ________________

(define interpretador
  (sllgen:make-rep-loop  "--> "
                         (lambda (pgm)
                           (eval-program  pgm ambiente-global ambiente-procs)) 
                         (sllgen:make-stream-parser 
                          scanner-spec-simple-interpreter
                          grammar-simple-interpreter)))

(define eval-program
  (lambda (pgm env ambiente-solo-procs)
    (cases programa pgm
      (un-programa (globals procs body return)
                   (begin
                     (guardar-procs-en-global-env ambiente-solo-procs procs))
                   (eval-expresions  body return env)
                   
                   
                   ))))




;;mirar si hay un conidional, si lo hay, no se evalua el ultimo return
(define eval-expresions
  (lambda (body return env)
    (let*
        (
         (return-cercano '())
         (return-cercano-encontrado? #f)
         (lista-resultado-map (map (
                                    lambda (x)
                                     (if (condicional-encontrado? x return-cercano-encontrado? env)
                                         (begin
                                           (set! return-cercano-encontrado? #t)
                                           (set! return-cercano x))
                                         
                                         (if return-cercano-encontrado?
                                             '()
                                             (eval-expresion x env)))) body)))
      (if (eq? return-cercano '())
          (eval-expresion-inter return ambiente-global)
          (eval-expresion return-cercano ambiente-global)
          ))))



(define condicional-encontrado?
  (lambda (exp boolean env)
    (cases expresion exp
      (condicional-exp (condicion true-body true-return false-body false-return)
                       (if  (not boolean)
                            #t
                            #f))
      
      (else #f))))


(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (condicional-exp (condicion true-body true-return false-body false-return)
                       (eval-condicional-exp condicion true-body true-return false-body false-return env))
      (asignando (clase valor)
                 (begin
                   (si-proc-entonces-evaluar valor env);;por si tiene referencia y hay que hacer un cambio por referencia
                   (asignar-o-crear-para-procs clase valor env))
                 )
      (simple-if (condicion true-body false-body)

                 (begin
                   (cases expresion-inter condicion
                     (una-expresion-inter-bool1 (exp-bool)
                                                (if (eval-inter-exp-bool exp-bool env)
                                                    (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                                    (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                     (proc-evaluacion (id-proc rands)
                                      (if (eval-procedimiento id-proc rands env)
                                          (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                          (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                     (else
                      (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))
                   )
                 )
      (iteracion (clase-declaracion valor-inicial condicion clase-asignacion paso body)
                 (eval-for clase-declaracion valor-inicial condicion clase-asignacion paso body env))
      (print (exp-inter)
             (write (eval-expresion-inter exp-inter env)))
      )))


(define eval-for
  (lambda (clase-declaracion valor-inicial condicion clase-asignacion paso body env)
    (letrec
        (
         (ambiente-aislado (extended-env-record (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) env));extender
                 
         (asignando-valor-inicial
          (asignar-o-crear-valor-inicial-for clase-declaracion valor-inicial ambiente-aislado))

         (degueando (set! ambiente-debug ambiente-aislado))
         
         (para-que-el-return-coja-lo-ultimo (set! ambiente-global ambiente-aislado))

         ;(nada (write (eval-expresion-inter valor-inicial env)));solo retorna #void
         ) 
                 
      (cases expresion-inter condicion
        (una-expresion-inter-bool1 (exp-bool)
                                   (if (eval-inter-exp-bool exp-bool ambiente-aislado)
                                       (begin
                                         (map (lambda (x) (eval-expresion-solo-for x ambiente-aislado)) body)
                                         (eval-for clase-declaracion paso condicion clase-asignacion paso body ambiente-aislado)
                                         )
                                       
                                       (set! ambiente-global ambiente-aislado)));;cuando se termina el for, que el ambiente aislado sea el ambiente-glob orignal
        (proc-evaluacion (id-proc rands)
                         (if (eval-procedimiento id-proc rands ambiente-aislado)
                             (begin
                               (map (lambda (x) (eval-expresion-solo-for x ambiente-aislado)) body)
                               (eval-for clase-declaracion paso condicion clase-asignacion paso body ambiente-aislado)
                               )
                                       
                             (set! ambiente-global ambiente-aislado)))
        (else
         (eopl:error 'empty-env "La expresion ~s no es booleana" condicion))))
    ))




(define asignar-o-crear-valor-inicial-for
  (lambda (clase valor env)
    (cases clase-de-igualdad clase
      (variable (tipo id)
                (if (search-sym-in-this-env env id)
                    (eopl:error 'empty-env "Variable ~s already created" id)
                    (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                        (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                        (if (search-sym-in-env ambiente-global id)
                            (allocate-in-env env "var" (eval-tipo tipo) id valor)
                            (allocate-in-env ambiente-global "var" (eval-tipo tipo) id valor)))))
  
      (variable-ref (tipo id)
                    (if (search-sym-in-this-env env id)
                        (eopl:error 'empty-env "Variable ~s already created" id)
                        (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                            (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                            (if (search-sym-in-env env (get-id-of-expresion-inter valor env))
                                (allocate-in-env env "var&" (eval-tipo tipo) id (get-id-of-expresion-inter valor env))
                                (eopl:error 'empty-env "identifier ~s is not already created" id)))))
      (else (eopl:error 'error-logico "En el primer slot del for solo se permite crear variables"))
      )))

(define asignar-o-crear-paso-for
  (lambda (clase valor env)
    (cases clase-de-igualdad clase
      (asignacion-multiple (tipo id)
                           (if (search-sym-in-this-env env id)
                               (eopl:error 'empty-env "Variable ~s already created" id)
                               (if (and (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env)))
                                        (not (eq? (get-tipo-of-expresion-inter valor env) "unknown" )))
                                   (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                   (if (search-sym-in-env ambiente-global id)
                                       (allocate-in-env env "var" (eval-tipo tipo) id valor)
                                       (allocate-in-env ambiente-global "var" (eval-tipo tipo) id valor)))))
      (else (eopl:error 'error-logico "En el tercer slot del for solo se permite setear variables"))
      )))


(define eval-expresion-IF-for
  (lambda (exp env)
    (cases expresion exp
      (condicional-exp (condicion true-body true-return false-body false-return)
                       (eopl:error "Un if con return no puede ir dentro de un IF o de un for"))
      (asignando (clase valor)
                 (begin
                   (si-proc-entonces-evaluar valor env);;por si tiene referencia y hay que hacer un cambio por referencia
                   (letrec
                       (
                        (ambiente-aislado (extended-env-record (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) env))
                        (as (asignar-o-crear-para-procs clase valor ambiente-aislado))
                        )
                     (set! ambiente-global ambiente-aislado))
                   ))
      (simple-if (condicion true-body false-body)

                 (begin
                   (cases expresion-inter condicion
                     (una-expresion-inter-bool1 (exp-bool)
                                                (if (eval-inter-exp-bool exp-bool env)
                                                    (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                                    (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                     (proc-evaluacion (id-proc rands)
                                      (if (eval-procedimiento id-proc rands env)
                                          (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                          (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                     (else
                      (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))
                   )
                 )
      (iteracion (clase-declaracion valor-inicial condicion clase-asignacion paso body)
                 (eopl:error 'esthetic-error "Si se usa procedimiento no se necesita for, debido a que se tiene recursion"))
      (print (exp-inter)
             (write (eval-expresion-inter exp-inter env)))
      
      )
    ))

(define eval-condicional-exp
  (lambda (condicion true-body true-return false-body false-return env)
    (cases expresion-inter condicion
      (una-expresion-inter-bool1 (exp-bool)
                                 (eval-body-condiconal-exp (eval-inter-exp-bool exp-bool env)  true-body true-return false-body false-return env))
      (proc-evaluacion (id-proc rands)
                       (eval-body-condiconal-exp (eval-procedimiento id-proc rands env)  true-body true-return false-body false-return env))
      (else
       (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))))


(define eval-body-condiconal-exp
  (lambda (condicion-evaluacion true-body true-return false-body false-return env)
    (if condicion-evaluacion
        (eval-expresions true-body true-return env)
        (eval-expresions false-body false-return env))))

(define eval-expresions-procs
  (lambda (body return env)
    (let*
        (
         (return-cercano '())
         (return-cercano-encontrado? #f)
         (lista-resultado-map (map (
                                    lambda (x)
                                     (if (condicional-encontrado? x return-cercano-encontrado? env)
                                         (begin
                                           (set! return-cercano-encontrado? #t)
                                           (set! return-cercano x))
                                         
                                         (if return-cercano-encontrado?
                                             '()
                                             (eval-expresion-procs x env)))) body)))
      (if (eq? return-cercano '())
          (eval-expresion-inter return env)
          (eval-expresion-procs return-cercano env)
          ))))

(define eval-expresion-solo-for
  (lambda (exp env)
    (cases expresion exp
      (condicional-exp (condicion true-body true-return false-body false-return)
                       (eval-condicional-exp-procs condicion true-body true-return false-body false-return env))
      (asignando (clase valor)
                 (begin
                   (si-proc-entonces-evaluar valor env);;por si tiene referencia y hay que hacer un cambio por referencia
                   (asignar-o-crear-paso-for clase valor env)))
      (simple-if (condicion true-body false-body)
                 ;(begin
                 (cases expresion-inter condicion
                   (una-expresion-inter-bool1 (exp-bool)
                                              (if (eval-inter-exp-bool exp-bool env)
                                                  (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                                  (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                   (proc-evaluacion (id-proc rands)
                                    (if (eval-procedimiento id-proc rands env)
                                        (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                        (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                   (else
                    (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))
                 ;)
                 )
      (iteracion (clase-declaracion valor-inicial condicion clase-asignacion paso body)
                 (eval-for clase-declaracion valor-inicial condicion clase-asignacion paso body env))
      (print (exp-inter)
             (write (eval-expresion-inter exp-inter env)))
      
      )))


(define eval-expresion-procs
  (lambda (exp env)
    (cases expresion exp
      (condicional-exp (condicion true-body true-return false-body false-return)
                       (eval-condicional-exp-procs condicion true-body true-return false-body false-return env))
      (asignando (clase valor)
                 (begin
                   (si-proc-entonces-evaluar valor env);;por si tiene referencia y hay que hacer un cambio por referencia
                   (asignar-o-crear-para-procs clase valor env)))
      (simple-if (condicion true-body false-body)
                 ;(begin
                 (cases expresion-inter condicion
                   (una-expresion-inter-bool1 (exp-bool)
                                              (if (eval-inter-exp-bool exp-bool env)
                                                  (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                                  (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                   (proc-evaluacion (id-proc rands)
                                    (if (eval-procedimiento id-proc rands env)
                                        (map (lambda (x) (eval-expresion-IF-for x env)) true-body)
                                        (map (lambda (x) (eval-expresion-IF-for x env)) false-body)))
                   (else
                    (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))
                 ;)
                 )
      (iteracion (clase-declaracion valor-inicial condicion clase-asignacion paso body)
                 (eopl:error 'esthetic-error "Si se usa procedimiento no se necesita for, debido a que se tiene recursion"))
      (print (exp-inter)
             (write (eval-expresion-inter exp-inter env)))
      
      
      )))

(define eval-condicional-exp-procs
  (lambda (condicion true-body true-return false-body false-return env)
    (cases expresion-inter condicion
      (una-expresion-inter-bool1 (exp-bool)
                                 (eval-body-condiconal-exp-procs (eval-inter-exp-bool exp-bool env)  true-body true-return false-body false-return env))
      (proc-evaluacion (id-proc rands)
                       (eval-body-condiconal-exp-procs (eval-procedimiento id-proc rands env)  true-body true-return false-body false-return env))
      
      (else
       (eopl:error 'empty-env "La expresion ~s no es booleana" condicion)))))

(define eval-body-condiconal-exp-procs
  (lambda (condicion-evaluacion true-body true-return false-body false-return env)
    (if condicion-evaluacion
        (eval-expresions-procs true-body true-return env)
        (eval-expresions-procs false-body false-return env))))

;;asigna o aloja un procedimiento o una variable o const...
(define asignar-o-crear
  (lambda (clase valor env)
    (cases clase-de-igualdad clase
      (constante (tipo id)
                 (if (search-sym-in-env env id)
                     (eopl:error 'empty-env "Constant ~s already created" id)
                     (if (not (eq? (eval-tipo tipo) "function"))
                         (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                             (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                             (allocate-in-env env "const" (eval-tipo tipo) id valor))
                         (allocate-in-env env "const" (eval-tipo tipo) id valor))))
      (variable (tipo id)
                (if (search-sym-in-env env id)
                    (eopl:error 'empty-env "Variable ~s already created" id)
                    (if (not (eq? (eval-tipo tipo) "function"))
                        (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                            (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                            (allocate-in-env env "var" (eval-tipo tipo) id valor))
                        (allocate-in-env env "var" (eval-tipo tipo) id valor))))
      (constante-ref (tipo id)
                     (if (search-sym-in-env env id)
                         (eopl:error 'empty-env "Constant ~s already created with the same name" id)
                         (if (not (eq? (eval-tipo tipo) "function"))
                             (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                 (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                 (allocate-in-env env "const&" (eval-tipo tipo) id valor))
                             (allocate-in-env env "const&" (eval-tipo tipo) id valor))))
      (variable-ref (tipo id)
                    (if (search-sym-in-env env id)
                        (eopl:error 'empty-env "Variable ~s already created" id)
                        (if (not (eq? (eval-tipo tipo) "function"))
                            (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                (if (search-sym-in-env env (get-id-of-expresion-inter valor env))
                                    (allocate-in-env env "var&" (eval-tipo tipo) id (get-id-of-expresion-inter valor env))
                                    (eopl:error 'empty-env "identifier ~s is not already created" id)))
                            (allocate-in-env env "var&" (eval-tipo tipo) id (get-id-of-expresion-inter valor env)))))
      (asignacion-multiple (tipo id)
                           (if (and (search-sym-in-env env id) (or (eq? (type-class-of env id) "var") (eq? (type-class-of env id) "var&")))
                               (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                   (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                   (if (eq? (type-class-of env id) "var")
                                       (set-in-env env id valor)
                                       (if (search-sym-in-env env (get-value-of env id)) ;; referencia a un blanco indirecto?
                                           (set-in-env env (get-value-of env id) valor) ;;si referencia a un blanco indirecto setee ese blanco indirecto
                                           (set-in-env env id valor))));; si referencia a un blanco directo solo cambie de valor
                               (eopl:error 'empty-env "variable ~s not created yet" id)))
      (variable-unica (tipo id)
                      (if (search-sym-in-env env id)
                          (eopl:error 'empty-env "Variable unica ~s already created" id)
                          (if (not (eq? (eval-tipo tipo) "function"))
                              (if (not (or (eq? (eval-tipo tipo) "null") (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))))
                                  (eopl:error 'type-error "Type: ~s is diferent of ~s" tipo(get-tipo-of-expresion-inter valor env))
                                  (allocate-in-env env "let" (eval-tipo tipo) id valor))
                              (allocate-in-env env "let" (eval-tipo tipo) id valor))))
      (ultima-asignacion (tipo id)
                         (if (and (search-sym-in-env env id) (eq? (type-class-of env id) "let")) 
                             (if (eqv? (eval-expresion-inter (apply-env env id) env)  "null");para este caso apply-env retorna sint abstracta
                                 (set-in-env env id valor)
                                 (eopl:error 'empty-env "Variable ~s has already been assigned" id))
                             ((eopl:error 'empty-env "Variable of unique assignment ~s is not created yet" id)))))))

(define eval-expresion-inter
  (lambda (exp-inter env)
    (cases expresion-inter exp-inter
      (null-no-asignado () "null")
      (una-expresion-inter-ent1 (exp-int) (eval-inter-exp-ent exp-int env))
      (una-expresion-inter-bool1 (exp-bool) (eval-inter-exp-bool exp-bool env))
      (una-expresion-inter-float1 (exp-float) (eval-inter-exp-float exp-float env))
      (una-expresion-inter-extra1 (exp-hexa) (eval-inter-exp-hexa exp-hexa env))
      (una-expresion-inter-oct1 (exp-oct) (eval-inter-exp-oct exp-oct env))
      (una-expresion-inter-b321 (exp-b32) (eval-inter-exp-b32 exp-b32 env))
      (una-expresion-inter-cadena1 (exp-cadena) (eval-inter-exp-cadena exp-cadena env))
      (una-expresion-inter-listas1 (una-lista) (eval-inter-exp-lista una-lista env))
      (primer-elemento (una-lista) (eval-firstElement una-lista env))
      (proc-evaluacion (id clase-y-rands) (eval-procedimiento id clase-y-rands env ambiente-procs))
      (una-expresion-inter-vector1 (un-vector) (eval-inter-exp-vector un-vector env))
      (n-elemento-vector (exp-inter1 exp-inter2) (eval-vector-ref! exp-inter1 exp-inter2 env))
      )))

(define eval-tipo
  (lambda (tipo-exp)
    (cases tipo tipo-exp
      (tipo-int () "int")
      (tipo-bool () "bool")
      (tipo-float () "float")
      (tipo-hexa () "hexa")
      (tipo-oct () "oct")
      (tipo-b32 () "b32")
      (tipo-string () "string")
      (tipo-list () "list")
      (tipo-vector () "vector")
      (tipo-funcion () "function")
      (tipo-llamado () "unknown")
      )))

(define get-tipo-of-expresion-inter
  (lambda (exp-inter env)
    (cases expresion-inter exp-inter
      (null-no-asignado () "null")
      (una-expresion-inter-ent1 (exp-int) "int")
      (una-expresion-inter-bool1 (exp-bool) "bool")
      (una-expresion-inter-float1 (exp-float) "float")
      (una-expresion-inter-extra1 (exp-hexa) "hexa")
      (una-expresion-inter-oct1 (exp-oct) "oct")
      (una-expresion-inter-b321 (exp-b32) "b32") 
      (una-expresion-inter-cadena1 (exp-cadena) "string")
      (una-expresion-inter-listas1 (una-lista) "list")
      (primer-elemento (una-lista) "unknown")
      (proc-evaluacion (id clase-y-rands) "unknown")
      (una-expresion-inter-vector1 (un-vector) "vector")
      (n-elemento-vector (exp-inter1 exp-inter2) "unkwon")
      )))

(define get-id-of-expresion-inter
  (lambda (exp-inter env)
    (cases expresion-inter exp-inter
      (null-no-asignado () "null")
      (una-expresion-inter-ent1 (exp-int) (cases  expresion-inter-ent exp-int (identificador-lit-inter-ent (id) id) (else -1)))
      (una-expresion-inter-bool1 (exp-bool) (cases  expresion-inter-bool exp-bool (identificafor-lit-inter-bool (id) id) (else -1)))
      (una-expresion-inter-float1 (exp-float) (cases  expresion-inter-float exp-float (identificafor-lit-inter-float (id) id) (else -1)))
      (una-expresion-inter-extra1 (exp-hexa) (cases  expresion-inter-hexa exp-hexa (identificafor-lit-inter-hexa (id) id) (else -1)))
      (una-expresion-inter-oct1 (exp-oct) (cases  expresion-inter-oct exp-oct (identificafor-lit-inter-oct (id) id) (else -1)))
      (una-expresion-inter-b321 (exp-b32) (cases  expresion-inter-b32 exp-b32 (identificafor-lit-inter-b32 (id) id) (else -1)))
      (una-expresion-inter-cadena1 (exp-cadena) (cases  expresion-inter-cadena exp-cadena (identificafor-lit-inter-cadena (id) id) (else -1)))
      (una-expresion-inter-listas1 (exp-lista) (cases  expresion-inter-lista exp-lista (identificafor-lit-inter-lista (id) id) (else -1)))
      (proc-evaluacion (id clase-y-rands) id)
      (una-expresion-inter-vector1 (exp-vector) (cases  expresion-inter-vector exp-vector (vector-lit-inter-vector (id) id) (else -1)))
      (else
       "no tiene id")
      )))

;si es un procedimiento entonces evaluese
(define si-proc-entonces-evaluar
  (lambda (exp-inter env)
    (cases expresion-inter exp-inter
      (proc-evaluacion (id clase-y-rands) (eval-procedimiento id clase-y-rands env ambiente-procs))
      (else
       exp-inter)
      )))
;*******************************************************************************************
;------------------------ evaluaciones relacionadas con vectores ---------------------------
;*******************************************************************************************

(define eval-inter-exp-vector
  (lambda (un-vector env)
    (cases  expresion-inter-vector un-vector
      (vector-lit-inter (un-vector) un-vector)
      (vector-lit-inter-vector (identificador)
                               (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                   (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "vector")
                                           (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                       (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                       (eopl:error 'empty-env "No equal type for ~s" identificador))
                                   (if (or (eq? (type-of-sym env identificador) "vector")
                                           (eq? (type-of-sym env identificador) "unknown"))
                                       (eval-expresion-inter (apply-env env identificador) env)
                                       (eopl:error 'empty-env "No equal type for ~s" identificador))))
      (exp-make-vector (exp-int1 exp-int2)
                       (make-vector (eval-expresion-inter exp-int1 env) (eval-expresion-inter exp-int2 env)))
                                    
      (cambiar-vector (exp-int1 exp-int2 exp-int3)
                      (letrec
                          ((a (eval-expresion-inter exp-int1 env))
                           (b (vector-set! a (eval-expresion-inter exp-int2 env) (eval-expresion-inter exp-int3 env))))
                        a))
      )))
      


;(eval-vector-ref! exp-inter1 exp-inter2 env)
(define eval-vector-ref!
  (lambda (exp-inter1 exp-inter2 env)
    (vector-ref (eval-expresion-inter exp-inter1 env) (eval-expresion-inter exp-inter2 env))))






;*******************************************************************************************
;----------------------------- evaluaciones relacionadas con enteros  ----------------------------------
;*******************************************************************************************


(define eval-primitiva-ent
  (lambda (rand1 x rand2)
    (cases primitiva-ent x
      (primi-suma () (+ rand1 rand2))
      (primi-resta () (- rand1 rand2))
      (primi-multi () (* rand1 rand2))
      (primi-modulo () (modulo rand1 rand2))
      (primi-divi () (/ rand1 rand2))
      (else 0))))

(define eval-primitiva-bin
  (lambda (x rand)
    (cases primitiva-ent x
      (primi-uno-mas () (+ rand 1))
      (primi-uno-menos () (- rand 1))
      (else 0))))

;evalua sintaxis abstracta de expresiones enteras. Usa primitivas enteras. Da como resultado enteros.
(define eval-inter-exp-ent
  (lambda (x env)
    (cases expresion-inter-ent x
      (numero-lit-inter (entero) entero)
      (primi-length (cadena) (length cadena))
      (identificador-lit-inter-ent (identificador)
                                   (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                       (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "int")
                                               (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))
                                       (if (or (eq? (type-of-sym env identificador) "int")
                                               (eq? (type-of-sym env identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) env)
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-ent (rand1 primitiva-ent rand2) (eval-primitiva-ent (eval-expresion-inter rand1 env) primitiva-ent (eval-expresion-inter rand2 env)))
      (binaria (primitiva-bin rand) (eval-primitiva-bin primitiva-bin (eval-expresion-inter rand env))))))


;*******************************************************************************************
;------------------------------ evaluaciones relacionas con bool        ----------------------------------
;*******************************************************************************************

(define eval-primitiva-bool
  (lambda (rand1 x rand2)
    (cases primitiva-bool x
      (primi-menor () (< rand1 rand2))
      (primi-mayor () (> rand1 rand2))
      (primi-menor-igual () (<= rand1 rand2))
      (primi-mayor-igual () (>= rand1 rand2))
      (primi-igual-igual () (eqv? rand1 rand2))
      (primi-no-igual () (not (eqv? rand1 rand2)))
      (primi-and () (and rand1 rand2))
      (primi-or () (or rand1 rand2))
      (else 0))))

(define eval-primitiva-bool-bin
  (lambda (x rand)
    (cases primitiva-bool x
      (primi-not () (not rand))
      (else 0))))

;evalua sintaxis abstracta de expresiones enteras. Usa primitivas bool. Da como resultado bool.
(define eval-inter-exp-bool
  (lambda (x env)
    (cases expresion-inter-bool x
      (bool-lit-inter (boolean) boolean)
      (isList (exp-int) (list? (eval-expresion-inter exp-int)))
      (isEmpty (exp-int) (eqv? '() (eval-expresion-inter exp-int env)))
      (bool-lit-bool (boolean) (eval-bool boolean env))
      (identificafor-lit-inter-bool (identificador)
                                    (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                        (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "bool")
                                                (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                            (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                            (eopl:error 'empty-env "No equal type for ~s" identificador))
                                        (if (or (eq? (type-of-sym env identificador) "bool")
                                                (eq? (type-of-sym env identificador) "unknown"))
                                            (eval-expresion-inter (apply-env env identificador) env)
                                            (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-bool (exp-inter1 primi-bool exp-inter2)
                           (eval-primitiva-bool (eval-expresion-inter exp-inter1 env) primi-bool (eval-expresion-inter exp-inter2 env)))
      (exp-aritmetica-bool-bin (primi-bool exp-inter) (eval-primitiva-bool-bin primi-bool (eval-expresion-inter exp-inter env)))
      (es-vector? (exp-inter) (eval-expresion-inter exp-inter env))
      )))

(define eval-bool
  (lambda (x env)
    (cases bool x
      (true-bool () #t)
      (false-bool () #f))))



;*******************************************************************************************
;----------------------aplicacion de procedimientos----------------------------
;*******************************************************************************************

(define quitar-ultima-extencion-de-ambiente
  (lambda (env)
    (cases environment env
      (empty-env-record () env)
      (extended-env-record (type-classes types syms vals old-env) old-env)
      )))



(define eval-procedimiento
  (lambda (proc-id rands-values env env-procs)
    
    (letrec
        (
         (ambiente-aislado (extended-env-record (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) env));extender
         (closure-proc (get-value-of env-procs proc-id));;saco los ids, body, return de un env que solo tiene los procs
         (rands-ids (car closure-proc))
         (body-proc (cadr closure-proc))
         (return-proc (caddr closure-proc))
         (expandir-los-args (colocar-valores-a-argumentos-de-proc ambiente-aislado rands-ids rands-values))
         (degueando (set! ambiente-debug ambiente-aislado))
         )

      (eval-expresions-procs body-proc return-proc ambiente-aislado)
      )))
 

;función que mapea a todos los argumentos de un procedimiento. (Llama a colocar-valor-a-argumento-de-proc en cada map)
(define colocar-valores-a-argumentos-de-proc
  (lambda (env id-args id-values)
    (map (lambda (x y) (asignar-o-crear-para-procs x y env)) id-args id-values)
    ))
(define asignar-o-crear-para-procs
  (lambda (clase valor env)
    (cases clase-de-igualdad clase
      (constante (tipo id)
                 (if (search-sym-in-this-env env id)
                     (eopl:error 'empty-env "Constant ~s already created" id)
                     (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                         (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                         (allocate-in-env env "const" (eval-tipo tipo) id valor))))
      (variable (tipo id)
                (if (search-sym-in-this-env env id)
                    (eopl:error 'empty-env "Variable ~s already created" id)
                    (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                        (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                        (allocate-in-env env "var" (eval-tipo tipo) id valor))))
      (constante-ref (tipo id)
                     (if (search-sym-in-this-env env id)
                         (eopl:error 'empty-env "Constant ~s already created" id)
                         (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                             (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                             (allocate-in-env env "const&" (eval-tipo tipo) id valor))))
      (variable-ref (tipo id)
                    (if (search-sym-in-this-env env id)
                        (eopl:error 'empty-env "Variable ~s already created" id)
                        (if (search-sym-in-env env (get-id-of-expresion-inter valor env))
                            (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                (allocate-in-env env "var&" (eval-tipo tipo) id (get-id-of-expresion-inter valor env)))
                            (eopl:error 'empty-env "identifier ~s is not already created" id))))
      (asignacion-multiple (tipo id)
                           (if (and (search-sym-in-env env id) (or (eq? (type-class-of env id) "var") (eq? (type-class-of env id) "var&")))
                               (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                   (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                   (if (eq? (type-class-of env id) "var")
                                       (begin
                                         (set-in-env env id valor)
                                         (allocate-in-env env "var" (eval-tipo tipo) id valor))
                                       (if (search-sym-in-env env (get-value-of env id)) ;; referencia a un blanco indirecto?
                                           (set-in-env ambiente-global (get-value-of env id) valor) ;;si referencia a un blanco indirecto setee ese blanco indirecto
                                           (set-in-env env id valor))));; si referencia a un blanco directo solo cambie de valor
                               (eopl:error 'empty-env "variable ~s not created yet" id)))
      (variable-unica (tipo id)
                      (if (search-sym-in-this-env env id)
                          (eopl:error 'empty-env "Variable unica ~s already created" id)
                          
                          (if (not (or (eq? (eval-tipo tipo) "null") (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env)))) 
                              (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                              (allocate-in-env env "let" (eval-tipo tipo) id valor))))
      (ultima-asignacion (tipo id)
                         (if (and (search-sym-in-this-env env id) (eq? (type-class-of env id) "let"))
                             (if (not (eq? (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))) 
                                 (eopl:error 'type-error "Type: ~s is diferent of ~s" (eval-tipo tipo) (get-tipo-of-expresion-inter valor env))
                                 (if (eqv? (eval-expresion-inter (apply-env env id) env)  "null");para este caso apply-env retorna sint abstracta
                                     (set-in-env env id valor)
                                     (eopl:error 'empty-env "Variable ~s has already been assigned" id)))
                             ((eopl:error 'empty-env "Variable of unique assignment ~s is not created yet" id)))))))











;*******************************************************************************************
;------------------------------procedimientos        ----------------------------------
;*******************************************************************************************

;creacion de closures para cada procedmiento.
(define guardar-procs-en-global-env
  (lambda (env procs)
    (map (lambda (x) (guardar-closure x env)) procs)))

(define guardar-closure
  (lambda (proc env-de-procs)
    (cases procedimiento proc
      (procedimiento-recursivo (clase-de-igual-id rands-ids body return)
                               (asignar-o-crear clase-de-igual-id (list rands-ids body return)  env-de-procs))
      (proc-no-asignado (clase-de-igual)
                        (asignar-o-crear clase-de-igual "null" env-de-procs)))))




;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (type-classes vector?)
                       (types vector?)
                       (syms vector?)
                       (vals vector?)
                       (env environment?)))

(define ambiente-procs (extended-env-record (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (empty-env-record)))
(define ambiente-global (extended-env-record (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) (make-vector 10 0) ambiente-procs))
(define ambiente-debug "None")

(define scheme-value? (lambda (v) #t))

;función que crea un ambiente extendido con los parametros dados
(define extend-env
  (lambda (type-classes types syms vals env)
    (extended-env-record type-classes types syms vals env)))

;función que busca un valor en un ambiente, si lo encuentra retorna el valor que representa el search-sym, sino lanza un error
(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 (if (eq? (vector-ref type-classes pos) "var&");el vector es una referenica?
                                     (apply-env env (get-value-of env search-sym));retorna el blanco directo del blanco inderecto
                                     (vector-ref vals pos))
                                 (apply-env old-env search-sym)))))))

;función que busca un valor en un ambiente, si lo encuentra retorna #t, sino retorna #f
(define search-sym-in-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        #f)
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 #t
                                 (search-sym-in-env old-env search-sym)))))))

;función que busca un tipo de clase de algo en un ambiente, si lo encuentra retorna la clase, sino lanza un error
(define type-class-of
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No clase asignada a ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 (vector-ref type-classes pos)
                                 (type-class-of old-env search-sym)))))))

;función que busca un tipo de clase de algo en un ambiente, si lo encuentra retorna el valor de un id en un env, sino lanza un error
(define get-value-of
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No valor encontrado al id a ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 (vector-ref vals pos)
                                 (type-class-of old-env search-sym)))))))


;función que busca un tipo de clase de algo en un ambiente, si lo encuentra retorna la tipo, sino lanza un error
(define type-of-sym
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'Error-de-tipos "Tipos no coinciden para ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 (vector-ref types pos)
                                 (type-of-sym old-env search-sym)))))))

;función que setea un valor de un sym en un ambiente, si lo encuentra lo cambia, sino lanza un error
(define set-in-env
  (lambda (env search-sym val)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No clase asignada a ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 (if (and (not (eq? (vector-ref type-classes pos) "const")) (not (eq? (vector-ref type-classes pos) "const&")))
                                     (vector-set! vals pos val)
                                     (eopl:error 'error-logico "Seteo ilegal de ~s" search-sym))
                                 (set-in-env old-env search-sym val)))))))

;función que busca un slot en la ultima capa del env (extendido) que no este tomado y aloja algo, sino lanza un error
(define allocate-in-env
  (lambda (env type-class type search-sym val)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No sltos disponibles para alojar ~s" search-sym))
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((slot-disponible (list-find-position 0 (vector->list syms))))
                             (if (number? slot-disponible)
                                 (begin
                                   (vector-set! type-classes slot-disponible type-class)
                                   (vector-set! types slot-disponible type)
                                   (vector-set! syms slot-disponible search-sym)
                                   (vector-set! vals slot-disponible val))
                                 (allocate-in-env old-env type-class type search-sym val)))))))
;(allocate-in-env global-env "const" "null" 'c 3)
;..............auxiliares para ambientes de procs........................................

;función que busca un valor en el ambiente mas exterior, si lo encuentra retorna #t, sino retorna #f
(define search-sym-in-this-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
                        #f)
      (extended-env-record (type-classes types syms vals old-env)
                           (let ((pos (list-find-position search-sym (vector->list syms))))
                             (if (number? pos)
                                 #t
                                 #f))))))


;Funciones Auxiliares

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

;******************************************************************************************











;*******************************************************************************************
;------------------------------ evaluaciones relacionas con float        ----------------------------------
;*******************************************************************************************
(define eval-primitiva-float
  (lambda (rand1 x rand2)
    (cases primitiva-float x
      (primi-suma-float () (+ rand1 rand2))
      (primi-resta-float () (- rand1 rand2))
      (primi-multi-float () (* rand1 rand2))
      (primi-divi-float () (/ rand1 rand2))
      (primi-mod-float () (modulo rand1 rand2))
      (else 0))))

(define eval-primitiva-bin-float
  (lambda (x rand)
    (cases primitiva-float x
      (primi-uno-mas-float () (+ rand 1))
      (primi-uno-menos-float () (- rand 1))
      (else 0))))

;evalua sintaxis abstracta de expresiones flotantes. Usa primitivas floatantes. Da como resultado flotantes.
(define eval-inter-exp-float
  (lambda (x env)
    (cases expresion-inter-float x
      (float-lit-inter (flotante) flotante)
      (identificafor-lit-inter-float (identificador)
                                     (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                         (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "float")
                                                 (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                             (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                             (eopl:error 'empty-env "No equal type for ~s" identificador))
                                         (if (or (eq? (type-of-sym env identificador) "float")
                                                 (eq? (type-of-sym env identificador) "unknown"))
                                             (eval-expresion-inter (apply-env env identificador) env)
                                             (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-float (rand1 primitiva-float rand2) (eval-primitiva-float (eval-expresion-inter rand1 env) primitiva-float (eval-expresion-inter rand2 env)))
      (binaria-float (primitiva-bin-float rand) (eval-primitiva-bin-float primitiva-bin-float (eval-expresion-inter rand env))))))

;*******************************************************************************************
;------------------------------ evaluaciones relacionas con hexa        ----------------------------------
;*******************************************************************************************
(define eval-primitiva-hexa
  (lambda (rand1 x rand2)
    (if (and (esBase-N? rand1 16) (esBase-N? rand2 16))
        (cases primitiva-hexa x
          (primi-suma-hexa () (suma rand1 rand2 15))
          (primi-resta-hexa () (resta rand1 rand2 15))
          (primi-multi-hexa () (multiplicacion rand1 rand2 15))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x))))

(define eval-primitiva-bin-hexa
  (lambda (x rand)
    (if (esBase-N? rand 16) 
        (cases primitiva-hexa x
          (primi-uno-mas-hexa () (add1 rand 15))
          (primi-uno-menos-hexa () (sub1 rand 15))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x)
        )))

(define eval-inter-exp-hexa
  (lambda (x env)
    (cases expresion-inter-hexa x
      (hexa-lit-inter (hexal) (eval-hexal-exp hexal env))
      (identificafor-lit-inter-hexa (identificador)
                                    (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                        (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "hexa")
                                                (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                            (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                            (eopl:error 'empty-env "No equal type for ~s" identificador))
                                        (if (or (eq? (type-of-sym env identificador) "hexa")
                                                (eq? (type-of-sym env identificador) "unknown"))
                                            (eval-expresion-inter (apply-env env identificador) env)
                                            (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-hexa (rand1 primitiva-hexa rand2)
                           (eval-primitiva-hexa (eval-inter-exp-hexa rand1 env) primitiva-hexa (eval-inter-exp-hexa rand2 env)))
      (binaria-hexa (primitiva-bin-hexa rand) (eval-primitiva-bin-hexa primitiva-bin-hexa (eval-inter-exp-hexa rand env))))))

(define eval-hexal-exp
  (lambda (x env)
    (cases hexa x
      (hexales (lista-hexa) lista-hexa))))



;*******************************************************************************************
;------------------------------ evaluaciones relacionas con oct        ----------------------------------
;*******************************************************************************************
(define eval-primitiva-oct
  (lambda (rand1 x rand2)
    (if (and (esBase-N? rand1 8) (esBase-N? rand2 8))
        (cases primitiva-oct x
          (primi-suma-oct () (suma rand1 rand2 7))
          (primi-resta-oct () (resta rand1 rand2 7))
          (primi-multi-oct () (multiplicacion rand1 rand2 7))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x))))

(define eval-primitiva-bin-oct
  (lambda (x rand)
    (if (esBase-N? rand 8) 
        (cases primitiva-oct x
          (primi-uno-mas-oct () (add1 rand 7))
          (primi-uno-menos-oct () (sub1 rand 7))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x))))

(define eval-inter-exp-oct
  (lambda (x env)
    (cases expresion-inter-oct x
      (oct-lit-inter (octal) (eval-oct-exp octal env))
      (identificafor-lit-inter-oct (identificador)
                                   (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                       (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "oct")
                                               (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))
                                       (if (or (eq? (type-of-sym env identificador) "oct")
                                               (eq? (type-of-sym env identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) env)
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-oct (rand1 primitiva-oct rand2)
                          (eval-primitiva-oct (eval-inter-exp-oct rand1 env) primitiva-oct (eval-inter-exp-oct rand2 env)))
      (binaria-oct (primitiva-bin-oct rand) (eval-primitiva-bin-oct primitiva-bin-oct (eval-inter-exp-oct rand env))))))

(define eval-oct-exp
  (lambda (x env)
    (cases oct x
      (octales (lista-oct) lista-oct))))

;*******************************************************************************************
;------------------------------ evaluaciones relacionas con b32        ----------------------------------
;*******************************************************************************************
(define eval-primitiva-b32
  (lambda (rand1 x rand2)
    (if (and (esBase-N? rand1 32) (esBase-N? rand2 32))
        (cases primitiva-b32 x
          (primi-suma-b32 () (suma rand1 rand2 31))
          (primi-resta-b32 () (resta rand1 rand2 31))
          (primi-multi-b32 () (multiplicacion rand1 rand2 31))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x))))

(define eval-primitiva-bin-b32
  (lambda (x rand)
    (if (esBase-N? rand 32) 
        (cases primitiva-b32 x
          (primi-uno-mas-b32 () (add1 rand 31))
          (primi-uno-menos-b32 () (sub1 rand 31))
          (else 0))
        (eopl:error 'empty-env "Value not in required base for ~s" x))))

(define eval-inter-exp-b32
  (lambda (x env)
    (cases expresion-inter-b32 x
      (b32-lit-inter (b32ss) (eval-b32-exp b32ss env))
      (identificafor-lit-inter-b32 (identificador)
                                   (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                       (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "b32")
                                               (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))
                                       (if (or (eq? (type-of-sym env identificador) "b32")
                                               (eq? (type-of-sym env identificador) "unknown"))
                                           (eval-expresion-inter (apply-env env identificador) env)
                                           (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (exp-aritmetica-b32 (rand1 primitiva-b32 rand2)
                          (eval-primitiva-b32 (eval-inter-exp-b32 rand1 env) primitiva-b32 (eval-inter-exp-b32 rand2 env)))
      (binaria-b32 (primitiva-bin-b32 rand) (eval-primitiva-bin-b32 primitiva-bin-b32 (eval-inter-exp-b32 rand env))))))

(define eval-b32-exp
  (lambda (x env)
    (cases b32 x
      (b32s (lista-b32) lista-b32))))

;*******************************************************************************************
;------------------------------ cadenas       ----------------------------------
;*******************************************************************************************


(define eval-inter-exp-cadena
  (lambda (x env)
    (cases expresion-inter-cadena x
      (string-lit-inter (str) (substring (symbol->string str) 1 (- (string-length (symbol->string str)) 1)))
      (identificafor-lit-inter-cadena (identificador)
                                      (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                          (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "string")
                                                  (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                              (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                              (eopl:error 'empty-env "No equal type for ~s" identificador))
                                          (if (or (eq? (type-of-sym env identificador) "string")
                                                  (eq? (type-of-sym env identificador) "unknown"))
                                              (eval-expresion-inter (apply-env env identificador) env)
                                              (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (primi-concat (str1 str2) (string-append (eval-expresion-inter str1 env) (eval-expresion-inter str2 env)))
      )))


;*******************************************************************************************
;----------------------------------- listas  ----------------------------------------
;*******************************************************************************************


(define eval-inter-exp-lista
  (lambda (x env)
    (cases expresion-inter-lista x
      (list-lit-inter (una-list) (eval-cada-elem-de-lista (eval-lista-exp una-list env) env))
      (identificafor-lit-inter-lista (identificador)
                                     (if (search-sym-in-env (quitar-ultima-extencion-de-ambiente env) identificador)
                                         (if (or (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "list")
                                                 (eq? (type-of-sym (quitar-ultima-extencion-de-ambiente env) identificador) "unknown"))
                                             (eval-expresion-inter (apply-env env identificador) (quitar-ultima-extencion-de-ambiente env))
                                             (eopl:error 'empty-env "No equal type for ~s" identificador))
                                         (if (or (eq? (type-of-sym env identificador) "list")
                                                 (eq? (type-of-sym env identificador) "unknown"))
                                             (eval-expresion-inter (apply-env env identificador) env)
                                             (eopl:error 'empty-env "No equal type for ~s" identificador))));no necesita un env
      (empty1 () empty)
      (cons1 (exp-inter1 exp-inter2) (append (list (eval-expresion-inter exp-inter1 env)) (eval-expresion-inter exp-inter2 env)));necesario tipos?
      (cdr1 (exp-inter) (cdr (eval-expresion-inter exp-inter env)))
      (append1 (exp-inter1 exp-inter2) (append (eval-expresion-inter exp-inter1 env) (eval-expresion-inter exp-inter2 env)));necesario tipos?
      )))

(define eval-lista-exp
  (lambda (x env)
    (cases lista x
      (listas (una-lista) una-lista))))

(define eval-cada-elem-de-lista
  (lambda (x env)
    (map (lambda (x) (eval-expresion-inter x env)) x)))


;*******************************************************************************************
;----------------------------------- firstElement  ----------------------------------------
;*******************************************************************************************

(define eval-firstElement
  (lambda (x env)
    (car (eval-inter-exp-lista x env))))



;*******************************************************************************************
;----------------- add1 sub1 suma resta multiplicacion hexa octa b32 esBase-N?    ---------------------
;*******************************************************************************************

;;proposito: mira si los elementos de una lista son menores a B (mira si una lista es hexa, octa o b32)
(define esBase-N?
  (lambda (x B)
    (if (eq? '() x)
        #t
        (if (< (car x) B)
            (esBase-N? (cdr x) B)
            #f))))

;;Propósito: cuando se llama, devuelve una lista vacia
(define zero
  (lambda () '()))
;(zero)
;()
;(zero)
;()

;;Propósito: checkea si el argumento es una lista vacia, si no lo es, retorna #f
(define is-zero?
  (lambda (lista)
    (null? lista)))
;(is-zero? '())
;#t
;(is-zero? '(1))
;#f

;;Proposito: halla la respresentacion en Bignum del siguiente numero en Bignum que se le pase como parámetro.
(define add1
  (lambda (lista B)
    (if (eq? lista '())
        '(1)
        (if (eq? (car lista) B)
            (if (eq? (length lista) 1)
                '(0 1);aumenta uno al final
                (append '(0) (add1 (cdr lista) B)))
            (append (list (+ 1 (car lista))) (cdr lista))))))
;(add1 '(0 0 1) 15)
;256 -> 257
;'(1 0 1)
;(add1 '(15 15) 15)
;255 -> 256
;'(0 0 1)
;(add1 '(7 8 9 3 5 3) 15)
;3488135 -> 3488136
;'(8 8 9 3 5 3)
;(add1 '(3 4 1 1) 15)
;4419 -> 4420
;'(4 4 1 1)

;;Proposito: halla la representacion en Bignum del numero que precede al numero que representa la entra en Bignum
(define sub1
  (lambda (lista B)
    (if (eq? (car lista) 0)
        (append (list B) (sub1 (cdr lista) B))
        (if (or (equal? lista (list 1)) (equal? lista (list)))
            '()
            (append (list (- (car lista) 1)) (cdr lista))))
    ))
;(sub1 '(1) 15)
;'()
;(sub1 '(0 1) 15)
;'(15)
;(sub1 '(4 4 1 1) 15)
;(3 4 1 1)
;(sub1 '(8 8 9 3 5 3) 15)
;(7 8 9 3 5 3)
;(sub1 '(0 0 1) 15)
;(15 15)
;(sub1 '(1 0 1) 15)
;(0 0 1)

;suma para Bignum datatype
;solo tengo en cuenta dos sumandos
;Proposito: tiene como entrada dos parametros que deben estar en Bignum, retorna el Bignum que representa la suma de los numeros representados en los parametros
(define suma
  (lambda (x y B)
    (if (is-zero? x)
        y
        (add1 (suma (sub1 x B) y B) B))))
;(suma '(15 15) '(0 0 1) 15)
;(15 15 1)
;(suma '(15 15) '(1) 15)
;(0 0 1)
;(suma '(15 15) '() 15)
;(15 15)

;Proposito: tiene como entrada dos parametros que deben estar en Bignum, retorna el Bignum que representa la resta de los numeros representados en los parametros. La resta siempre debe dar como resultado un Bignum que representa a un numero positivo.
(define resta
  (lambda (x y B)
    (if (is-zero? y)
        x
        (sub1 (resta  x (sub1 y B) B) B))))
;(resta '(0 0 1) '(15 15) 15)
;(1)
;(resta '(15 15) '() 15)
;(15 15)
;(resta '(15 15) '(1) 15)
;(14 15)

;Proposito: tiene como entrada dos parametros que deben estar en Bignum, retorna el Bignum que representa la multiplicacion de los numeros representados en los parametros.
(define multiplicacion
  (lambda (x y B)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (sub1 x B) y B) y B))
    ))
;(multiplicacion '(1) '(0 1) 15)
;(0 1)
;(multiplicacion '(0 1) '(1) 15)
;(0 1)
;(multiplicacion '(0 1) '(0 0 1) 15)
;(0 0 0 1)
;(multiplicacion '(0 0 1) '(0 1) 15)
;(0 0 0 1)
;*******************************************************************************************
;----------    fin add1 sub1 suma resta multiplicacion hexa octa b32 esBase-N?    ----------
;*******************************************************************************************













































































; proc let function doble = (var int num = )
; {
;    return int (int num * int 2) ; 
; }
; main(){
;      const unknwon gh = doble ( int 1 );
;      return int gh;
; }


; proc let function simpleEnv = (var int num = )
; {
;  let bool booleano = bool false; 
;  return bool not bool booleano ;
; }
; main(){
;      return simpleEnv ( int 2 );
; }


; proc let function factorial = (var int num = ){
;   if ( bool (int num == int 1) ) {
;       return int 1;
;   }else{
;       return int (int num * factorial (int (int num - int 1) ) ) ; 
;   }
;       return int 1010101;
;   }
; main(){
;      return factorial( int sub1 int 4 );
; }


; proc let function fibonacci = (var int num = ){
;   if (bool ( bool (int num == int 1) or bool (int num == int 0) ) ) {
;       if ( bool (int num == int 0) )
;       {
;          var int a = int 1;
;          set_var int a = int 0;
;          return int a;                              
;       }
;       else
;       {
;          return int 1;
;       }                                                              
;       return int -1;
;   }else
;  {
;       return int (fibonacci (int (int num - int 1)) + fibonacci (int (int num - int 2) ) ); 
;   }
;   return int -1;
;   }
; main(){
;      return fibonacci ( int sub1 int 9 );
; }


; proc let function simpleEnv = (var& int num = )
; {
;  set_var int num = int (int 13 + int 1); 
;  return int num;
; }
; main(){
;      var int g = int 1;
;      var unknown v = simpleEnv(int g);
;      return int g;
; }


; main(){
;   var int dt = int 13;
;   var& int aumentar = bool dt;
;   set_var int aumentar = int (int 13 + int 1); 
;   return vector aumentar;
; }


; main(){
;   const int dt = int 13;
;   var& int  aumentar = int dt;
;   set_var int aumentar = int (int 13 + int 1); 
;   return int aumentar;
; }


; main(){
;   var int aumentar = int 13;
;   const int numero = int -100;
;   set_var int aumentar = int (int numero + int 1);
;   IF (bool (int -98 == int aumentar))
;   {
;     set_var int aumentar = int (int 13 + int 2);
;   }
;   ELSE
;   {
;     set_var int aumentar = int 0;
;   }
;   return int aumentar;
; }


; proc const function prueba = (var int x =)
; {
;   var int aumentar = int 13;
;   const int numero = int -100;
;   set_var int aumentar = int (int numero + int 1);
;   IF (bool (int -99 == int aumentar))
;   {
;     set_var int aumentar = int (int 13 + int 2);
;     for (var int a = int 1; bool (int a < int 10); set_var int a = int (int a + int 1))
;     {
;       set_var int aumentar = int (int b + int 1);
;     }
;   }
;   ELSE
;   {
;     set_var int aumentar = int 0;
;   }
;   return int aumentar;
; }
; main(){
;   return prueba(int 1);
; }


; main(){
;   var b = int 0;
;   for (var a = int 1; bool (int a < int 10); set_var a = int (int a + int 1))
;   {
;     IF (bool ( int (int a % int 2) == int 0) )
;     {
;        print(int a);
;        print(string '         ');
;     }
;     ELSE
;     {
;     }
;     
;   }
;   return int b;
; }


; main(){
;   var int b = int 1;
;   var vector c = vector make-vector(int 10, string 'a');    
;   for (var int a = int 0; bool (int a < int 10); set_var int a = int (int a + int 1))
;   {
;     set_var vector c = vector vector-set!(vector c,int a,int a) ;
;     print(int a);
;     set_var int b = int (int b + int a);
;   }
;   print(string '     ');
;   return vector c;
; }


; main(){
;   
;   var int c = int 1;
;   var string txt = string ' ';
;   var vector b = vector make-vector(int 10, int 0);
;   for (var list a = list [int 1, int 2, int 1]; bool not bool isEmpty?(list a) ; var list a = list restElements ( list a) )
;   { 
;      set_var string txt = string concat(firstElement(a),string txt);
;      print( string txt );
;      
;     
;   
;   }
;   print(string '     '); 
;   return string txt;
; }

; main(){
;   var int b = int 1;
;   var list c = list empty( ) ;    
;   for (var int a = int 10; bool (int a <= int 10); set_var int a = int (int a + int 1))
;   {
;     set_var list c = list cons(int a, list c);
;     print(int a);
;     set_var int b = int (int b + int a);
;   }
;   print(string '     ');
;   return list c;
; }
(interpretador)
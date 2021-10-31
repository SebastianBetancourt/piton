#lang eopl
; PITÓN 3
; the only side
; 17/08/2019

(define especificacion-lexica
  ;  nombre          patrón                                                                                     modo de guardado
  '((comentario      ("#" (arbno (not #\newline)))                                                             skip)
    (identacion      ("\t")                                                                                    string)
    (sdl             ("\n")                                                                                    string)
    (espacio-blanco  (whitespace)                                                                              skip)
    (numero          (digit (arbno digit))                                                                     number)
    (numero          (digit (arbno digit) "." digit (arbno digit))                                             number)
    (hexadecimal     ("hex(" (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")) ")") string)
    (octal           ("oct(" (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8")) ")")                             string)
    (hexadecimal     ("hex(-" (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")) ")") string)
    (octal           ("oct(-" (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8")) ")")                             string)
    (identificador   ((or "_" letter) (arbno (or digit letter "_")))                                                        symbol)
    (identificador   ("super()")                                                        symbol)
    (cadena          ("\""(arbno (not #\")) "\"")                                                              string)
    (cadena          ("'"(arbno (not #\')) "'")                                                                string)))

(define gramatica-completa
  '((prog     ("\n")                                                                 nada)
    (prog     ("class" identificador herencia ":" sdl (arbno linea) sdl)          class)
    (prog     (expr sdl)                                                           un-prog-corto)
    (prog     (cdf sdl (arbno linea) sdl)                                                  un-prog)
    (herencia () object)
    (herencia ("(" identificador ")") child)
    (linea    ((arbno identacion) orden)                                           una-linea)
    (orden    (expr sdl)                                                           una-expr)
    (orden    (cdf sdl)                                                            un-cdf)
    (orden    ("return" expr sdl)                                                  un-rtrn)
    (expr     (identificador rest-id)                                              id)
    (expr      ("(-" numero ")" rest)                             nnum)
    (expr     (numero rest)                                                        num)
    (expr     (cadena rest)                                                        cad)
    (expr     (hexadecimal rest)                                                   hex)
    (expr     (octal rest)                                                         octal)
    (expr     ("True" rest)                                                        v)
    (expr     ("False" rest)                                                       f)
    (expr     (op-un rest)                                                              cion-un)
    (expr     ("[" (separated-list expr ",") "]")                                  lst)
    (rest     ()                                                                   simple)
    (rest     (dor-bin expr)                                                       cion-bin)
    (rest-id  ("." identificador rest-id)                                          obj-cion)
    (rest-id  (rest)                                                               rest-comun)
    (rest-id  ("=" expr)                                                           asign)
    (rest-id  ("(" (separated-list expr ",") ")" rest)                             eje)
    (cdf      ("if" expr ":")                                                      si)
    (cdf      ("elif" expr ":")                                                    sinosi)
    (cdf      ("else:")                                                            sino)
    (cdf      ("for" identificador "in" expr ":")                                  for)
     (cdf               ("def" identificador "(" (separated-list identificador phint ",") ")" rhint ":") def)
    (phint (":" texpr) a-phint)
    (phint () no-phint)
    (rhint ("->" texpr) a-rhint)
    (rhint () no-rhint)
    (texpr ("number") number-type)
    (texpr ("str") str-type)
    (texpr ("bool") bool-type)
    (texpr ("(" (separated-list texpr " x ") "->" texpr ")") f-type)
    (dor-bin  (">")                                                                g)
    (dor-bin  (">=")                                                               ge)
    (dor-bin  ("<")                                                                l)
    (dor-bin  ("<=")                                                               le)
    (dor-bin  ("==")                                                               e)
    (dor-bin  ("!=")                                                               d)
    (dor-bin  ("and")                                                              a)
    (dor-bin  ("or")                                                               o)
    (dor-bin  ("+")                                                                sum)
    (dor-bin  ("-")                                                                minus)
    (dor-bin  ("%")                                                                modu)
    (dor-bin  ("*")                                                                times)
    (dor-bin  ("/")                                                                div)
    (op-un    ("s(" expr ")")                                                      s)
    (op-un    ("p(" expr ")")                                                      p)
    (op-un    ("len(" expr ")")                                                    long)
    (op-un    ("range(" expr ")")                                                  rang)
    (op-un    ("not(" expr ")")                                                   no)
    (op-un    ("print(" expr ")") print)
    (programa          ()                                                                                         salto)
    (programa          (expresion)                                                                                      un-programa)
    (expresion         (numero)                                                                                         numero-literal)
    (expresion         ("class" identificador "(" identificador "):\n" subprograma)                                     declaracion-clase)
    (expresion         (hexadecimal)                                                                                    hexadecimal-literal)
    (expresion         (octal)                                                                                          octal-literal)
    (expresion         ("\"" cadena "\"")                                                                               cadena-literal)
    (expresion         ("'" cadena "'")                                                                                 cadena-literal)
    (expresion         (booleano)                                                                                       booleano-literal)
    (expresion         (identificador)                                                                                  identificador-literal)
    (expresion         (expresion operador-binario expresion)                                                           operacion-binaria)
    (expresion         (operador-unario "(" expresion ")")                                                              operacion-unaria)
    (booleano          ("True")                                                                                         verdadero)
    (booleano          ("False")                                                                                        falso)
    (expresion         ("return" expresion)                                                                             un-return)
    (expresion         ("def" identificador "(" (separated-list identificador phint ",") "):" rhint subprograma)                    definicion-procedimiento)
    (expresion         (identificador "(" (separated-list expresion ",") ")")                                           ejecucion)
    (expresion         (identificador "=" expresion)                                                                    asignacion)
    (expresion         ("if"expresion ":" subprograma (arbno "\nelif" expresion ":" subprograma) "\nelse:" subprograma) condicional)
    (expresion         ("for" identificador "in" expresion ":" subprograma)                                             ciclo-for)
    (expresion         ("[" (separated-list expresion ",") "]")                                                         lista)
    (expresion         (identificador "." identificador)                                                                objeto-identificador)
    (expresion         (identificador "." identificador "=" expresion)                                                  objeto-asignacion)
    (expresion         (identificador "." identificador "(" (separated-list expresion ",") ")")                         objeto-ejecucion)
    (subprograma       ((arbno expresion))                                                                              sub-programa-identado)
    (operador-binario  (">")                                                                                            mayor-que)
    (operador-binario  (">=")                                                                                           mayor-o-igual-que)
    (operador-binario  ("<")                                                                                            menor-que)
    (operador-binario  ("<=")                                                                                           menor-o-igual-que)
    (operador-binario  ("==")                                                                                           igual-que)
    (operador-binario  ("!=")                                                                                           diferente-que)
    (operador-binario  ("and")                                                                                          disyuncion)
    (operador-binario  ("or")                                                                                           conjuncion)
    (operador-binario  ("+")                                                                                            suma)
    (operador-binario  ("-")                                                                                            resta)
    (operador-binario  ("%")                                                                                            %)
    (operador-binario  ("*")                                                                                            multiplicacion)
    (operador-binario  ("/")                                                                                            division)
    (operador-unario   ("s")                                                                                            sucesor)
    (operador-unario   ("p")                                                                                            predecesor)
    (operador-unario   ("len")                                                                                          longitud)
    (operador-unario   ("range")                                                                                        range)
    (operador-unario   ("not")                                                                                          negacion)
    (operador-unario   ("print")                                                                                          impresion)))

;; UGLY
(define ugly
  '((prog     ("\n")                                                                 nada)
    (prog     ("class" identificador herencia ":" sdl (arbno linea) sdl)          class)
    (prog     (expr sdl)                                                           un-prog-corto)
    (prog     (cdf sdl (arbno linea) sdl)                                                  un-prog)
    (herencia () object)
    (herencia ("(" identificador ")") child)
    (linea    ((arbno identacion) orden)                                           una-linea)
    (orden    (expr sdl)                                                           una-expr)
    (orden    (cdf sdl)                                                            un-cdf)
    (orden    ("return" expr sdl)                                                  un-rtrn)
    (expr     (identificador rest-id)                                              id)
    (expr      ("(-" numero ")" rest)                             nnum)
    (expr     (numero rest)                                                        num)
    (expr     (cadena rest)                                                        cad)
    (expr     (hexadecimal rest)                                                   hex)
    (expr     (octal rest)                                                         octal)
    (expr     ("True" rest)                                                        v)
    (expr     ("False" rest)                                                       f)
    (expr     (op-un rest)                                                              cion-un)
    (expr     ("[" (separated-list expr ",") "]")                                  lst)
    (rest-id  ("." identificador rest-id)                                          obj-cion)
    (rest     ()                                                                   simple)
    (rest     (dor-bin expr)                                                       cion-bin)
    (rest-id  (rest)                                                               rest-comun)
    (rest-id  ("=" expr)                                                           asign)
    (rest-id  ("(" (separated-list expr ",") ")" rest)                                  eje)
    (cdf      ("if" expr ":")                                                      si)
    (cdf      ("elif" expr ":")                                                    sinosi)
    (cdf      ("else:")                                                            sino)
    (cdf      ("for" identificador "in" expr ":")                                  for)
     (cdf  ("def" identificador "(" (separated-list identificador phint ",") ")" rhint ":")                             def)
    (phint (":" texpr) a-phint)
    (phint () no-phint)
    (rhint ("->" texpr) a-rhint)
    (rhint () no-rhint)
    (texpr ("number") number-type)
    (texpr ("str") str-type)
    (texpr ("bool") bool-type)
    (texpr ("(" (separated-list texpr " x ") "->" texpr ")") f-type)
    (dor-bin  (">")                                                                g)
    (dor-bin  (">=")                                                               ge)
    (dor-bin  ("<")                                                                l)
    (dor-bin  ("<=")                                                               le)
    (dor-bin  ("==")                                                               e)
    (dor-bin  ("!=")                                                               d)
    (dor-bin  ("and")                                                              a)
    (dor-bin  ("or")                                                               o)
    (dor-bin  ("+")                                                                sum)
    (dor-bin  ("-")                                                                minus)
    (dor-bin  ("%")                                                                modu)
    (dor-bin  ("*")                                                                times)
    (dor-bin  ("/")                                                                div)
    (op-un    ("s(" expr ")")                                                      s)
    (op-un    ("p(" expr ")")                                                      p)
    (op-un    ("len(" expr ")")                                                    long)
    (op-un    ("range(" expr ")")                                                  rang)
    (op-un    ("not(" expr ")")                                                   no)
    (op-un    ("print(" expr ")") print)))

; sgte-subprograma :: (lista-of linea) number -> (list-of linea)
; compila todas las lineas seguidas desde la primera que tengan mayor o igual identacion que nivel-identacion, y devuelve una lista de ellas. (cadr pero de bloques)
(define sgte-subprograma
  (lambda (lineas nivel-identacion)
    (if (null? lineas) '()
    (cases linea (car lineas)
     (una-linea ( identaciones orden)
                (if (< (length identaciones) nivel-identacion)
                     '()
                     (cons (car lineas) (sgte-subprograma (cdr lineas) nivel-identacion))))))))



;retirar-subprograma :: (lista-of linea) number -> (list-of linea)
; retira todas las lineas seguidas desde la primera (inclusive) que tengan mayor o igual identacion que nivel-identacion, y devuelve una lista del resto
(define retirar-subprograma
  (lambda (lineas nivel-identacion)
    (if (null? lineas) '()
    (cases linea (car lineas)
      (una-linea ( identaciones orden)
                 (if (< (length identaciones) nivel-identacion)
                     lineas
                     (retirar-subprograma (cdr lineas) nivel-identacion)))))))



; UN PROGRAMA ES UNA SOLA EXPRESION 
; construir-expresion :: (list-of linea) -> expresion
; procesa una expresion completa
(define construir-expresion
  (lambda (lineas)
                   (cases linea (car lineas)
                     (una-linea ( identaciones la-orden)
                                (cases orden la-orden
                                  (una-expr (expr sdl)
                                                 (procesar-expr expr))
                                  (un-cdf (el-cdf sdl)
                                          (procesar-cdf el-cdf lineas (length identaciones)))
                                  (un-rtrn (expr sdl) (un-return (procesar-expr expr)))
                                  )))))

; retirar-expresion :: (list-of linea) -> (list-of linea)
; retira una expresion completa de la lista lineas y devuelve el resto
(define retirar-expresion
  (lambda (lineas)
    (if (null? lineas) '()
                   (cases linea (car lineas)
                     (una-linea ( identaciones la-orden)
                                (cases orden la-orden
                                  (una-expr (expr sdl)
                                                 (cdr lineas))
                                  (un-cdf (el-cdf sdl)
                                          (retirar-cdf lineas (length identaciones)))
                                  (un-rtrn (cdr lineas))
                                  ))))))

; map-procesar-subloques :: (list-of linea) -> (list-of expresion)
; Recibe una lista de lineas y las convierte en estructuras de nice y las agrupa en una lista (aka map)
(define map-procesar-subloques
  (lambda (lineas)
    (if (null? lineas)
        empty
    (cons (construir-expresion lineas) (map-procesar-subloques (retirar-expresion lineas))))))

; procesar-subloque :: (list-of linea) -> sub-programa-identado
; Recibe una lista de lineas que y crea un sub-programa-identado con todas las expresiones que haya después de la primera y estén identadas
(define procesar-subloque
  (lambda (lineas identacion)
    (sub-programa-identado (map-procesar-subloques (sgte-subprograma (cdr lineas) (+ 1 identacion))))))

; procesar-cdf :: cdf (list-of lineas) number -> expresion
; Toma una cdf y la convierte a la estructura correspondiente de nice. De encontrar ej. un else sin if, un elif sin if, devuelve error
(define procesar-cdf
 (lambda (el-cdf lineas identacion)
   (cases cdf el-cdf
     (si (expr)
         (let ((sinos (procesar-sinos (retirar-subprograma (cdr lineas) (+ 1 identacion)) identacion)))
          (condicional (procesar-expr expr) (procesar-subloque lineas identacion) (car sinos) (cadr sinos) (caddr sinos))))
     (for (dor able) (ciclo-for dor (procesar-expr able) (procesar-subloque lineas identacion)))
     (def (dor param phints rhint) (definicion-procedimiento dor param phints rhint (procesar-subloque lineas identacion)))
     (else "Error: else/elif sin un if precedente"))))

; procesar-sinos :: (list-of lineas) number -> (list (list-of expresion) (list-of subprograma) subprograma)
; Recibe una lista de lineas (que, en teoria, debían de tener un if en la linea anterior) y procesa todas las elif/else clauses (procesar: convertir a nice).
; devuelve una lista de: una lista de condiciones de elifs, una lista de consecuencias de elifs, una consecuencia de else
(define procesar-sinos
  (lambda (lineas identacion)
    (let loop ((lineas lineas)
               (coleccion (list empty empty (sub-programa-identado empty))))
  (if (null? lineas) coleccion
   (cases linea (car lineas)
     (una-linea ( identaciones la-orden)
      (cases orden la-orden
   (un-cdf (el-cdf sdl)
           (cases cdf el-cdf
             (si (expr) coleccion)
             (sinosi (expr) (loop (retirar-subprograma (cdr lineas) (+ 1 identacion)) (list (cons (procesar-expr expr) (car coleccion))
                                                                                                (cons (procesar-subloque lineas identacion) (cadr coleccion))
                                                                                                (caddr coleccion))))
             (sino () (list (car coleccion) (cadr coleccion) (procesar-subloque lineas identacion)))
             (else coleccion)))
    (else coleccion))))))))

; retirar-cdf :: cdf (list-of linea) number -> (list-of lineas)
; Retira todas las lineas de lineas que pertenezcan a un solo cdf y retorna el resto.
(define retirar-cdf
 (lambda (lineas identacion)
   (if (null? lineas) '()
    (cases linea (car lineas)
    (una-linea ( identaciones la-orden)
      (cases orden la-orden
   (un-cdf (el-cdf sdl)
           (cases cdf el-cdf
             (si (expr) (retirar-sinos (retirar-subprograma (cdr lineas) (+ 1 identacion)) identacion))
             (for (dor expr) (retirar-subprograma (cdr lineas) (+ 1 identacion)))
             (def (dor ids phints rhint) (retirar-subprograma (cdr lineas) (+ 1 identacion)))
             (else (eopl:error "Encontrado un elif/else sin un if precedente"))))
    (else lineas)))))))

; retirar-sinos :: list-of linea number -> list-of linea
; retira todas los else, elif y sus consecuencias de lineas (que debía tener un if en la linea anterior) y devuelve el resto
(define retirar-sinos
  (lambda (lineas identacion)
    (if (null? lineas) '()
   (cases linea (car lineas)
     (una-linea ( identaciones la-orden)
      (cases orden la-orden
   (un-cdf (el-cdf sdl)
           (cases cdf el-cdf
             (si (expr) lineas)
             (sinosi (expr) (retirar-sinos (retirar-subprograma (cdr lineas) (+ 1 identacion)) identacion))
             (sino () (retirar-subprograma (cdr lineas) (+ 1 identacion)))
             (else lineas)))
    (else lineas)))))))
    

; procesar-dor-bin ::  dor-bin -> operador-binario
; transforma un dor-bin en su operador-binario correspondiente
(define procesar-dor-bin
  (lambda (dor)
    (cases dor-bin dor
    (g () (mayor-que))
    (ge () (mayor-o-igual-que))
    (l () (menor-que))
    (le () (mayor-o-igual-que))
    (e () (igual-que))
    (d () (diferente-que))
    (a () (disyuncion))
    (o () (conjuncion))
    (sum () (suma))
    (minus () (resta))
    (modu () (%))
    (times () (multiplicacion))
    (div () (division)))))

; procesar-op-un ::  op-un -> expresion
; transforma un dor-un en su operacion-unaria correspondiente de nice
(define procesar-op-un
  (lambda (op)
    (cases op-un op
    (s (expr) (operacion-unaria (sucesor) (procesar-expr expr)))
    (p (expr) (operacion-unaria (predecesor) (procesar-expr expr)))
    (long (expr) (operacion-unaria (longitud) (procesar-expr expr)))
    (no (expr) (operacion-unaria (negacion) (procesar-expr expr)))
    (rang (expr) (operacion-unaria (range) (procesar-expr expr)))
    (print (expr) (operacion-unaria (impresion) (procesar-expr expr))))))

; procesar-expr :: expr -> expresion
; Toma una expr de ugly y devuelve una expresion de nice
(define procesar-expr
  (lambda (la-expr)
    (cases expr la-expr
      (id (dor el-rest-id) (cases rest-id el-rest-id
                            (asign (valor) (asignacion dor (procesar-expr valor)))
                            (eje (params el-rest)
                             (cases rest el-rest
                             (simple () (ejecucion dor (map procesar-expr params)))
                               (cion-bin (dor-bin expr2) (operacion-binaria (ejecucion dor (map procesar-expr params)) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
                            (rest-comun (el-rest) (cases rest el-rest
                                                  (simple () (identificador-literal dor))
                                                  (cion-bin (dor-bin expr2) (operacion-binaria (identificador-literal dor) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
                             (obj-cion (id el-rest-id)
                                       (cases rest-id el-rest-id
                                         (asign (valor) (objeto-asignacion dor id (procesar-expr valor)))
                                         (obj-cion (id2 el-rest-id) (eopl:error 'ImplementationLimitError "the fields of an object cannot return an object"))
                                         (eje (params el-rest)
                                              (cases rest el-rest
                                                (simple () (objeto-ejecucion dor id (map procesar-expr params)))
                                                (cion-bin (dor-bin expr2) (operacion-binaria (objeto-ejecucion dor id (map procesar-expr params)) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
                                         (rest-comun (el-rest) (cases rest el-rest
                                                                 (simple () (objeto-identificador dor id))
                                                                 (cion-bin (dor-bin expr2) (operacion-binaria (objeto-identificador dor id) (procesar-dor-bin dor-bin) (procesar-expr expr2))))))
                                       )
                             ))
      (nnum (el-num el-rest) (cases rest el-rest
                             (simple () (numero-literal (* -1 el-num)))
                               (cion-bin (dor-bin expr2) (operacion-binaria (numero-literal (* -1 el-num)) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (num (el-num el-rest) (cases rest el-rest
                             (simple () (numero-literal el-num))
                             (cion-bin (dor-bin expr2) (operacion-binaria (numero-literal el-num) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (cad (la-str el-rest) (cases rest el-rest
                             (simple () (cadena-literal (substring la-str 1 (- (string-length la-str) 1))))
                             (cion-bin (dor-bin expr2) (operacion-binaria (cadena-literal (substring la-str 1 (- (string-length la-str) 1))) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (hex (el-hex el-rest) (cases rest el-rest
                             (simple () (hexadecimal-literal el-hex))
                             (cion-bin (dor-bin expr2) (operacion-binaria (hexadecimal-literal el-hex) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (octal (el-octal el-rest) (cases rest el-rest
                             (simple () (octal-literal el-octal))
                             (cion-bin (dor-bin expr2) (operacion-binaria (octal-literal el-octal) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (cion-un (op-un el-rest) (cases rest el-rest
                             (simple () (procesar-op-un op-un))
                             (cion-bin (dor-bin expr2) (operacion-binaria (procesar-op-un op-un) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (lst (exprs) (lista (map procesar-expr exprs)))
      (v (el-rest) (cases rest el-rest
                             (simple () (booleano-literal (verdadero)))
                             (cion-bin (dor-bin expr2) (operacion-binaria (booleano-literal (verdadero)) (procesar-dor-bin dor-bin) (procesar-expr expr2)))))
      (f (el-rest) (cases rest el-rest
                             (simple () (booleano-literal (falso)))
                             (cion-bin (dor-bin expr2) (operacion-binaria (booleano-literal (falso)) (procesar-dor-bin dor-bin) (procesar-expr expr2))))))))
      
; procesar-programa :: prog -> programa
; Recibe un prog, y retorna el programa equivalente de nice
(define procesar-prog
  (lambda (el-prog)
   (cases prog el-prog
     (nada () (salto))
     (class (id h sdl1 lineas sdl2) (un-programa (declaracion-clase id (cases herencia h
                                                            (object () 'Object)
                                                            (child (i) i))
                                                       (sub-programa-identado (let loop ((l lineas)) (if (null? l) '() (cons (construir-expresion l) (loop (retirar-expresion l)))))))))
     (un-prog-corto (expr sdl) (un-programa (procesar-expr expr)))
     (un-prog (cdf sdl1 lineas sdl) (un-programa
                                     (construir-expresion (cons (una-linea '() (un-cdf cdf "\n")) lineas)))))))

;; NICE
; Ambientes

;La siguiente definición equivale a un define-datatype de ambiente, solo que los datos de tipo ambiente serían mutables
(define ramificar-ambiente
  (lambda (amb)
    (if (vacio? amb)
        (ambiente-vacio)
    (vector (vector-ref amb 0) (vector-ref amb 1) (ramificar-ambiente (vector-ref amb 2)))
    )))

(define ambiente-vacio
  (lambda () (make-vector 3 'vacio)))

(define extendido?
  (lambda (amb)
    (and (vector? amb) (= 3 (vector-length amb)) (symbol? (vector-ref amb 0)) (vector? (vector-ref amb 2)))))

(define vacio?
  (lambda (amb)
    (and (vector? amb) (= 3 (vector-length amb)) (equal? (vector-ref amb 0) 'vacio) (equal? (vector-ref amb 1) 'vacio) (equal? (vector-ref amb 2) 'vacio))))

(define ambiente?
 (lambda (ambiente)
   (let loop ((amb ambiente))
     (cond [(extendido? amb) (ambiente? (vector-ref amb 2))]
           [(vacio? amb) #t]
           [else #f]))))
         

(define ambiente-inicial
  (ambiente-vacio)) ; TODO: funciones y constantes default a incluir aqui

(define-datatype procedimiento procedimiento?
  (cerradura (parametros (list-of symbol?)) (cuerpo (list-of expresion?)))
  (constructor (parametros (list-of symbol?)) (cuerpo (list-of expresion?))))

(define None 'None)
(define isNone? (lambda (x) (equal? x None)))

; list-search : symbol list -> number
; Recorre una lista buscando un symbol equal? a id, y de encontrarlo, devuelve la posición (que va desde 0 hasta (length l)).
; De no encontrarlo, devuelve -1
(define list-search
  (lambda (id l)
    (let loop ((l l) (i 0))
    (cond [(null? l) -1]
          [(equal? id (car l)) i]
          [else (loop (cdr l) (+ i 1))]))))

; buscar-variable : symbol ambiente -> any
; Toma un ambiente y busca en el y en todos sus antecesores por un symbol igual a id y devuelve el valor correspondiente guardado en el vector
(define buscar-variable
  (lambda (id amb)
    (cond 
      [(extendido? amb) (let
                            ((id-amb (vector-ref amb 0))
                             (val (vector-ref amb 1))
                             (viejo (vector-ref amb 2)))
                          (if (equal? id id-amb) val (buscar-variable id viejo)))]
      [(vacio? amb) (eopl:error 'NameError "name '~s' is not defined" id)])))

; asignar-variable! : symbol any ambiente -> ambiente
; asigna valor a la variable id, buscando en el ambiente para saber si id ya esta definida. De estarlo, modifica el valor, de no estarlo, crea un nuevo ambiente y una nueva variable
(define asignar-variable!
  (lambda (id val amb)
    (cond 
      [(extendido? amb) (let
                            ((id-amb (vector-ref amb 0))
                             (viejo (vector-ref amb 2)))
                          (if (equal? id id-amb)
                              (begin (vector-set! amb 1 val) None)
                              (asignar-variable! id val viejo)
                              ))]
      [(vacio? amb) (begin
                      (vector-set! amb 0 id)
                      (vector-set! amb 1 val)
                      (vector-set! amb 2 (ambiente-vacio))
                      None)])))

; Procesamiento
; reestructurar : any -> expresion
; Recibe lo que devolvería una expresion literal después de ser evaluada, y la convierte en la expresion de antes de ser evaluada
(define reestructurar
  (lambda (datum)
    (cond
                    [(number? datum) (numero-literal datum)]
                    [(string? datum) (cond
                                       [(< (string-length datum) 5) (cadena-literal datum)]
                                       [(string=? (substring datum 0 3) "hex")
                                        (hexadecimal-literal datum)]
                                       [(string=? (substring datum 0 3) "oct")
                                        (octal-literal datum)]
                                       [else (cadena-literal datum)])]
                    [(boolean? datum) (booleano-literal (if datum (verdadero) (falso)))]
                    [(equal? datum 'True) (booleano-literal (verdadero))]
                    [(equal? datum 'False) (booleano-literal (falso))]
                    [else (eopl:error 'reestructurar "Dato no reestructuralizable: ~s" datum)])))

; destructurar : expresion -> any
; Recibe una expresion literal y le saca el contenido en bruto, con tipos de datos de scheme
(define destructurar
           (lambda (expr) (cases expresion expr
                                        (numero-literal (n) n)
                                        (hexadecimal-literal (h) h)
                                        (octal-literal (o) o)
                                        (cadena-literal (c) c)
                                        (booleano-literal (b) (cases booleano b (verdadero () #t) (falso () #f)))
                            (lista (exprs) exprs)
                                        (else (eopl:error 'destructurar "expresion no destructurizable: ~s" expr)))))

; evaluar-operacion-binaria : expresion operador-binario expresion -> any
; produce un resultado para una operacion binaria
(define evaluar-operacion-binaria
  (lambda (izq dor der amb eval)
    (let ((precedencia (list
                       (multiplicacion)
                       (division)
                       (%)
(resta)
                       (suma)
                       
                       (igual-que)
                       (diferente-que)
                       (mayor-que)
                       (mayor-o-igual-que)
                       (menor-que)
                       (menor-o-igual-que)
                       (disyuncion)
                       (conjuncion))))
      (let loop ((expr (aplanar-operacion-binaria izq dor der))
                 (dores precedencia))
        (if (null? dores) (eval (car expr) amb) (loop (procesar-un-operador expr (car dores) amb eval) (cdr dores)))))))

; aplanar-operacion-binaria :expresion operador-binario expresion -> list
; Aplana el arbol binario representado en operacion-binaria
(define aplanar-operacion-binaria
  (lambda (izq dor der)
    (cons (cases expresion izq
      (operacion-binaria (izq2 dor2 der2) (aplanar-operacion-binaria izq2 dor2 der2))
      (else izq))
    (cons dor
           (cases expresion der
             (operacion-binaria (izq3 dor3 der3) (aplanar-operacion-binaria izq3 dor3 der3))
             (else (cons der '())))))))

; procesar-un-operador : list operador-binario -> list
; Recibe una operacion-binaria ya aplanada y procesa las operaciones que correspondan al operador-binario con las dos expresiones adyacentes
(define procesar-un-operador
  (lambda (cion dor amb eval)
    (if (or (null? cion) (null? (cdr cion))) cion
        (if (equal? (cadr cion) dor)
            (procesar-un-operador (cons (procesar-operacion-binaria-primitiva (car cion) dor (caddr cion) amb eval) (cdddr cion)) dor amb eval)
            (cons (car cion) (cons (cadr cion) (procesar-un-operador (cddr cion) dor amb eval)))))))

;procesar-operacion-binaria-primitiva : expresion operador-binario expresion -> expresion
(define procesar-operacion-binaria-primitiva
  (lambda (izq-a-evaluar dor der-a-evaluar amb eval)
    (let*
        ((evaluar (lambda (x) (destructurar (basex->basey (reestructurar (eval x amb)) 10))))
         (base (lambda (x)
                 (cases expresion (reestructurar (eval x amb))
                   (numero-literal (n) 10)
                   (hexadecimal-literal (h) 16)
                   (octal-literal (o) 8)
                   (booleano-literal (b) 'booleano)
                   (cadena-literal (b) 'cadena)
                   (else (eopl:error "expresion no devuelve un valor operable")))))
         (izq (evaluar izq-a-evaluar))
         (der (evaluar der-a-evaluar)))
         (basex->basey
          (reestructurar
           (cases operador-binario dor
            (mayor-que () (> izq der))
            (mayor-o-igual-que () (>= izq der))
            (menor-que () (< izq der))
            (menor-o-igual-que () (>= izq der))
            (igual-que () (equal? izq der))
            (diferente-que () (not (equal? izq der)))
            (disyuncion () (and izq der))
            (conjuncion () (or izq der))
            (suma () (if (number? izq) (+ izq der) (string-append izq der)))
            (resta () (- izq der))
            (% () (modulo izq der))
            (multiplicacion () (* izq der))
            (division () (/ izq der))))
          (base izq-a-evaluar)))))

; construir-literal : string (or number symbol) -> expresion
; Construye un dato del tipo de n desde el string
(define construir-literal
 (lambda (lit n) ; lit debe contener no puede contener constructores (ej. "hex(") n puede ser 10, 8, 16, 'cadena o 'booleano
   (cond
     [(symbol? n) (cond [(equal? n 'cadena) (cadena-literal lit)]
                        [(and (equal? n 'booleano) (equal? "True" lit)) (verdadero)]
                        [(and (equal? n 'booleano) (equal? "False" lit)) (falso)])]
     [(= n 10) (numero-literal (string->number lit))]
     [(= n 16) (hexadecimal-literal (string-append "hex(" lit ")"))]
     [(= n 8) (octal-literal (string-append "oct(" lit ")"))]
     [else (eopl:error 'basex->basey "No existe un tipo de dato ~s" n)])))

; basex->basey : expresion number -> expresion
; Convierte el numero guardado en la expresion x a la base b. Si la expresion no es de base10, base16 o base8, devuelve la expresion x intacta
(define basex->basey
  (lambda (x b)
    
        (let ((alfabeto (string->list "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
          (letrec
              ; l : lista de chars en reversa; b integer (base) -> integner
              ((basex->base10 (lambda (l b) (if (null? l) 0 (+ (list-search (car l) alfabeto) (* b (basex->base10 (cdr l) b))))))
               ; n : integer; b integer (base) -> lista de chars en reversa
               (base10->basex (lambda (n b) (if (< n b)
                                                (list (list-ref alfabeto (modulo n b)))
                                                (cons (list-ref alfabeto (modulo n b)) (base10->basex (quotient n b) b)))))
               (pipear (lambda ()
                         (let* ((negativo? (cases expresion x
                                             (numero-literal (n) (> 0 n))
                                             (hexadecimal-literal (h) (equal? #\- (string-ref h 4)))
                                             (octal-literal (o) (equal? #\- (string-ref o 4)))
                                             (else (eopl:error 'basex->basey "Error inalcanzable"))))
                                (x (reestructurar (cases expresion x
                                                    (numero-literal (n) (abs n))
                                                    (hexadecimal-literal (h) (if negativo? (string-append "hex(" (substring h 5)) h))
                                                    (octal-literal (o) (if negativo? (string-append "oct(" (substring o 5)) o))
                                                    (else (eopl:error 'basex->basey "Error inalcanzable"))))))
                           (construir-literal (string-append (if negativo? "-" "") (list->string (reverse (base10->basex
                                                                                                           (cases expresion x
                                                                                                             (numero-literal (n) n)
                                                                                                             (hexadecimal-literal (h) (basex->base10 (cdr (reverse (cddddr (string->list h)))) 16))
                                                                                                             (octal-literal (o) (basex->base10 (cdr (reverse (cddddr (string->list o)))) 8))
                                                                                                             (else (eopl:error 'basex->basey "Error inalcanzable")))
                                                                                                           b)))) b)))))
            (cases expresion x
              (numero-literal (n) (if (and (number? n) (not (integer? n))) x (pipear)))
              (hexadecimal-literal (h) (pipear))
              (octal-literal (o) (pipear))
              (else x))))))

(define rango
  (lambda (n)
    (let loop ((i 0)) (if (= i n) '() (cons (numero-literal i) (loop (+ i 1)))))))

; evaluar-operacion-unaria : operador-unario expr 
(define evaluar-operacion-unaria
  (lambda (dor preando amb eval)
    
    (let ((ando (reestructurar (eval preando amb))))
    (cases operador-unario dor
      (sucesor () (evaluar-expresion (procesar-operacion-binaria-primitiva ando (suma) (numero-literal 1) amb evaluar-expresion) amb))
      (predecesor () (evaluar-expresion(procesar-operacion-binaria-primitiva ando (resta) (numero-literal 1) amb evaluar-expresion) amb))
      (longitud () (evaluar-expresion(reestructurar (if (string? (destructurar ando)) (string-length (destructurar ando)) (length (destructurar ando)))) amb))
      (negacion () (evaluar-expresion(reestructurar (not (destructurar ando)))amb))
      (range () (evaluar-expresion(lista  (rango (destructurar ando))) amb))
      (impresion () (begin (display (imprimir-bonito (eval ando amb))) (display "\n") None))))))

; OJO: el-ambiente ya debe estar ramificado, pues va a ser modificado
(define evaluar-proc
  (lambda (subprograma el-ambiente)
    (letrec ((evaluar-subexpresion
           (lambda (la-expresion)
             (cases expresion la-expresion
               (declaracion-clase (a b c) (eopl:error 'SyntaxError "class declaration cannot be inside function"))
               (numero-literal (n) None)
               (hexadecimal-literal (h) None)
               (octal-literal (o) None)
               (cadena-literal (c) None)
               (booleano-literal (b) None)
               (lista (lst) None)
               ;
               (identificador-literal (el-id) None)
               (operacion-binaria (izq dor der) (begin (evaluar-parametros (list izq der) el-ambiente) None))
               (operacion-unaria (dor ando) (begin (evaluar-operacion-unaria dor ando el-ambiente evaluar-subexpresion) None))
               (un-return (expr) (evaluar-expresion expr el-ambiente))
               (definicion-procedimiento (id ids phints rhint cuerpo) (asignar-variable! id (cerradura ids cuerpo) el-ambiente))
               (ejecucion (id params) (begin (ejecutar-proc id (evaluar-parametros params el-ambiente) el-ambiente) None))
               (asignacion (id val) (asignar-variable! id (evaluar-expresion val el-ambiente) el-ambiente))
               (condicional (cond1 cons1 conds conss else) (evaluar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else) el-ambiente evaluar-expresion
                                                   evaluar-proc))
               (ciclo-for (id expr body) (evaluar-for id (evaluar-expresion expr el-ambiente) body el-ambiente evaluar-expresion evaluar-proc))
               (objeto-identificador (obj id) None)
               (objeto-asignacion (obj id expr) (asignar-variable! id (evaluar-expresion expr el-ambiente) (caadr (buscar-en-ambientes id (obj->plano obj)))))
               (objeto-ejecucion (obj id params) (begin (ejecutar-metodo obj id params el-ambiente) None))))))
      (begin
        (define resultado (evaluar-subexpresion (car subprograma)))
    (if (isNone? resultado) (if (null? (cdr subprograma)) None
                                (evaluar-proc (cdr subprograma) el-ambiente)) resultado)))))

(define evaluar-parametros
  (lambda (params amb)
    (if (null? params) '()
        (cons (evaluar-expresion (car params) amb) (evaluar-parametros (cdr params) amb)))))

(define ejecutar-proc
  (lambda (id params amb)
    (let ((proc (buscar-variable id amb)))
    (if (procedimiento? proc)
    (cases procedimiento proc
      (cerradura (ids cuerpo)
                 (if (not (= (length params) (length ids)))
                           (eopl:error 'TypeError "~s() takes ~s positional argument(s) but ~s were given" id (length ids) (length params))
                     (evaluar-bloque cuerpo ids params amb)))
      (constructor (ids cuerpo)
                 (if (not (= (+ 1 (length params)) (length ids)))
                           (eopl:error 'TypeError "~s() takes ~s positional argument(s) but ~s were given" id (length ids) (+ 1 (length params)))
                     (crear-objeto id cuerpo ids params amb))))
    (eopl:error 'TypeError "'~s object is not callable" id)))))

; extiende el ambinte amb ramificandolo (es decir, crea una copia aparte del vector amb recursivamente), incorpora new-ids con new-vals y ejecuta el programa 
(define evaluar-bloque
  (lambda (cuerpo new-ids new-vals amb) ; cuerpo
    (begin
      (let ((la-rama (ramificar-ambiente amb)))
        (let loop ((parametros new-vals)
                   (identificadores new-ids))
          (if (null? parametros) '()
              (cons (asignar-variable! (car identificadores) (car parametros) la-rama)
                    (loop (cdr parametros) (cdr identificadores)))))
        (evaluar-proc cuerpo la-rama)))))

(define subprograma->list
  (lambda (spi) (cases subprograma spi (sub-programa-identado (exprs) exprs))))

(define evaluar-condicional
  (lambda (conds conss else amb eval evals)
    (if
     (null? conds) (evals else amb)
    (let ((resultado (eval (car conds) amb)))
      (cond
        [(equal? resultado 'True) (evals (car conss) amb)]
        [(equal? resultado 'False) (evaluar-condicional (cdr conds) (cdr conss) else amb eval evals)]
        [else (eopl:error 'TypeError "Condition does not return a Boolean")])))))

(define evaluar-bloque-y-mostrar
  (lambda (cuerpo amb)
    (if (null? cuerpo) None
    (if (null? (cdr cuerpo)) (evaluar-expresion (car cuerpo) amb)
        (begin (let ((resultado (evaluar-expresion (car cuerpo) amb)))
               (if (isNone? resultado) 0 (begin (display (imprimir-bonito resultado))
                                           (display"\n"))))
                                           (evaluar-bloque-y-mostrar (cdr cuerpo) amb))))))

(define evaluar-for
  (lambda (i lst-str body amb eval evals)
    (let* ((reparsed (escanear-parsear (string-append lst-str "\n")))
           (l
            (cases prog reparsed
              (un-prog-corto (e sdl)
            (cases expr e
                (lst (exprs) (map procesar-expr exprs))
                (else (eopl:error 'TypeError "~s object is not iterable" lst-str))))
            (else (eopl:error 'TypeError "~s object is not iterable" lst-str)))))
    (let loop ((resto l))
        (if (null? (cdr resto))
            (begin
              (asignar-variable! i (eval (car resto) amb) amb)
              (evals (subprograma->list body) amb))
          (begin
            (asignar-variable! i (eval (car resto) amb) amb)
            (let ((resultado (evals (subprograma->list body) amb))) (if (isNone? resultado) 0 (begin (display resultado) (display "\n"))))
            (loop (cdr resto))))))))

(define imprimir-lista
  (lambda (l)
    (if (null? l) "[]"
    (string-append "[" (car l)
                   (let loop ((resto (cdr l))) (if (null? resto) "" (string-append "," (car resto) (loop (cdr resto)))))
                   "]"))))


;; TIPOS
(define-datatype tipo tipo?
  (NoneType)
  (tatomico (nombre symbol?))
  (tmaybe (tipos (list-of tipo?)))
  (tfuncion (args (list-of tipo?)) (resultado tipo?))
  (tvariable (serial integer?) (caja vector?)))

(define NoneType?
  (lambda (tv)
    (if (tipo? tv)
    (cases tipo tv
      (NoneType () #t)
      (else #f)) #f)))

(define tatomico?
  (lambda (tv)
    (if (tipo? tv)
    (cases tipo tv
      (tatomico (s) #t)
      (else #f)) #f)))

(define tmaybe?
  (lambda (tv)
    (if (tipo? tv)
    (cases tipo tv
      (tmaybe (l) #t)
      (else #f)) #f)))

(define tfuncion?
  (lambda (tv)
    (if (tipo? tv)
    (cases tipo tv
      (tfuncion (s v) #t)
      (else #f)) #f)))

(define tvariable?
  (lambda (tv)
    (if (tipo? tv)
    (cases tipo tv
      (tvariable (s v) #t)
      (else #f)) #f)))

(define tatomico->symbol
  (lambda (tv)
    (cases tipo tv
      (tatomico (s) s)
      (else (eopl:error 'tatomico->symbol "~s no es un tipo atomico" tv)))))

(define tfuncion->tparams
  (lambda (tv)
    (cases tipo tv
      (tfuncion (s v) s)
      (else (eopl:error 'tfuncion->tparams "~s no es una tipo funcion" tv)))))

(define tfuncion->tresult
  (lambda (tv)
    (cases tipo tv
      (tfuncion (s v) v)
      (else (eopl:error 'tfuncion->tresult "~s no es una tipo funcion" tv)))))

(define tvariable->valor
  (lambda (tv)
    (cases tipo tv
      (tvariable (s v) (vector-ref v 0))
      (else (eopl:error 'variable-valor "~s no es una variable de tipo" tv)))))

(define tramificar-amb
  (lambda (tamb)
    (if (tvacio? tamb)
        (tambiente-vacio)
    (vector (vector-ref tamb 0) (vector-ref tamb 1) (tramificar-amb (vector-ref tamb 2))))))

; 1: symbol? (id)
; 2: tipo?
; 3: tamb

(define tambiente-vacio
  (lambda () (make-vector 3 'tvacio)))

(define textendido?
  (lambda (tamb)
    (and (vector? tamb) (= 3 (vector-length tamb)) (symbol? (vector-ref tamb 0)) (tipo? (vector-ref tamb 1)) (vector? (vector-ref tamb 2)))))

(define tvacio?
  (lambda (tamb)
    (and (vector? tamb) (= 3 (vector-length tamb)) (equal? (vector-ref tamb 0) 'tvacio) (equal? (vector-ref tamb 1) 'tvacio) (equal? (vector-ref tamb 2) 'tvacio))))

(define tambiente?
 (lambda (tambiente)
   (let loop ((tamb tambiente))
     (cond [(textendido? tamb) (ambiente? (vector-ref tamb 2))]
           [(tvacio? tamb) #t]
           [else #f]))))

(define tambiente-inicial
  (tambiente-vacio)) 

; buscar-tvariable : symbol tambiente -> tipo
; Toma un tambiente y busca en el y en todos sus antecesores por un symbol igual a id y devuelve el tipo correspondiente guardado en el vector
(define tbuscar-variable
  (lambda (id tamb)
    (cond 
      [(textendido? tamb) (let
                            ((id-tamb (vector-ref tamb 0))
                             (tipo (vector-ref tamb 1))
                             (viejo (vector-ref tamb 2)))
                          (if (equal? id id-tamb) tipo (tbuscar-variable id viejo)))]
      [(tvacio? tamb) (eopl:error 'NameError "type of '~s' is not defined" id)])))

; tasignar-variable! : symbol tipo tambiente -> None
; asigna rpio a la tvariable id, buscando en el ambiente para saber si id ya esta definida. De estarlo, modifica el tipo, de no estarlo, crea un nuevo ambiente y una nueva variable
(define tasignar-variable!
  (lambda (id tipo tamb)
    (cond 
      [(textendido? tamb) (let
                            ((id-tamb (vector-ref tamb 0))
                             (viejo (vector-ref tamb 2)))
                          (if (equal? id id-tamb)
                              (begin (vector-set! tamb 1 tipo) (NoneType))
                              (tasignar-variable! id tipo viejo)
                              ))]
      [(tvacio? tamb) (begin
                      (vector-set! tamb 0 id)
                      (vector-set! tamb 1 tipo)
                      (vector-set! tamb 2 (tambiente-vacio))
                      (NoneType))])))
(define tnumber
  (tatomico 'number))
(define tbool
  (tatomico 'bool))
(define tstr
  (tatomico 'str))
(define tlist
  (tatomico 'list))

; aka expand-type-expression: <type-exp> -> <type>
; determina el tipo denotado por cada expresión de tipo
(define texpr->tipo 
  (lambda (e)
    (if (and (tvariable? e) (tvariable-vacia? e)) e
    (cases texpr e
      (number-type () tnumber)
      (str-type () tstr)
      (bool-type () tbool)
      (f-type (tps tr) (tfuncion (map texpr->tipo tps) (texpr->tipo tr)))))))

(define hint->texpr
  (lambda (h)
    (if (phint? h)
        (cases phint h
          (a-phint (t) t)
          (no-phint () (nueva-tvariable)))
        (cases rhint h
          (a-rhint (t) t)
          (no-rhint () (nueva-tvariable))))))

(define nueva-tvariable
  (let ((serial 0))
    (lambda ()
      (begin
        (set! serial (+ 1 serial))
        (tvariable serial (vector '()))))))

(define tvariable-vacia?
  (lambda (tv)
    (null? (tvariable->valor tv))))

(define revisar-ocurrencia-tvariable!
  (lambda (tvar tknown expr)
    (let loop ((t tknown))
      (cases tipo tknown
        (NoneType () #t)
        (tatomico (n) #t)
        (tmaybe (l) (if (list? (memv tvar l))
                        (write "TypeWarning: couldn't infer: ~s maybe occurs in type ~s at ~s"
                                   (timprimir-bonito tvar)
                                   (timprimir-bonito tknown)
                                   expr) #t))
        (tfuncion (tparams tr)
                  (begin
                    (map loop tparams)
                    (loop tr)))
        (tvariable (serial caja)
                   (if (eqv? (vector-ref caja 0) tvar)
                       (eopl:error 'TypeError "couldn't infer: ~s occurs in type ~s at ~s"
                                   (timprimir-bonito tvar)
                                   (timprimir-bonito t)
                                   expr)
                       #t))))))

(define comparar-tvariable!
  (lambda (tvar t expr)
    (if (tvariable-vacia? tvar)
        (begin
          (revisar-ocurrencia-tvariable! tvar t expr)
          (cases tipo tvar
            (tvariable (s v) (vector-set! v 0 t)) ; la inferencia occuriendo frente a tus ojos
            (else (eopl:error "error inalcanzable"))))
        (comparar-tipos! (tvariable->valor tvar) t expr))))

 (define timprimir-bonito
  (lambda (ty)
    (cases tipo ty
      (NoneType () '<NoneType>)
      (tmaybe (l) (begin
                    (display
                     (string-append "<maybe" (apply string-append (map (lambda (x) (string-append " " (symbol->string (timprimir-bonito x)))) l)))
                     '>)))
      (tatomico (n) (string->symbol (string-append "<" (symbol->string n) ">")))
      (tfuncion (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (timprimir-bonito result-type))))
      (tvariable (serial-number container) ;;; NUEVO
        (if (tvariable-vacia? ty)
            (string->symbol (string-append "<" (number->string serial-number) "?>"))
          (timprimir-bonito (tvariable->valor ty)))))))
    
(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (timprimir-bonito (car types)))
            (cons
             (timprimir-bonito (car types))
             (cons 'x
                   (arg-types-to-external-form (cdr types))))))))


; ENVIAR PRIMERO LA FUNCION YA ESTABLECIDA 
(define comparar-tipos!
  (lambda (t1 t2 expr)
    (cond
      [(eqv? t1 t2) #t]
      [(tvariable? t1) (comparar-tvariable! t1 t2 expr)]
      [(tvariable? t2) (comparar-tvariable! t2 t1 expr)]
      [(and (tatomico? t1) (tatomico? t2)) (if (eqv? (tatomico->symbol t1) (tatomico->symbol t2)) #t (error-tipos t1 t2 expr))]
      [(and (tfuncion? t1) (tfuncion? t2))
       (let
           ((tparams1 (tfuncion->tparams t1))
            (tparams2 (tfuncion->tparams t2))
            (tr1 (tfuncion->tresult t1))
            (tr2 (tfuncion->tresult t2)))
         (if (not (= (length tparams1) (length tparams2)))
             (eopl:error 'TypeError "a function in ~s takes ~s argument but ~s were given"
                         expr
                         (length tparams1)
                         (length tparams2))
             (begin
               (for-each
                (lambda (x y) (comparar-tipos! x y expr)) tparams1 tparams2)
               (comparar-tipos! tr1 tr2 expr))))]
      [else (error-tipos t1 t2 expr)])))

(define error-tipos
  (lambda (t1 t2 expr)
    (eopl:error 'TypeError "type mismatch: '~s' and '~s' in ~s"
                (timprimir-bonito t1)
                (timprimir-bonito t2)
                expr)))

      
(define tipar-dor-bin
  (lambda (dor izq)
         (cases operador-binario dor
            (mayor-que () (tfuncion (list tnumber tnumber) tbool))
            (mayor-o-igual-que () (tfuncion (list tnumber tnumber) tbool))
            (menor-que () (tfuncion (list tnumber tnumber) tbool))
            (menor-o-igual-que () (tfuncion (list tnumber tnumber) tbool))
            (igual-que () (cond 
                            [(eqv? izq tnumber) (tfuncion (list tnumber tnumber) tbool)]
                            [(eqv? izq tstr) (tfuncion (list tstr tstr) tbool)]
                            [else (error-tipos izq tnumber izq)])) ;!
            (diferente-que () (tfuncion (list tbool tbool) tbool))
            (disyuncion () (tfuncion (list tbool tbool) tbool))
            (conjuncion () (tfuncion (list tbool tbool) tbool))
            (suma () (cond 
                            [(eqv? izq tnumber) (tfuncion (list tnumber tnumber) tnumber)]
                            [(eqv? izq tstr) (tfuncion (list tstr tstr) tstr)]
                            [else (error-tipos izq tnumber izq)])) ;!
            (resta () (tfuncion (list tnumber tnumber) tnumber))
            (% () (tfuncion (list tnumber tnumber) tnumber))
            (multiplicacion () (tfuncion (list tnumber tnumber) tnumber))
            (division () (tfuncion (list tnumber tnumber) tnumber)))))

(define tipar-dor-un
  (lambda (dor ando)
    (cases operador-unario dor
      (sucesor () (tfuncion (list tnumber) tnumber))
      (predecesor () (tfuncion (list tnumber) tnumber))
      (longitud () (cond 
                            [(eqv? ando tlist) (tfuncion (list tlist) tnumber)]
                            [(eqv? ando tstr) (tfuncion (list tstr) tnumber)]
                            [else (error-tipos ando tlist ando)])) ;!
      (negacion () (tfuncion (list tbool) tbool))
      (range () (tfuncion (list tnumber) tlist))
      (impresion () (tfuncion (list ando) NoneType)))))

(define tipar-andos
  (lambda (l tamb) (map (lambda (x) (tipar-expresion x tamb)) l)))

(define tipar-operacion
  (lambda (tdor tandos e)
    (let ((tresultado (nueva-tvariable)))
      (begin
        (comparar-tipos!
         tdor
         (tfuncion tandos tresultado) e)
        tresultado))))

; OJO: el-ambiente ya debe estar ramificado, pues va a ser modificado
(define tipar-bloque
  (lambda (subprograma tamb)
    (letrec ((tipar-subexpresion
           (lambda (expr)
             (cases expresion expr
               (numero-literal (n) (NoneType))
               (hexadecimal-literal (h) (NoneType))
               (octal-literal (o) (NoneType))
               (cadena-literal (c) (NoneType))
               (booleano-literal (b) (NoneType))
               (lista (lst) (NoneType))
               ;
               (identificador-literal (el-id) (NoneType))
               (operacion-binaria (izq dor der) (begin (tipar-andos (list izq der) tamb) (NoneType)))
               (operacion-unaria (dor ando) (begin (tipar-subexpresion ando tamb) (NoneType)))
               (un-return (expr) (tipar-expresion expr tamb))
               (definicion-procedimiento (id ids phints rhint cuerpo)
                 (begin
                 (tasignar-variable! id (tipar-proc id ids (map (lambda (x) (texpr->tipo (hint->texpr x))) phints) (texpr->tipo (hint->texpr rhint)) cuerpo tamb) tamb)
                 (NoneType)))
               (ejecucion (id params) (begin (tipar-operacion (tbuscar-variable id tamb) (tipar-andos params)) (NoneType)))
               (asignacion (id val) (begin (tasignar-variable! id (tipar-expresion val tamb) tamb) (NoneType)))
               (condicional (cond1 cons1 conds conss else) (tipar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else) tamb
                                                   tipar-bloque))
               (ciclo-for (id expr body) (tipar-bloque body tamb))
               (else "todo")))))
      (begin
        (define resultado (tipar-subexpresion (car subprograma)))
    (if (NoneType? resultado) (if (null? (cdr subprograma)) (NoneType)
                                (tipar-bloque (cdr subprograma) tamb)) resultado)))))


; extiende el ambinte amb ramificandolo (es decir, crea una copia aparte del vector amb recursivamente), incorpora new-ids con new-vals y ejecuta el programa con eval 
(define tipar-proc
  (lambda (id ids tparams tr cuerpo tamb)
    (begin
      (let ((la-trama (tramificar-amb tamb)))
        (let loop ((tparametros tparams)
                   (tidentificadores ids))
          (if (null? tparametros) '()
              (cons (tasignar-variable! (car tidentificadores) (car tparametros) la-trama)
                    (loop (cdr tparametros) (cdr tidentificadores)))))
        (comparar-tipos! (tfuncion tparams (tipar-bloque cuerpo la-trama)) (tfuncion tparams tr) id)
        (tfuncion tparams tr)))))

(define tipar-condicional
  (lambda (conds conss else tamb eval)
     ; sames?: retorna -1 si todos son iguales eqv?, si no, retorna la posicion del primero diferente   
    (let* ((sames? (lambda (ls) (let loop ((n 2) (l ls)) (cond
                               [(null? (cdr l)) -1]
                               [(eqv? (car l) (cadr l)) (loop (+ n 1) (cdr l))]
                               [else n]))))
           (tconds (map (lambda (x) (tipar-expresion x tamb)) conds))
           (tconss (map (lambda (x) (eval x tamb)) (append conss (list else))))
          (same-conds? (sames? tconds))
          (same-conss? (sames? tconss)))
      (if (eqv? (car tconds) tbool)
          (if (= -1 same-conds?)
              (if (= -1 same-conss?) (car tconss) (tmaybe tconss))
          (eopl:error 'TypeError "condition ~s does not return a Boolean" same-conds?))
          (eopl:error 'TypeError "condition 1 does not return a Boolean")))))

(define tipar-bloque-y-mostrar
  (lambda (cuerpo tamb)
    (if (null? (cdr cuerpo)) (tipar-expresion (car cuerpo) tamb)
        (begin (display (tipar-expresion (car cuerpo) tamb))
                                           (display"\n")
                                           (tipar-bloque-y-mostrar (cdr cuerpo) tamb)))))

(define tipar-expresion
  (lambda (e tamb)
    (cases expresion e
      (numero-literal (n) tnumber)
      (hexadecimal-literal (h) tnumber)
      (octal-literal (o) tnumber)
      (cadena-literal (c) tstr)
      (booleano-literal (b) tbool)
      ; 
      (identificador-literal (el-id) (tbuscar-variable el-id tamb))
      (lista (exprs) tlist)
      (operacion-binaria (izq dor der) (tipar-operacion (tipar-dor-bin dor (tipar-expresion izq tamb)) (tipar-andos (list izq der) tamb) e))
      (operacion-unaria (dor ando) (tipar-operacion (tipar-dor-un dor (tipar-expresion ando tamb)) (tipar-andos (list ando) tamb) e))
      (un-return (expr) (eopl:error 'SyntaxError "'return' outside function"))
      (definicion-procedimiento (id ids phints rhint cuerpo)
        (tasignar-variable! id (tipar-proc id ids (map (lambda (x) (texpr->tipo (hint->texpr x))) phints)
                                           (texpr->tipo (hint->texpr rhint)) (subprograma->list cuerpo) tamb) tamb))
      (ejecucion (id params) (tipar-operacion (tbuscar-variable id tamb) (tipar-andos params)))
      (asignacion (id val) (tasignar-variable! id (tipar-expresion val tamb) tamb))
      (condicional (cond1 cons1 conds conss else) (tipar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else) tamb
                                                   tipar-bloque-y-mostrar))
      (ciclo-for (id expr body) (begin
                                  (tasignar-variable! id (nueva-tvariable) tamb)
                                  (tipar-bloque-y-mostrar (subprograma->list body) tamb)))
      (else "todo"))))
        
(define tipar-programa
  (lambda (p)
    (cases programa p
      (salto () (NoneType))
      (un-programa (la-expresion) (tipar-expresion la-expresion tambiente-inicial)))))

;; OBJETOS 

(define-datatype objeto objeto?
  (obj (clase symbol?) (plano (list-of ambiente?))))

(define obj->plano
  (lambda (o)
    (cases objeto o
      (obj (c p) p))))

(define-datatype clase clase?
  (clase-adan)
  (clase-hijo (id symbol?) (cuerpo-clase (list-of expresion?)) (papa symbol?)))

(define clase->id
  (lambda (c)
    (cases clase c
      (clase-adan () 'Object)
      (clase-hijo (id c p) id))))

(define clase->cuerpo-clase
  (lambda (c)
    (cases clase c
      (clase-adan () '())
      (clase-hijo (id c p) c))))

(define clase->papa
  (lambda (c)
    (cases clase c
      (clase-adan () 'Object)
      (clase-hijo (id c p) p))))

(define clases
  (list (clase-adan)))

(define agregar-clase!
  (lambda (c)
    (if (clase? c)
        (begin (set! clases (cons c clases)) None)
        (eopl:error "no es un clase"))))

(define buscar-clase (lambda (id-clase)
                       (let loop ((cs clases))
                         (if (null? cs) (eopl:error 'NameError "class '~s' is not defined" id-clase)
                             (cases clase (car cs)
                               (clase-adan () (if (eqv? id-clase 'Object) (car cs) (loop (cdr cs))))
                               (clase-hijo (id c p) (if (eqv? id-clase id) (car cs) (loop (cdr cs)))))))))

(define declarar-clase
  (lambda (id papa subprograma amb)
    (begin
      (define clase (clase-hijo id (subprograma->list subprograma) papa))
      (define tiene-init? #t)
      (define init (let loop ((es (subprograma->list subprograma)))
                     (if (null? es)
                         (begin (set! tiene-init? #f) (buscar-variable papa amb))
                         (cases expresion (car es)
                           (definicion-procedimiento (id ids phints rhint cuerpo)
                             (if (eqv? '__init__ id) (begin (set! tiene-init? #t) (list ids cuerpo)) 
                                 (loop (cdr es)))) (else (loop (cdr es)))))))
      (agregar-clase! clase) ; agregar clase
      (if tiene-init?
          (asignar-variable! id (constructor (car init) (subprograma->list (cadr init))) amb) ; guardar constructor
          (asignar-variable! id init amb)
          ))))

(define crear-objeto
  (lambda (id-clase cuerpo-init ids-init params amb)
    (begin
      (define evaluar-bloque
        (lambda (cuerpo amb)
          (if (null? (cdr cuerpo)) (begin (evaluar-expresion (car cuerpo) amb) amb)
              (begin (evaluar-expresion (car cuerpo) amb)
                     (evaluar-bloque (cdr cuerpo) amb)))))
      (define plano (let loop ((c (buscar-clase id-clase)))
        (cases clase c
          (clase-adan () (cons (ambiente-vacio) '()))
          (clase-hijo (id cuerpo-clase papa) (cons (evaluar-bloque cuerpo-clase (ambiente-vacio)) (loop (buscar-clase papa)))))))
      (define objeto (obj id-clase plano))
      (ejecutar-metodo objeto '__init__ params amb)
      objeto)))

(define buscar-en-ambientes ; car [el elemento encontrado] cadr [los ambientes restantes] caadr [el amb donde se encontro]
  (lambda (id ambientes lanzar-error?)
    (let ((buscar (lambda (amb) (let loop ((a amb)) (cond [(extendido? a) (let
                                                                              ((id-amb (vector-ref a 0))
                                                                               (val (vector-ref a 1))
                                                                               (viejo (vector-ref a 2)))
                                                                            (if (equal? id id-amb) val (loop viejo)))]
                                                          [(vacio? a) -1])))))
      (let loop ((ambs ambientes)) (cond
                                     [(null? ambs) (if lanzar-error? (eopl:error 'NameError "name '~s' is not defined" id) (list '() ambientes))]
                                     [(and (number? (buscar (car ambs))) (= -1 (buscar (car ambs)))) (loop (cdr ambs))]
                                     [else (list (buscar (car ambs)) ambs)])))))

(define buscar-en-objeto
  (lambda (id obj lanzar-error?)
    (buscar-en-ambientes id (obj->plano obj) lanzar-error?))) ; car [el elemento encontrado] cadr [los ambientes restantes] caadr [el amb donde se encontro]

(define ejecutar-metodo
  (lambda (objeto id params amb)
      (begin
        (define cabeza (ambiente-vacio)) ; ambiente de ejecucion
        (define metodo (cases procedimiento (car (buscar-en-ambientes id (obj->plano objeto) #t))
                         (cerradura (param cuerpo) (list param cuerpo))
                         (constructor (p c) (eopl:error 'NameError "~s is not a method" id))))
        
        (asignar-variable! 'self objeto cabeza) ; crear self
        (asignar-variable!  '|super()| (obj '|super()| (cadr (buscar-en-ambientes id (cdr (obj->plano objeto)) #f))) cabeza) ; crear super

        (if (= (+ 1 (length params)) (length (car metodo)) ) 1
            (eopl:error 'TypeError "~s() takes ~s positional argument(s) but ~s were given" id (length (car metodo)) (+ 1 (length params)))) ; asignar parametros
        (let loop ((p params) (i (cdar metodo))) (if (null? p) 1 (cons (asignar-variable! (car i) (car p) cabeza) (loop (cdr p) (cdr i))))) ; asignar parametros
        
        (define evaluar-parametros
          (lambda (params)
            (if (null? params) '()
                (cons (evaluar-subexpresion (car params) amb) (evaluar-parametros (cdr params))))))
        
        (define evaluar-subexpresion
           (lambda (expr ambi)
             (cases expresion expr
               (declaracion-clase (a b c) (eopl:error 'SyntaxError "class declaration cannot be inside function"))
               (numero-literal (n) n)
               (hexadecimal-literal (h) h)
               (octal-literal (o) o)
               (cadena-literal (c) c)
               (booleano-literal (b) b)
               (lista (lst) lst)
               ;
               (identificador-literal (el-id) (car (buscar-en-ambientes el-id (list cabeza (obj->plano objeto) ambi) #t))) ; buscar en cabeza, objeto, papas, y ambiente
               (operacion-binaria (izq dor der) (evaluar-operacion-binaria izq dor der ambi evaluar-subexpresion))
               (operacion-unaria (dor ando) (evaluar-operacion-unaria dor ando ambi evaluar-subexpresion))
               (un-return (expr) (evaluar-subexpresion expr ambi))
               (definicion-procedimiento (id ids phints rhint cuerpo) (asignar-variable! id (cerradura ids (subprograma->list cuerpo)) cabeza))
               (ejecucion (id params) (begin (ejecutar-proc id (evaluar-parametros params) amb) None)) 
               (asignacion (id val) (asignar-variable! id (evaluar-subexpresion val ambi) cabeza))
               (condicional (cond1 cons1 conds conss else) (evaluar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else) amb evaluar-subexpresion evaluar-cuerpo))
               (ciclo-for (id expr body) (evaluar-for id (evaluar-subexpresion expr ambi) body cabeza evaluar-subexpresion evaluar-cuerpo))
               (objeto-identificador (o id) (car (buscar-en-objeto id (car (buscar-en-ambientes o (list cabeza (obj->plano objeto) ambi) #t)) #t)))
               (objeto-asignacion (o id expr) (asignar-variable! id (evaluar-subexpresion expr ambi)
                                                                 (caadr (buscar-en-objeto id (car (buscar-en-ambientes o (list cabeza (obj->plano objeto) ambi) #t)) #f))))
               (objeto-ejecucion (o id params) (ejecutar-metodo (car (buscar-en-ambientes o (list cabeza (obj->plano objeto) ambi) #t)) id (evaluar-parametros params) amb)))))
        
        (define evaluar-cuerpo
          (lambda (l amb) (if (isNone? (ignorar-subexpresion (car l) amb))
                                                    (if (null? (cdr l)) None
                                                        (evaluar-cuerpo (cdr l) amb)) (ignorar-subexpresion (car l) amb))))
        
        (define ignorar-subexpresion
           (lambda (expr ambi)
             (cases expresion expr
               (declaracion-clase (a b c) (eopl:error 'SyntaxError "class declaration cannot be inside function"))
               (numero-literal (n) None)
               (hexadecimal-literal (h) None)
               (octal-literal (o) None)
               (cadena-literal (c) None)
               (booleano-literal (b) None)
               (lista (lst) None)
               ;
               (identificador-literal (el-id) None)
               (operacion-binaria (izq dor der) (begin (evaluar-parametros (list izq der)) None))
               (operacion-unaria (dor ando) (begin (evaluar-operacion-unaria dor ando ambi evaluar-subexpresion) None))
               (un-return (expr) (evaluar-subexpresion expr ambi))
               (definicion-procedimiento (id ids phints rhint cuerpo) (asignar-variable! id (cerradura ids (subprograma->list cuerpo)) cabeza))
               (ejecucion (id params) (begin (ejecutar-proc id (evaluar-parametros params) amb) None))
               (asignacion (id val) (asignar-variable! id (evaluar-subexpresion val ambi) cabeza))
               (condicional (cond1 cons1 conds conss else) (evaluar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else)) amb evaluar-subexpresion evaluar-cuerpo)
               (ciclo-for (id expr body) (evaluar-for id (evaluar-subexpresion expr ambi) body cabeza evaluar-subexpresion evaluar-cuerpo))
               (objeto-identificador (obj id) None)
               (objeto-asignacion (o id expr)
                                  (asignar-variable! id (evaluar-subexpresion expr ambi)
                                                     (caadr (buscar-en-objeto id (car (buscar-en-ambientes o (list cabeza (obj->plano objeto) ambi) #t)) #f))))
               (objeto-ejecucion (o id params) (begin (ejecutar-metodo (car (buscar-en-ambientes o (list cabeza (obj->plano objeto) ambi) #t)) id (evaluar-parametros params) ambi) None)))))
        (evaluar-cuerpo (cadr metodo) amb))))

   

(define evaluar-expresion
  (lambda (la-expresion el-ambiente)
     (cases expresion la-expresion
      ; Literales: Lo unico que hay que hacer es devolverlos
      (declaracion-clase (id herencia subprograma) (declarar-clase id herencia subprograma el-ambiente))
      (numero-literal (n) n)
      (hexadecimal-literal (h) h)
      (octal-literal (o) o)
      (cadena-literal (c) c)
      (booleano-literal (b) (cases booleano b (verdadero () 'True) (falso () 'False)))
      ;objetos
       (objeto-identificador (obj id) (car (buscar-en-objeto id (buscar-variable obj el-ambiente) #t)))
       (objeto-asignacion (obj id expr) (asignar-variable! id (evaluar-expresion expr el-ambiente) (caadr (buscar-en-objeto id (buscar-variable obj el-ambiente) #f))))
       (objeto-ejecucion (obj id params) (ejecutar-metodo (buscar-variable obj el-ambiente) id (evaluar-parametros params el-ambiente) el-ambiente))
       ;
      (identificador-literal (el-id) (buscar-variable el-id el-ambiente))
      (lista (exprs) (imprimir-lista (map (lambda (x) (any->string (evaluar-expresion x el-ambiente))) exprs)))
      (operacion-binaria (izq dor der) (evaluar-operacion-binaria izq dor der el-ambiente evaluar-expresion))
      (operacion-unaria (dor ando) (evaluar-operacion-unaria dor ando el-ambiente evaluar-expresion))
      (un-return (expr) (eopl:error 'SyntaxError "'return' outside function"))
      (definicion-procedimiento (id ids phints rhint cuerpo) (asignar-variable! id (cerradura ids (subprograma->list cuerpo)) el-ambiente))
      (ejecucion (id params) (ejecutar-proc id (evaluar-parametros params el-ambiente) el-ambiente))
      (asignacion (id val) (asignar-variable! id (evaluar-expresion val el-ambiente) el-ambiente))
      (condicional (cond1 cons1 conds conss else) (evaluar-condicional
                                                   (cons cond1 conds)
                                                   (cons (subprograma->list cons1) (map subprograma->list conss))
                                                   (subprograma->list else) el-ambiente evaluar-expresion
                                                   evaluar-bloque-y-mostrar))
      (ciclo-for (id expr body) (evaluar-for id (evaluar-expresion expr el-ambiente) body el-ambiente evaluar-expresion evaluar-bloque-y-mostrar)))))

(define evaluar-programa
  (lambda (el-programa)
    (cases programa el-programa
      (salto () '...)
      (un-programa (la-expresion) (imprimir-bonito (evaluar-expresion la-expresion ambiente-inicial))))))

(define any->string
  (lambda (d)
    (cond [(number? d) (number->string d)]
          [(symbol? d) (symbol->string d)]
          [(string? d) (string-append "'" d "'")])))

(define imprimir-bonito
  (lambda (d)
    (cond
       [(isNone? d) '...]
       [(number? d) d]
       [(symbol? d) d]
       [(string? d) (cond
                      [(equal? (string-ref d 0) #\[) (begin (display "<list ") (display d) '>)]
                      [(equal? (substring d 0 4) "oct(") (begin (display "<octal ") (display (substring d 4 (- (string-length d) 1))) '>)]
                      [(equal? (substring d 0 4) "hex(") (begin (display "<hexadecimal ") (display (substring d 4 (- (string-length d) 1))) '>)]
                      [else d])]
       [else d])))

;; SLLGEN

(define read-eval-loop
  (sllgen:make-rep-loop ">>> "
     (lambda (ugly) (evaluar-programa (procesar-prog ugly)))
    (sllgen:make-stream-parser 
      especificacion-lexica
      ugly)))

(sllgen:make-define-datatypes especificacion-lexica gramatica-completa)

(define piton3
  (lambda () (begin
    (display "Pitón 3.3.1 (default, ")
    (display "Aug 28 2019, 12:18:39) \n[DrRacket 6.7 3m] on eopl\nType \"help\", \"copyright\", \"credits\" or \"license\" for more information.\n")
    (read-eval-loop))))

(define iugly
  (sllgen:make-rep-loop "ugly> "
     (lambda (ugly) (procesar-prog ugly))
    (sllgen:make-stream-parser 
      especificacion-lexica
      ugly)))

(define tipos
  (sllgen:make-rep-loop "tipo> "
     (lambda (ugly) (timprimir-bonito (tipar-programa (procesar-prog ugly))))
    (sllgen:make-stream-parser 
      especificacion-lexica
      ugly)))

(define escanear-parsear
  (sllgen:make-string-parser especificacion-lexica ugly))

(piton3)
#lang racket

; Obtener el ultimo elementoi de una lista
;Domino: lista
;Recorrdo: Elemento
;Recursion de cola -> Sin elementos en espera

(define getLast (lambda (lista)
                  (if (null? (cdr lista))
                      ( car lista)
                      (getLast (cdr lista))
                      )))

; Unifica 2 listas
;Dominio: Al menos 2 listas
;Recorrido: Lista
; OJOS cons solo funciona con un numero y una lista, la deja en el primer elemento/
;Con append se puede agregar un  elemento al final, agregandolo como lista '(X)
;Derrepente es mejor usar list envez de '
(define myAppend (lambda (L1 L2)
                   (if (null? L1)
                       L2
                       (if (null? L2)
                           L1
                           (if (null? (cdr L1))
                               (car L1)
                               (if (null? cdr L2)
                                   (cons L1 (car L2))
                                   (myAppend L1 (cdr L2))
  
                                   ))
                           )

                       )
                   ))

;##########################################################################################
;###################################TDA COMMIT#############################################
;##########################################################################################



; Representacion:
; Una lista conformada por elementos, string x string x int x string x string
; '(Archivo, Autor, marca de tiempo, mensaje descriptivo, cambios almacenados)
;Archivo = String con el nombre del archivo con el formato "nombreArchivo.extencion"
;Autor = String Con el nombre del Autor con el formato "Nombre Apellido"
;Fecha = Int con el formato AñoMesDia Ejemplo: 18:29 3 de abril 2020 será representado por 202003031829
;Descripcion = String con la descripcion del cambio
;Cambios = String con los cambios agregados



;Constructor

;Ejemplo: (createCommit "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")
; > '("lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")
(define (createCommit archivo autor fecha descripcion cambios)
  (if (and (and autor? fecha?)(and descripcion? cambios?))
      (list archivo autor fecha descripcion cambios)
      "Commit con parametros no permitidos"
      )
  )

; Pertenencia

;Dominio: String
;Recorrido: Boleano
;Descripcion: Analiza el input, comprobando que la entrada sea un elemento valido para la construccion.


(define (archivo? archivo)
  (if (null? archivo)
      #f
      (if (string? archivo)
          #t
          #f)
      )
  )

;Dominio: String
;Recorrido: Boleano
;Descripcion: Analiza el input, comprobando que la entrada sea un elemento valido para la construccion.


(define (autor? autor)
  (if (null? autor)
      #f
      (if (string? autor)
          #t
          #f)
      )
  )  
 
;Dominio: Intiger
;Recorrido: Boleano
;Descripcion: Analiza el input, comprobando que la entrada sea un elemento valido para la construccion.

(define  (fecha? fecha)
  (if (null? fecha)
      #f
      (if (number? fecha)
          (if (and (> fecha 202000000000)(< fecha 202100000000))
              #t
              #f)
          #f)
      )
  )
  
 
;Dominio: String
;Recorrido: Boleano
;Descripcion: Analiza el input, comprobando que la entrada sea un elemento valido para la construccion.

(define (descripcion? descripcion)
  (if (null? descripcion)
      #f
      (if (string? descripcion)
          #t
          #f)
      )
  )
  
 
;Dominio: String
;Recorrido: Boleano
;Descripcion: Analiza el input, comprobando que la entrada sea un elemento valido para la construccion.

(define (cambios? cambios)
  (if (null? cambios)
      #f
      (if (string? cambios)
          #t
          #f)
      )
  )
  

; Selectores

;Dominio: Commit
;Recorrido: Nombre Archivo
;Descripcion: funcion que retorna el nombre del archivo
; Ejemplo: (getArchivo (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")) > "lab1.rkt"
(define (getArchivo commit)
  (if (archivo? (car commit))
      (car commit)
      null
      )
  )

;Dominio: Commit
;Recorrido: Nombre Autor
;Descripcion: funcion que retorna el nombre del Autor
; Ejemplo: (getAutor (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")) > "Seba Villa"
(define (getAutor commit)
  (if (autor? (car (cdr commit)))
      (car (cdr commit))
      null
      )
  )
;Dominio: Commit
;Recorrido: Fecha
;Descripcion: funcion que retorna la fecha del commit
; Ejemplo: (getFecha (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")) > 202003031829
(define (getFecha commit)
  (if (fecha? (car (cdr (cdr commit))))
      (car (cdr (cdr commit)))
      null
      )
  )
;Dominio: Commit
;Recorrido: Descripcion
;Descripcion: funcion que retorna la descripcion de los cambios
; Ejemplo: (getDescripcion (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")) > "agregar hora"
(define (getDescripcion commit)
  (if (descripcion? (car (cdr (cdr (cdr commit)))))
      (car (cdr (cdr (cdr commit))))
      null
      )
  )
;Dominio: Commit
;Recorrido: Cambios
;Descripcion: funcion que retorna los cambios en el archivo
; Ejemplo: (getCambios (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora")) > "add hora"
(define (getCambios commit)
  (if (cambios? (car (cdr (cdr (cdr (cdr commit))))))
      (car (cdr (cdr (cdr (cdr commit)))))
      null
      )
  )

;############ ejemplo ##################

#|
(define commit1 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora"))
(define commit2 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora2"))
(define commit3 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora3"))
(define commit4 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora4"))
(define commit5 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora5"))
(define commit6 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora6"))
(define commit7 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora7"))
(define commit8 (list "lab1.rkt" "Seba Villa" 202003031829 "agregar hora" "add hora8"))

(define WORKSPACE (createWorkspace (list commit1 commit2)))
(define INDEX (createIndex (list commit3 commit4)))
(define LOCAL (createLocal (list commit5 commit6)))
(define REMOTE (createRemote (list commit7 commit8)))

(createZonas WORKSPACE INDEX LOCAL REMOTE)

|#

;##########################################################################################
;###################################TDA ZONAS##############################################
;##########################################################################################



;Representacion:

; Zonas esta representado por una lista con 5 elementos, los 4 primeros elementos representarán una zona
; y cada zona será una lista con los commits en ella, el ultimo elemento es un log de comandos aplicados
; '( '(workspace) '(index) '(local) '(remote) '(logs )

; Workspace está representado por una lista de commits
; Index está representado por una lista de commits
; Local Repository está representado por una lista de commits
; Remote Repository está representado por una lista de commits
; Logs está representado por una lista de strings con el comando y el tiempo en el que se usaron.

; Constructores

;Dominio: workspace x index x local x remote
;Recorrido: zonas
;descripcion: Crea una lista que representa las zonas del GIT
(define (createZonas workspace index local remote logs)
  (if (and (and workspace? index?) (and local? remote?))
      (list workspace index local remote logs)
      "Las zonas tienen un formato incorrecto"
      )
  )



;Dominio: lista de commits
;Recorrido: Workspace
;Descripcion: Crea una lista con commits validos que representa el workspace
;Recursion: Natural
(define (createWorkspace listaCommits)
  (if (list? listaCommits)
      (if (null? listaCommits)
          null
          (if (commit? (car listaCommits))
              (cons (car listaCommits) (createWorkspace (cdr listaCommits)))
              (createWorkspace (cdr listaCommits))
              )
          )
      "No es una lista de commits"
      )
  )



;Dominio: lista de commits
;Recorrido: Index
;Descripcion: Crea una lista con commits validos que representa el index
;Recursion: Natural
(define (createIndex listaCommits)
  (if (list? listaCommits)
      (if (null? listaCommits)
          null
          (if (commit? (car listaCommits))
              (cons (car listaCommits) (createIndex (cdr listaCommits)))
              (createIndex (cdr listaCommits))
              )
          )
      "No es una lista de commits"
      )
  )

;Dominio: lista de commits
;Recorrido: Local Repository
;Descripcion: Crea una lista con commits validos que representa el Local Repository
;Recursion: Natural
(define (createLocal listaCommits)
  (if (list? listaCommits)
      (if (null? listaCommits)
          null
          (if (commit? (car listaCommits))
              (cons (car listaCommits) (createLocal (cdr listaCommits)))
              (createLocal (cdr listaCommits))
              )
          )
      "No es una lista de commits"
      )
  )


;Dominio: lista de commits
;Recorrido: Remote Repository
;Descripcion: Crea una lista con commits validos que representa el Remote Repository
;Recursion: Natural
(define (createRemote listaCommits)
  (if (list? listaCommits)
      (if (null? listaCommits)
          null
          (if (commit? (car listaCommits))
              (cons (car listaCommits) (createRemote (cdr listaCommits)))
              (createRemote (cdr listaCommits))
              )
          )
      "No es una lista de commits"
      )
  )






; Pertenencia

;Dominio: commit
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un commit valido.

(define (commit? commit)
  (if (list? commit)
      (if (and (archivo? (car commit))(and (and (autor? (car (cdr commit))) (fecha? (car(cdr (cdr commit)))))(and (descripcion? (car(cdr(cdr (cdr commit))))) (cambios? (car(cdr(cdr(cdr (cdr commit))))))))) 
          #t
          #f
          )
      #f
      )
  )

;Dominio: workspace
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un workspace valido, es decir que sea una lista de coimmits validos

(define (workspace? workspace)
  (if (list? workspace)
      (if (null? workspace)
          #t
          (if (commit? (car workspace))      
              (workspace? (cdr workspace))
              #f          
              )
          )
      #f
      )
  )

;Dominio: index
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un index valido, es decir que sea una lista de coimmits validos

(define (index? index)
  (if (list? index)
      (if (null? index)
          #t
          (if (commit? (car index))      
              (index? (cdr index))
              #f          
              )
          )
      #f
      )
  )

;Dominio: local repository
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un local repository valido, es decir que sea una lista de coimmits validos

(define (local? local)
  (if (list? local)
      (if (null? local)
          #t
          (if (commit? (car local))      
              (local? (cdr local))
              #f          
              )
          )
      #f
      )
  )

;Dominio: remote repository
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un remote repository valido, es decir que sea una lista de coimmits validos

(define (remote? remote)
  (if (list? remote)
      (if (null? remote)
          #t
          (if (commit? (car remote))      
              (remote? (cdr remote))
              #f          
              )
          )
      #f
      )
  )

;Dominio: remote repository
;Recorrido: boleano
;Descripcion:  Analiza el input, comprobando que la entrada sea un remote repository valido, es decir que sea una lista de coimmits validos

(define (log? log)
  (if (list? log)
      #t
      #f
      )
  )



; Selectores

;Dominio: Zonas
;Recorrido: Workspace
;Descripcion: Obtiene el workspace de la zona

(define (getWorkspace zona)
  (if (null? zona)
      null
      (if (workspace? (car zona))
          (car zona)
          null
          )
      )
  )

;Dominio: Zonas
;Recorrido: Index
;Descripcion: Obtiene el Index de la zona

(define (getIndex zona)
  (if (null? zona)
      null
      (if (index? (car (cdr zona)))
          (car (cdr zona))
          null
          )
      )
  )
;Dominio: Zonas
;Recorrido: Local Repository
;Descripcion: Obtiene el Local Repository de la zona

(define (getLocal zona)
  (if (null? zona)
      null
      (if (local? (car (cdr (cdr zona))))
          (car (cdr (cdr zona)))
          null
          )
      )
  )

;Dominio: Zonas
;Recorrido: Remote Repository
;Descripcion: Obtiene el Remote Repository de la zona

(define (getRemote zona)
  (if (null? zona)
      null
      (if (remote? (car (cdr (cdr (cdr zona)))))
          (car (cdr (cdr (cdr zona))))
          null
          )
      )
  )

;Dominio: Zonas
;Recorrido: Logs
;Descripcion: Obtiene los logs de la zona

(define (getLogs zona)
  (if (null? zona)
      null
      (if (log? (car (cdr (cdr (cdr (cdr zona))))))
          (car (cdr (cdr (cdr (cdr zona)))))
          null
          )
      )
  )


;MODIFICADORES

(define (addLogs comando tiempo zonas)
  (list (getWorkspace zonas) (getIndex zonas) (getLocal zonas) (getRemote zonas) (append (list tiempo comando) (getLogs zonas) ))
  )


;##########################################################################################
;######################################GIT################################################
;##########################################################################################

(define git (lambda (comando) (lambda (b) (comando b ))))

;##########################################################################################
;######################################PULL################################################
;##########################################################################################


;Descripcion: Funciona que retorna una lista con todos los cambios (commits) desde el remote repository
; al workspace
;Dominio: zonas
;Recorrido: nuevas zonas

(define pull (lambda (zonas) (lambda (tiempo)
                               (list (getRemote zonas)) (getIndex zonas) (getLocal zonas) (getRemote zonas) (addLogs "pull" tiempo zonas))))



;##########################################################################################
;######################################ADD#################################################
;##########################################################################################



;Dominio: arvhivo x workspace
;Recorrido: lista
;Descripcion:  Retorna una lista con los commits que tienen el nombre del archivo buscado en la primera posicion  de un elemento de una lista.


(define add1 (lambda (archivo) (lambda (workspace) 
                                 (if (null? workspace)
                                     null
                                     (if (equal? (getArchivo (car workspace)) archivo)
                                         (cons  (car workspace) ((add1 archivo) (cdr workspace)))
                                         ((add1 archivo) (cdr workspace))
                                         )))))

;Dominio: lista de archivos x workspace
;Recorrido: lista de match
;Descripcion: Recorre una lista de commits (Workspace) y compara su contenido con el primer elemento de cada
;commit, si hay match lo guarda en una lista, cuando termina de recorrer ambas listas retorna una lista
;con todos los elementos encontrados.
;Ejemplo: ((add2 (list "hola" "sebas" )) (list (list "hola" "Seba Villa" 202003031829 "agregar hora" "add hora") (list "holsa" "Seba Villa" 202003031829 "agregar hora" "add hora") (list "sebas" "Seba Villa" 202003031829 "agregar hora" "add hora" )))
; > '(("hola" "Seba Villa" 202003031829 "agregar hora" "add hora")
;  ("sebas" "Seba Villa" 202003031829 "agregar hora" "add hora"))

 (define add2 (lambda (archivos) (lambda (workspace) 
                                 (if (null? archivos)
                                     null
                                     (append ((add1 (car archivos)) workspace) ((add2 (cdr archivos)) workspace) )
                                     )
                                   )
                ))
                                     





;Descripcion: Funcion que añade los cambios locales registrados en el workspace al index
;registrados en la zona de trabajo.
;Dominio: archivos
;Recorrido: Una nueva zona donde se ven reflejado los cambios hechos en los archivos especificados

;# CURRIFICACION CTM
(define (suma n m) (+ n m))

(define giat (lambda (a) (lambda (b) (* a (suma b)))))

; # CADA VEZ QUE SE USA GIT SE VA CREANDO UN COMMIT, ANALIZAR, SE VA CREANDO PERO NO ENTERO ALGUNOS ELEMENTOS
; QUEDAN EN NULL



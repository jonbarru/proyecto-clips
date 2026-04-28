;; =================================================================           
;; 1. ESTRUCTURAS DE DATOS (TEMPLATES)
;; =================================================================

;; Controla el estado global del juego (tamaño y en qué paso estamos)
(deftemplate juego
    (slot tamano)
    (slot fase (default pedir-tamano)) 
)

;; Representa cada celda del tablero
(deftemplate casilla
    (slot fila) 
    (slot columna)
    (slot estado (default vacia)) ; Puede ser: vacia, negra, blanca
)

;; Datos de la sesión actual: de quién es el turno y conteo de fichas
(deftemplate partida
    (slot turno (default negra)) 
    (slot fichas-negras (default 2)) 
    (slot fichas-blancas (default 2))
)

;; Hecho temporal que marca qué casillas son legales para el turno actual
(deftemplate movimiento-valido
    (slot fila)
    (slot columna)
)

;; Hecho temporal para capturar la entrada del usuario
(deftemplate jugada
    (slot fila)
    (slot columna)
)

;; Coordenadas para explorar las 8 direcciones (N, NE, E, SE, S, SO, O, NO)
(defglobal ?*dirs* = (create$ -1 -1  -1 0  -1 1   0 -1  0 1   1 -1  1 0  1 1))
(deftemplate movimiento
    (slot fila) ; fila del movimiento
    (slot columna) ; columna del movimiento
    (slot jugador) ; jugador que realiza el movimiento: negra o blanca
    (slot fichas-capturadas) ; número de fichas capturadas con este movimiento
)

;; Hecho temporal para marcar que hay que voltear en una dirección concreta
(deftemplate voltear-direccion
    (slot fila-origen)
    (slot columna-origen)
    (slot dx)
    (slot dy)
    (slot jugador)
)


;; =================================================================
;; 2. INICIALIZACIÓN
;; =================================================================

(deffacts inicio
    (juego (tamano 0) (fase pedir-tamano))
)


;; =================================================================
;; 3. REGLAS DE CONFIGURACIÓN Y FLUJO
;; =================================================================

;; Fase 1: Solicitar tamaño y validar que sea par y >= 4
(defrule pedir-tamano
    ?j <- (juego (tamano 0) (fase pedir-tamano))
    =>
    (printout t "Introduce el tamaño del tablero (4, 6, 8, 10...): " crlf)
    (bind ?t (read))

    (while (or (not (integerp ?t)) (< ?t 4) (<> (mod ?t 2) 0))
        (printout t "Error: Debe ser entero, par y >= 4." crlf)
        (bind ?t (read))
    )

    (modify ?j (tamano ?t) (fase inicializar-tablero))
)

;; Fase 2: Crear los hechos 'casilla' y colocar las 4 fichas centrales
(defrule inicializar-tablero
    ?j <- (juego (tamano ?t) (fase inicializar-tablero))
    =>
    (bind ?m1 (/ ?t 2))
    (bind ?m2 (+ ?m1 1))

    (loop-for-count (?f 1 ?t)
        (loop-for-count (?c 1 ?t)
            (bind ?est vacia)
            ;; Lógica de posición inicial de Othello/Reversi
            (if (and (= ?f ?m1) (= ?c ?m1)) then (bind ?est blanca))
            (if (and (= ?f ?m1) (= ?c ?m2)) then (bind ?est negra))
            (if (and (= ?f ?m2) (= ?c ?m1)) then (bind ?est negra))
            (if (and (= ?f ?m2) (= ?c ?m2)) then (bind ?est blanca))

            (assert (casilla (fila ?f) (columna ?c) (estado ?est)))
        )
    )
    (assert (partida (turno negra)))
    (modify ?j (fase calcular-movimientos))
)


;; =================================================================
;; 4. LÓGICA DE JUEGO (CÁLCULO Y MOVIMIENTOS)
;; =================================================================

;; Fase 3: Borrar movimientos del turno anterior para no mezclar pistas
(defrule limpiar-movimientos
    ?j <- (juego (fase limpiar-movimientos))
    =>
    (do-for-all-facts ((?m movimiento-valido)) TRUE
        (retract ?m)
    )
    (modify ?j (fase calcular-movimientos))
)

;; Fase 4: Detectar qué casillas encierran las fichas del otro jugador
(defrule detectar-movimientos
    ?j <- (juego (fase calcular-movimientos))
    ?p <- (partida (turno ?turno))
    ?c <- (casilla (fila ?f) (columna ?col) (estado vacia))
    =>
    (bind ?enemigo (if (eq ?turno negra) then blanca else negra))

    ;; Explorar las 8 direcciones usando el vector global ?*dirs*
    (loop-for-count (?i 1 8)
        (bind ?dx (nth$ (- (* ?i 2) 1) ?*dirs*))
        (bind ?dy (nth$ (* ?i 2) ?*dirs*))
        (bind ?x (+ ?f ?dx))
        (bind ?y (+ ?col ?dy))
        (bind ?encontradas 0)

        (while TRUE
            (bind ?estado none)
            ;; Consultamos el estado de la casilla en la dirección actual
            (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?x) (= ?cas:columna ?y))
                (bind ?estado ?cas:estado))

            ;; Si salimos del tablero o hay hueco, esta dirección no vale
            (if (eq ?estado none) then (break))

            (if (eq ?estado ?enemigo) 
                then 
                (bind ?encontradas (+ ?encontradas 1))
                (bind ?x (+ ?x ?dx))
                (bind ?y (+ ?y ?dy))
                else 
                ;; Si tras encontrar enemigas, encontramos una propia: ¡MOVIMIENTO VÁLIDO!
                (if (and (eq ?estado ?turno) (> ?encontradas 0)) then
                    (assert (movimiento-valido (fila ?f) (columna ?col)))
                )
                (break)
            )
        )
    )
)

;; Regla "puente": Cuando 'detectar-movimientos' termina, pasamos a imprimir
;; El salience -10 asegura que se ejecute al final de la fase de cálculo

(defrule finalizar-deteccion
    (declare (salience -10))
    ?j <- (juego (fase calcular-movimientos))
    =>
    (modify ?j (fase imprimir-tablero))
)


;; =================================================================
;; 5. INTERFAZ (SALIDA POR PANTALLA)
;; =================================================================

;; Fase 5: Dibujar el tablero y mostrar opciones legales
(defrule imprimir-tablero
    ?j <- (juego (tamano ?t) (fase imprimir-tablero))
    ?p <- (partida (turno ?turno))
    =>
    (printout t crlf "    ")
    (loop-for-count (?c 1 ?t) (printout t ?c "  "))
    (printout t crlf "   " "---" crlf)

    (loop-for-count (?f 1 ?t)
        (printout t ?f " | ")
        (loop-for-count (?c 1 ?t)
            (bind ?simbolo ".")
            (bind ?es-valida FALSE)

            ;; Miramos si esta casilla es una de las calculadas como válidas
            (do-for-fact ((?m movimiento-valido)) (and (= ?m:fila ?f) (= ?m:columna ?c))
                (bind ?es-valida TRUE))

            ;; Miramos quién ocupa la casilla
            (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?f) (= ?cas:columna ?c))
                (if (eq ?cas:estado negra) then (bind ?simbolo "N"))
                (if (eq ?cas:estado blanca) then (bind ?simbolo "B"))
            )

            ;; Si está vacía pero se puede jugar, ponemos una pista
            (if (and (eq ?simbolo ".") ?es-valida) then (bind ?simbolo "o"))
            (printout t ?simbolo "  ")
        )
        (printout t "|" crlf)
    )
    (printout t "Turno: " (upcase ?turno) crlf)
    (modify ?j (fase esperar-jugada))
)

; Imprimir tablero al principio, antes del turno humano para ver posibles, y después de cada movimiento para ver el resultado. También al final para mostrar el resultado final.



;; =================================================================
;; 6. INTERACCIÓN Y VALIDACIÓN DE JUGADA
;; =================================================================

;; Fase 6: Pedir input al usuario
(defrule pedir-jugada
    ?j <- (juego (fase esperar-jugada))
    =>
    (printout t "Introduce fila: ")
    (bind ?f (read))
    (printout t "Introduce columna: ")
    (bind ?c (read))
    (assert (jugada (fila ?f) (columna ?c)))
    (modify ?j (fase verificar-jugada))
)

;; Fase 7a: Si la jugada coincide con un 'movimiento-valido', aplicar cambios
(defrule validar-jugada-correcta
    ?j <- (juego (fase verificar-jugada))
    ?p <- (partida (turno ?turno))
    ?mov <- (movimiento-valido (fila ?f) (columna ?c))
    ?jug <- (jugada (fila ?f) (columna ?c))
    ?cas <- (casilla (fila ?f) (columna ?c) (estado vacia))
    =>
    (printout t "¡Jugada aceptada!" crlf)
    (modify ?cas (estado ?turno))

    ;; Lanzar un hecho de volteo por cada dirección
    (loop-for-count (?i 1 8)
        (bind ?dx (nth$ (- (* ?i 2) 1) ?*dirs*))
        (bind ?dy (nth$ (* ?i 2) ?*dirs*))
        (assert (voltear-direccion
            (fila-origen ?f)
            (columna-origen ?c)
            (dx ?dx)
            (dy ?dy)
            (jugador ?turno)))
    )

    (bind ?nuevo-turno (if (eq ?turno negra) then blanca else negra))
    (modify ?p (turno ?nuevo-turno))
    (retract ?jug)
    (modify ?j (fase voltear-fichas))
)

(defrule voltear-fichas
    (declare (salience 10))
    ?j <- (juego (fase voltear-fichas))
    ?vd <- (voltear-direccion

                (fila-origen ?f0)
                (columna-origen ?c0)
                (dx ?dx) (dy ?dy)
                (jugador ?jugador))
    =>
    (bind ?enemigo (if (eq ?jugador negra) then blanca else negra))
    (bind ?x (+ ?f0 ?dx))
    (bind ?y (+ ?c0 ?dy))

    ;; Primero verificar que esta dirección es válida (hay una propia al final)
    (bind ?valida FALSE)
    (bind ?cx ?x)
    (bind ?cy ?y)
    (bind ?encontradas 0)

    (while TRUE
        (bind ?est none)
        (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?cx) (= ?cas:columna ?cy))
            (bind ?est ?cas:estado))
        (if (eq ?est none) then (break))
        (if (eq ?est ?enemigo)
            then
                (bind ?encontradas (+ ?encontradas 1))
                (bind ?cx (+ ?cx ?dx))
                (bind ?cy (+ ?cy ?dy))
            else
                (if (and (eq ?est ?jugador) (> ?encontradas 0))
                then (bind ?valida TRUE))
                (break)
        )
    )

    ;; Si la dirección es válida, voltear todas las enemigas que encontramos
    (if ?valida then
        (while (not (and (= ?x ?cx) (= ?y ?cy)))
            (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?x) (= ?cas:columna ?y))
                (modify ?cas (estado ?jugador))
            )
            (bind ?x (+ ?x ?dx))
            (bind ?y (+ ?y ?dy))
        )
    )

    (retract ?vd)
)


(defrule fin-volteo
    ?j <- (juego (fase voltear-fichas))
    (not (voltear-direccion))
    =>
    (modify ?j (fase limpiar-movimientos))
)

;; Fase 7b: Si la jugada no existe como hecho 'movimiento-valido'
(defrule validar-jugada-incorrecta
    ?j <- (juego (fase verificar-jugada))
    ?jug <- (jugada (fila ?f) (columna ?c))
    (not (movimiento-valido (fila ?f) (columna ?c)))
    =>
    (printout t "ERROR: Esa posición no es válida. Prueba otra." crlf)
    (retract ?jug)
    (modify ?j (fase esperar-jugada))
)

;; ================================================================
;; 7. FIN DE PARTIDA Y PASO DE TURNO
;; ================================================================


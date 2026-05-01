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
    (slot humano (default negra)) ; El jugador humano elige color al inicio
    (slot maquina (default blanca))
    (slot pases-consecutivos (default 0)) ; controla si ambos pasan
)

;; Hecho temporal que marca qué casillas son legales para el turno actual
(deftemplate movimiento-valido
    (slot fila)
    (slot columna)
    (slot capturas (default 0)) ; Fichas que capturaría
    (slot score (default 0))    ; Puntuación total (capturas + prioridad)
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
    (printout t "Introduce el tamano del tablero (4, 6, 8, 10...): " crlf)
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
;; Fase 4: Detectar qué casillas son legales y calcular su puntuación (Heurística)
(defrule detectar-movimientos
    ?j <- (juego (tamano ?t) (fase calcular-movimientos))
    ?p <- (partida (turno ?turno))
    ?c <- (casilla (fila ?f) (columna ?col) (estado vacia))
    =>
    (bind ?enemigo (if (eq ?turno negra) then blanca else negra))
    (bind ?total-volteadas 0)

    ;; Explorar las 8 direcciones usando el vector global ?*dirs*
    (loop-for-count (?i 1 8)
        (bind ?dx (nth$ (- (* ?i 2) 1) ?*dirs*))
        (bind ?dy (nth$ (* ?i 2) ?*dirs*))
        (bind ?x (+ ?f ?dx))
        (bind ?y (+ ?col ?dy))
        (bind ?encontradas 0)
        (bind ?direccion-valida FALSE)

        (while TRUE
            (bind ?estado none)
            ;; Consultamos el estado de la casilla en la dirección actual
            (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?x) (= ?cas:columna ?y))
                (bind ?estado ?cas:estado)
            )

            ;; Si salimos del tablero o encontramos una casilla vacía, cortamos
            (if (or (eq ?estado none) (eq ?estado vacia)) then (break))

            (if (eq ?estado ?enemigo) 
                then 
                (bind ?encontradas (+ ?encontradas 1))
                (bind ?x (+ ?x ?dx))
                (bind ?y (+ ?y ?dy))
                else 
                ;; Si tras encontrar enemigas, encontramos una propia: ¡DIRECCIÓN VÁLIDA!
                (if (and (eq ?estado ?turno) (> ?encontradas 0)) then
                    (bind ?direccion-valida TRUE)
                )
                (break)
            )
        )
        
        ;; Si esta dirección es buena, sumamos las fichas al total
        (if ?direccion-valida then
            (bind ?total-volteadas (+ ?total-volteadas ?encontradas))
        )
    )

    ;; Si el movimiento captura al menos 1 ficha en total, es válido y le damos nota
    ;; HEURISTICA -- independiente del tamaño
    (if (> ?total-volteadas 0) then
        (bind ?prioridad 0)

        (bind ?penultima (- ?t 1))

        ;; 1. Heurística: ¿Es una esquina? (+100 puntos)
        (if (or (and (= ?f 1) (= ?col 1)) 
                (and (= ?f 1) (= ?col ?t)) 
                (and (= ?f ?t) (= ?col 1)) 
                (and (= ?f ?t) (= ?col ?t))) then
            (bind ?prioridad 100)
        else
            ;; 2. Heurística: ¿Es adyacente a una esquina? (-50 puntos) ¡Evitar!
            (if (or (and (<= ?f 2) (<= ?col 2)) 
                    (and (<= ?f 2) (>= ?col ?penultima)) 
                    (and (>= ?f ?penultima) (<= ?col 2)) 
                    (and (>= ?f ?penultima) (>= ?col ?penultima))) then
                (bind ?prioridad -50)
            else
            ;; 3. Heurística: ¿Está en el borde? (+10 puntos)
                (if (or (= ?f 1) (= ?f ?t) (= ?col 1) (= ?col ?t)) then
                        (bind ?prioridad 10)
                    )
            )
        )

        ;; El score final junta la posición y la cantidad de fichas robadas
        (bind ?score-final (+ ?total-volteadas ?prioridad))
        
        ;; Registramos el movimiento temporalmente
        (assert (movimiento-valido (fila ?f) (columna ?col) (capturas ?total-volteadas) (score ?score-final)))
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
    (bind ?negras 0)
    (bind ?blancas 0)
    (do-for-all-facts ((?cas casilla)) (eq ?cas:estado negra) (bind ?negras (+ ?negras 1)))
    (do-for-all-facts ((?cas casilla)) (eq ?cas:estado blanca) (bind ?blancas (+ ?blancas 1)))

    (printout t crlf "   ")
    
    ;; 1. Imprimir cabecera de columnas dinámicamente
    (loop-for-count (?c 1 ?t) 
        ;; Si es de un dígito le ponemos un espacio extra para alinear con los de dos dígitos
        (if (< ?c 10) then (printout t " " ?c " ") else (printout t ?c " "))
    )
    
    ;; 2. Borde superior dinámico
    (printout t crlf "   +")
    (loop-for-count (?c 1 ?t) (printout t "---"))
    (printout t "+" crlf)

    ;; 3. Filas y casillas
    (loop-for-count (?f 1 ?t)
        ;; Alinear números de fila (espacio extra si es < 10)
        (if (< ?f 10) then (printout t " " ?f " |") else (printout t ?f " |"))
        
        (loop-for-count (?c 1 ?t)
            (bind ?simbolo ".")
            (bind ?es-valida FALSE)

            (do-for-fact ((?m movimiento-valido)) (and (= ?m:fila ?f) (= ?m:columna ?c))
                (bind ?es-valida TRUE))

            (do-for-fact ((?cas casilla)) (and (= ?cas:fila ?f) (= ?cas:columna ?c))
                (if (eq ?cas:estado negra) then (bind ?simbolo "N"))
                (if (eq ?cas:estado blanca) then (bind ?simbolo "B"))
            )

            (if (and (eq ?simbolo ".") ?es-valida) then (bind ?simbolo "o"))
            ;; Imprimimos el símbolo con un espacio a cada lado para darle "aire"
            (printout t " " ?simbolo " ")
        )
        (printout t "|" crlf)
    )
    
    ;; 4. Borde inferior dinámico
    (printout t "   +")
    (loop-for-count (?c 1 ?t) (printout t "---"))
    (printout t "+" crlf)
    
    ;; 5. Puntuaciones
    (printout t "PUNTUACION -> Negras (N): " ?negras " | Blancas (B): " ?blancas crlf)
    (printout t "TURNO ACTUAL: " (upcase ?turno) crlf)
    
    (modify ?j (fase esperar-jugada))
)

; Imprimir tablero al principio, antes del turno humano para ver posibles, y después de cada movimiento para ver el resultado. También al final para mostrar el resultado final.



;; =================================================================
;; 6. INTERACCIÓN Y VALIDACIÓN DE JUGADA
;; =================================================================

;; Fase 6: Pedir input al usuario
(defrule pedir-jugada
    ?j <- (juego (fase esperar-jugada))
    ?p <- (partida (turno ?turno) (humano ?turno)) ; Comprueba si es el turno del humano
    =>
    (printout t "Tu turno (" (upcase ?turno) "). Introduce fila: ") 
    (bind ?f (read))
    (printout t "Introduce columna: ")
    (bind ?c (read))
    (assert (jugada (fila ?f) (columna ?c)))
    (modify ?j (fase verificar-jugada))
)

(defrule turno-maquina
    ?j <- (juego (fase esperar-jugada))
    ?p <- (partida (turno ?turno) (maquina ?turno)) ; Comprueba si es el turno de la máquina
    =>
    (printout t "Pensando jugada de la maquina..." crlf)
    
    (bind ?max-score -9999)
    (bind ?mejor-f -1)
    (bind ?mejor-c -1)
    
    ;; Revisa todos los movimientos y se queda con el de mayor SCORE
    (do-for-all-facts ((?m movimiento-valido)) TRUE
        (if (> ?m:score ?max-score) then
            (bind ?max-score ?m:score)
            (bind ?mejor-f ?m:fila)
            (bind ?mejor-c ?m:columna)
        )
    )
    
    (printout t ">> La maquina decide jugar en Fila " ?mejor-f ", Columna " ?mejor-c crlf)
    
    ;; Ejecuta la jugada automáticamente
    (assert (jugada (fila ?mejor-f) (columna ?mejor-c)))
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
    (printout t "Jugada aceptada!" crlf)
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
    (modify ?p (turno ?nuevo-turno) (pases-consecutivos 0)) ; Reseteamos el contador de pases
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
    (printout t "ERROR: Esa posicion no es valida. Prueba otra." crlf)
    (retract ?jug)
    (modify ?j (fase esperar-jugada))
)

;; ================================================================
;; 7. FIN DE PARTIDA Y PASO DE TURNO
;; ================================================================

;; Si el jugador actual no tiene movimientos válidos, pasa el turno.
(defrule pasar-turno-sin-movimientos
    (declare (salience 5))
    ?j <- (juego (fase imprimir-tablero))
    ?p <- (partida (turno ?turno) (pases-consecutivos ?pases))
    (not (movimiento-valido))
    =>
    (printout t crlf ">>> El jugador " (upcase ?turno) " no tiene movimientos legales. Pasa el turno." crlf)
    (bind ?nuevo-turno (if (eq ?turno negra) then blanca else negra))
    
    ;; Sumamos 1 al contador de pases consecutivos
    (modify ?p (turno ?nuevo-turno) (pases-consecutivos (+ ?pases 1)))
    (modify ?j (fase calcular-movimientos))
)


(defrule detectar-fin-por-tablero-lleno
    (declare (salience 20))
    ?j <- (juego (fase limpiar-movimientos))
    (not (casilla (estado vacia)))
    =>
    (modify ?j (fase fin-juego))
)

(defrule detectar-fin-por-doble-pase
    (declare (salience 50)) ; Prioridad alta para que se active antes que nada
    ?j <- (juego (fase ?fase&~fin-juego)) ; Si no estamos ya en el fin
    ?p <- (partida (pases-consecutivos 2)) ; Si han habido 2 pases seguidos
    =>
    (printout t crlf "ATENCION! Ningun jugador tiene movimientos validos." crlf)
    (printout t "Terminando la partida anticipadamente..." crlf)
    (modify ?j (fase fin-juego))
    (modify ?p (pases-consecutivos 0)) ; Reset por seguridad
)

(defrule mostrar-ganador
    ?j <- (juego (fase fin-juego))
    ?p <- (partida (humano ?humano))
    =>
    (bind ?negras 0)
    (bind ?blancas 0)
    
    (do-for-all-facts ((?c casilla)) (eq ?c:estado negra) (bind ?negras (+ ?negras 1)))
    (do-for-all-facts ((?c casilla)) (eq ?c:estado blanca) (bind ?blancas (+ ?blancas 1)))
    
    (printout t crlf "===== RESULTADO FINAL =====" crlf)
    (printout t "Negras: " ?negras " | Blancas: " ?blancas crlf)
    
    (bind ?ganador none)
    (if (> ?negras ?blancas) then (bind ?ganador negra))
    (if (> ?blancas ?negras) then (bind ?ganador blanca))
    
    (if (eq ?ganador ?humano) then
        (printout t " __   __  _______  __   __    __   __  ___   __    _ " crlf)
        (printout t "|  | |  ||       ||  | |  |  |  | |  ||   | |  |  | |" crlf)
        (printout t "|  |_|  ||   _   ||  | |  |  |  |_|  ||   | |   |_| |" crlf)
        (printout t "|       ||  | |  ||  |_|  |  |       ||   | |       |" crlf)
        (printout t "|_     _||  |_|  ||       |  |       ||   | |  _    |" crlf)
        (printout t "  |   |  |       ||       |   |     | |   | | | |   |" crlf)
        (printout t "  |___|  |_______||_______|    |___|  |___| |_|  |__|" crlf)
    else
        (if (eq ?ganador none) then
            (printout t "------- EMPATE -------" crlf)
        else
            (printout t "__   __  _______  __   __    ___      _______  _______  _______ " crlf)
            (printout t "|  | |  ||       ||  | |  |  |   |    |       ||       ||       |" crlf)
            (printout t "|  |_|  ||   _   ||  | |  |  |   |    |   _   ||  _____||    ___|" crlf)
            (printout t "|       ||  | |  ||  |_|  |  |   |    |  | |  || |_____ |   |___ " crlf)
            (printout t "|_     _||  |_|  ||       |  |   |___ |  |_|  ||_____  ||    ___|" crlf)
            (printout t "  |   |  |       ||       |  |       ||       | _____| ||   |___ " crlf)
            (printout t "  |___|  |_______||_______|  |_______||_______||_______||_______|" crlf)
        )
    )
    (halt)
)
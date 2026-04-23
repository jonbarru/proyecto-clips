;; ==========================================
;; ==========================================
;; 1. TEMPLATES (Estructuras de datos)
;; ==========================================
;; ==========================================

(deftemplate juego
   (slot tamano)
   (slot fase (default pedir-tamano)) ; fase actual del juego
)

(deftemplate casilla
   (slot fila) ; fila, y
   (slot columna) ; columna, x
   (slot estado (default vacia)) ; vacia, negra, blanca
)

(deftemplate partida
   (slot turno (default negra)) ; turno actual: negra o blanca
   (slot fichas-negras (default 2)) ; número de fichas negras en el tablero
   (slot fichas-blancas (default 2)) ; número de fichas blancas
)

(deftemplate movimiento
   (slot fila) ; fila del movimiento
   (slot columna) ; columna del movimiento
   (slot jugador) ; jugador que realiza el movimiento: negra o blanca
   (slot fichas-capturadas) ; número de fichas capturadas con este movimiento
)


;; ==========================================
;; ==========================================
;; 2. HECHOS INICIALES
;; ==========================================
;; ==========================================

(deffacts inicio
   (juego (tamano 0) (fase pedir-tamano))
)



;; ==========================================
;; ==========================================
;; 3. REGLAS DE FLUJO Y MOTOR
;; ==========================================
;; ==========================================

; Usamos el control de fases para guiar al usuario a través de los pasos necesarios para configurar el juego.
; Así mantenemos un orden lógico y claro en la interacción, evitando confusiones y asegurando que se sigan los pasos correctos.
; Flujo básico del juego -> fases. IA/heurísticos -> prioridad.

;; Regla 1: Pedir tamaño del tablero al user y validar
(defrule pedir-tamano
   ?j <- (juego (tamano 0) (fase pedir-tamano))
   =>
   (printout t "Introduce el tamaño del tablero (4, 6, 8, 10...): " crlf)
   (bind ?t (read))

   ; bucle para validar el tamaño introducido sea entero, par y minimo 4
   (while (or (not (integerp ?t)) (< ?t 4) (<> (mod ?t 2) 0))
      (printout t "Error: El tamaño debe ser un número entero, par y mayor o igual a 4." crlf)
      (printout t "Introduce un tamaño válido: " crlf)
      (bind ?t (read))
   )

   ; cuando es valido, actualizamos tamaño y ya pasamos 
   (modify ?j (tamano ?t) (fase inicializar-tablero))
   (printout t "¡Perfecto! Creando un tablero de  " ?t "x" ?t "..." crlf)
)

;; Regla 2: Inicializar el tablero con las 4 fichas iniciales

(defrule inicializar-tablero
   ?j <- (juego (tamano ?t) (fase inicializar-tablero))
   =>
   ; Calculamos las coordenadas centrales
   (bind ?mitad-arriba (/ ?t 2))
   (bind ?mitad-abajo (+ ?mitad-arriba 1))

   ; Doble bucle para recorrer el tablero y colocar las fichas
   (loop-for-count (?f 1 ?t)
      (loop-for-count (?c 1 ?t)
         (bind ?est vacia) ; por defecto, todas las casillas están vacías

         ; Colocamos las fichas iniciales en el centro
         (if (and (= ?f ?mitad-arriba) (= ?c ?mitad-arriba)) then (bind ?est blanca))
         (if (and (= ?f ?mitad-arriba) (= ?c ?mitad-abajo)) then (bind ?est negra))
         (if (and (= ?f ?mitad-abajo) (= ?c ?mitad-arriba)) then (bind ?est negra))
         (if (and (= ?f ?mitad-abajo) (= ?c ?mitad-abajo)) then (bind ?est blanca))

         ; Creamos el hecho de la casilla con su estado en la memoria de CLIPS
         (assert (casilla (fila ?f) (columna ?c) (estado ?est)))
      )
   )

   ; Creamos hecho de partida
   (assert (partida (turno negra) (fichas-negras 2) (fichas-blancas 2)))

   ; Pasamos a fase de juego (turno humano o máquina)
   (modify ?j (fase imprimir-tablero))

   (printout t "Tablero inicializado. Comienza el juego. Es el turno de las fichas negras." crlf)
   (printout t "--------------------------------------------------------" crlf)
)

(defrule imprimir-tablero
   ?j <- (juego (tamano ?t) (fase imprimir-tablero))
   ?p <- (partida (turno ?turno))
   =>
   (printout t crlf "    ")

   ; encabezado columnas
   (loop-for-count (?c 1 ?t)
      (if (< ?c 10)
         then (printout t ?c "  ")
         else (printout t ?c " ")
      )
   )
   (printout t crlf "   ")

   ; linea superior
   (loop-for-count (?i 1 (+ (* 3 ?t) 1))
      (printout t "-")
   )
   (printout t crlf)

   ; recorrer filas
   (loop-for-count (?f 1 ?t)
      ; numero de fila
      (if (< ?f 10)
         then (printout t " " ?f " | ")
         else (printout t ?f " | ")
      )

      ; columnas
      (loop-for-count (?c 1 ?t)
         (bind ?simbolo ".")

         (do-for-fact ((?cas casilla))
            (and (= ?cas:fila ?f) (= ?cas:columna ?c))
            
            (if (eq ?cas:estado negra) then (bind ?simbolo "N"))
            (if (eq ?cas:estado blanca) then (bind ?simbolo "B"))
         )

         (printout t ?simbolo "  ")
      )

      (printout t "|" crlf)
   )

   ; linea inferior
   (printout t "   ")
   (loop-for-count (?i 1 (+ (* 3 ?t) 1))
      (printout t "-")
   )
   (printout t crlf)

   ; mostrar turno
   (printout t "Turno: " (upcase ?turno) crlf crlf)

   ; pasar a siguiente fase
   (modify ?j (fase turno-jugador))
) ; para añadir: otro simbolo para indicar movimientos posibles si es el turno del humano.

; Imprimir tablero al principio, antes del turno humano para ver posibles, y después de cada movimiento para ver el resultado. También al final para mostrar el resultado final.



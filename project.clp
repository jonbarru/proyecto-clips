(deftemplate juego
   (slot tamano)
)

(deffacts inicio
   (juego (tamano 0))
)

(defrule pedir-tamano
   ?j <- (juego (tamano 0))
   =>
   (printout t "Introduce el tamaño del tablero (4,6,8,...): " crlf)
   (bind ?t (read))
   (modify ?j (tamano ?t))
)
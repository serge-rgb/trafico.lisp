(require 'cl-glfw3)
(require 'cl-opengl)

(use-package :cl-glfw3)

(defvar *pi* 3.1416)

(defclass pt ()
  ((x
    :initarg :x
    :type single-float)
   (y
    :initarg :y
    :type single-float)))

(defun de-polar (r a)
  "Convierte coord a coordenadas cartesianas."
  ;; (declare (type single-float r a))
  (make-instance 'pt :x (* r (cos a)) :y (* r (sin a))))

(defun dibujar-camino (puntos-polares)
  "Toma un arreglo de puntos polares y lo dibuja"
  (gl:polygon-mode :front-and-back :line)
  (gl:color 0 0 0)
  (gl:begin :polygon)
  (loop for p in puntos-polares do
       (with-slots ((r x) (a y)) p
         (let ((c (de-polar r a)))
           (with-slots (x y) c
             (gl:vertex x y 0)))))
  (gl:end))

(defun generar-camino-circulo (r)
  "Genera un camino circular"
  (loop for i from 0 to (* 2 *pi*) by 0.1 collect
       (make-instance 'pt :x r :y i)))

(defun generar-camino-sinoidal (r)
  "Genera un camino circular"
  (loop for i from 0 to (* 2 *pi*) by 0.1 collect
       (make-instance 'pt :x (* r (sin (* i 2))) :y i)))

(defclass agente ()
  ((camino
    :initarg :camino)
   (resto
    :initarg :resto)
   (tiempo
    :initarg :tiempo)
   (vel
    :initarg :vel)
   (mx
    :initarg :mx)))

(defun nuevo-agente (camino resto vel)
  (make-instance 'agente
                 :camino camino
                 :resto resto
                 :tiempo (get-internal-run-time)
                 :vel vel
                 :mx vel))
;; ===============
(defvar *camino* nil)

(defvar *agentes* nil)

(defun crear-mundo ()
  (setq *camino* (generar-camino-circulo 30))
  ;; (setq *camino* (generar-camino-sinoidal 30))
  (setq *agentes* (loop for i from 0 below 15 collect
                       (nuevo-agente *camino* (subseq *camino* (random 30)) (+ 2 (random 2))))))

(defun aceleracion (agentes)
  (mapcar #'(lambda (a)
              (with-slots (camino resto tiempo vel mx) a
                (let ((nuevo a))
                  (loop for b in agentes do
                       (let ((b-resto (with-slots (resto) b resto)))
                         (let* ((la (length resto))
                                (lb (length b-resto))
                                (diff (- la lb)))
                           (setq nuevo (make-instance 'agente
                                                      :camino camino
                                                      :resto resto
                                                      :tiempo tiempo
                                                      :vel mx
                                                      :mx mx))
                           (if (< 0 diff 6)
                               (cond
                                 ((= diff 5) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto resto
                                                                  :tiempo tiempo
                                                                  :vel 70
                                                                  :mx mx)))
                                ((= diff 4) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto resto
                                                                  :tiempo tiempo
                                                                  :vel 65
                                                                  :mx mx)))
                                 ((= diff 3) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto resto
                                                                  :tiempo tiempo
                                                                  :vel 60
                                                                  :mx mx)))
                                 ((= diff 2) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto resto
                                                                  :tiempo tiempo
                                                                  :vel 50
                                                                  :mx mx)))
                                 ((= diff 1) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto resto
                                                                  :tiempo tiempo
                                                                  :vel 40
                                                                  :mx mx)))
                                 ((= diff 0) (setq nuevo
                                                   (make-instance 'agente
                                                                  :camino camino
                                                                  :resto (subseq resto 5)
                                                                  :tiempo tiempo
                                                                  :vel mx
                                                                  :mx mx))))))))

                  nuevo))) agentes))


(defun agente-actualizado (agente)
  (with-slots (camino resto tiempo vel mx) agente
    (let* ((nuevo (get-internal-run-time))
           (step? (> (- nuevo tiempo) vel))
           (n-resto (if step?
                        (rest resto)
                        resto)))
      (make-instance 'agente
                     :camino camino
                     :resto (if (> vel 0)
                                (if n-resto
                                    n-resto
                                    camino)
                                resto)
                     :tiempo (if step?
                                 nuevo
                                 tiempo)
                     :vel vel
                     :mx mx))))

(defun dibujar-agentes ()
  (gl:color 1 0 0)
  (gl:begin :points)
  (loop for a in *agentes* do
       (with-slots (resto) a
         (with-slots (x y) (first resto)
           (let ((c (de-polar x y)))
             (with-slots (x y) c
               (gl:vertex x y 0))))))
  (gl:end))


(defun print-agentes (agentes)
  (mapcar #'(lambda(a)
              (with-slots (resto) a
                (let* ((pt (first resto)))
                  (with-slots ((px x) (py y)) pt
                    (let ((c (de-polar px py)))
                      (with-slots (x y) c
                        (print (list x y)))))))) agentes)
  nil)

(defun tick ()
  "Funcion principal. Cada cuadro se evalua esto."
  (gl:clear-color 1 1 1 0)
  (gl:clear :color-buffer-bit)
  (gl:point-size 10)
  (setq *agentes* (mapcar #'agente-actualizado *agentes*))
  (setq *agentes* (aceleracion *agentes*))
  (gl:color 0 0 0)
  (dibujar-camino *camino*)
  (dibujar-agentes)
  (swap-buffers))


;; ====== Detalles del sistem de ventanas.
(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defmethod set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback window-size-callback (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun ventana ()
  (crear-mundo)
  (with-init-window (:title "Trafico" :width 800 :height 800)
    (set-key-callback 'key-callback)
    (set-window-size-callback 'window-size-callback)
    (gl:clear-color 1 1 1 0)
    (set-viewport 800 800)
    (loop until (window-should-close-p)
         do (tick)
         do (poll-events))))


;; (ventana)
;; (quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;; Copyright (C) Fausto Almeida <fausto@gia.ist.utl.pt>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cavalo em tabuleiro 6x6
;;;

(in-package :common-graphics-user)

(defstruct cv-estado
  jogadas
  posicao
  tabuleiro)

(defun cv-estado-inicial ()
  (let ((ini
  (make-cv-estado :jogadas 35
		  :posicao (cons 0 0)
                  :tabuleiro (make-array '(6 6)))))
    (setf (aref (cv-estado-tabuleiro ini) 0 0) 0)
    ini))

(defun cv-objectivo? (estado)
  (zerop (cv-estado-jogadas estado)))

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

(defun cv-operador (estado)
  (labels ((pos-valida? (x y)
	     (and (>= x 0) (< x 6)
		  (>= y 0) (< y 6)))
	   (livre (x y array)
	     (not (aref array x y))))
  (let ((pos-x (car (cv-estado-posicao estado)))
	(pos-y (cdr (cv-estado-posicao estado)))
	estados)
    (dolist (el '((1 . -2) (2 . -1) (2 . 1) (1 . 2) (-1 . 2) (-2 . 1) (-2 . -1) (-1 . -2)))
      (let ((new-pos-x (+ pos-x (car el)))
	    (new-pos-y (+ pos-y (cdr el))))
	(when (and (pos-valida? new-pos-x new-pos-y)
		   (livre new-pos-x new-pos-y (cv-estado-tabuleiro estado)))
	  (let ((novo-tabluleiro (copy-array (cv-estado-tabuleiro estado))))
	    (setf (aref novo-tabluleiro new-pos-x new-pos-y) (- 36 (cv-estado-jogadas estado)))
	    (push (make-cv-estado :jogadas (1- (cv-estado-jogadas estado))
				  :posicao (cons new-pos-x new-pos-y)
				  :tabuleiro novo-tabluleiro)
		  
		  estados)))))
    (values estados))))

(defun cv-h (estado)
  (let ((h (cv-estado-jogadas estado))
	(tab (cv-estado-tabuleiro estado)))
    (let ((jogada (1- (- 36 (cv-estado-jogadas estado)))))
      ;;(pprint jogada)
      (when (or
	     (and (aref tab 1 3) (= (aref tab 1 3) (1- jogada)) (not (aref tab 0 5)))
	     (and (aref tab 2 4) (= (aref tab 2 4) (1- jogada)) (not (aref tab 0 5)))
	     (and (aref tab 3 4) (= (aref tab 3 4) (1- jogada)) (not (aref tab 5 5)))
	     (and (aref tab 4 3) (= (aref tab 4 3) (1- jogada)) (not (aref tab 5 5)))
	     (and (aref tab 3 1) (= (aref tab 3 1) (1- jogada)) (not (aref tab 5 0)))
	     (and (aref tab 4 2) (= (aref tab 4 2) (1- jogada)) (not (aref tab 5 0))))
	(setf h most-positive-fixnum)))
    (values h)))


;;; 
;;;
;;; ((1.-2) (2.-1) (2.1) (1.2) (-1.2) (-2.1) (-2.-1) (-2.-2))
;;;   0 1 2 3 4 5
;;; 0 x 8 x 1 x x
;;; 1 7 x x x 2 x
;;; 2 x x C x x x
;;; 3 6 x x x 3 x
;;; 4 x 5 x 4 x x
;;; 5 x x x x x x
;;;

;;; Exemplo PPP
;;; (time (procura (cria-problema (cv-estado-inicial) (list #'cv-operador) :objectivo? #'cv-objectivo? :estado= #'equal) "profundidade" :espaco-em-arvore? T))

;;; Exemplo A*
;;; (time (procura (cria-problema (cv-estado-inicial) (list #'cv-operador) :objectivo? #'cv-objectivo? :estado= #'equal :heuristica #'cv-h) "a*" :espaco-em-arvore? T))

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

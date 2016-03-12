(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;         STRUCTS        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct estado-puzzle
  listaPecas
  totalPecas
  rectanguloTabuleiro
  contadorPecasColocadas
  areaUltimaPecaColocada
  tamanhos)

(defstruct piece
  width
  height
  position
  orientation)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;      MAKE ZONE         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-layout-problem (pieces rectangle)
  (list pieces rectangle))

(defun make-rectangulo (width heigth)
  (list width heigth))

(defun make-pos (x y)
  (list x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;      AUXILIARES        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos-x (pos)
  (first pos))

(defun pos-y (pos)
  (second pos))

(defun equalPosition (pos1 pos2)
  (equal pos1 pos2))


(defun descartar-filho? (filhos peca i)
  (let ((orientacao (if (eq i 0) 'H 'V)))
    (dolist (filho filhos)
      (cond ( (or (and (eq (piece-width filho) (piece-width peca)) (eq (piece-height filho) (piece-height peca))) 
                  (and (eq (piece-width filho) (piece-height peca)) (eq (piece-height filho) (piece-width peca))))
             (cond ( (eq (piece-height filho) (piece-width filho))
                    (return-from descartar-filho? t))
                   ( (eq orientacao (piece-orientation filho))
                    (return-from descartar-filho? t)))))))
  nil)


(defun atualizaTamanhos (estado)
  (let ((ultimaPeca (estado-puzzle-areaUltimaPecaColocada estado))
        (contador 0)
        (lastTamanho (first (estado-puzzle-tamanhos estado)) ))
    (dolist (tamanho (estado-puzzle-tamanhos estado))
      (if (not (eq tamanho lastTamanho)) (incf contador))
      (cond ((eq ultimaPeca tamanho)
             (setf (estado-puzzle-tamanhos estado) (remove tamanho (estado-puzzle-tamanhos estado) :count 1 ))
             (return-from atualizaTamanhos contador)))
      (setf lastTamanho tamanho))
    0))

(defun iniTamanhos (pecas)
  (let ((tamanhos)
        (area))
    (dolist (peca pecas)
      (setf area (* (piece-width peca) (piece-height peca)))
      (setf tamanhos (concatenate 'list tamanhos (list area))))
    (sort tamanhos #'>)))

(defun ildsParte (estado)
  (let ((total (estado-puzzle-totalPecas estado))
        (offset 0)
        (particao 0))
    (setf particao (floor total 2))
    (setf offset (- total (* particao 2)))
    (cons particao offset)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   MODELACAO DO PROBLEMA  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun puzzle-estado-inicial (pecas rectanguloArg)
  (let ((largura (pos-x rectanguloArg))
        (altura (pos-y rectanguloArg)))
    
    (make-estado-puzzle  :listaPecas pecas
                         :totalPecas (list-length pecas)
                         :rectanguloTabuleiro (make-array (list largura altura))
                         :contadorPecasColocadas 0
                         :areaUltimaPecaColocada 0
                        :tamanhos (iniTamanhos pecas))))

(defun puzzle-objetivo? (estado)
  (zerop (- (estado-puzzle-totalPecas estado) (estado-puzzle-contadorPecasColocadas estado))))

(defun puzzle-operador (estado)
  ;(print (estado-puzzle-rectanguloTabuleiro estado))
  (let ((estados)
        (indexPeca -1)
        (tamanhoLista (estado-puzzle-totalPecas estado))
        (filhos ))
    (dolist (peca (estado-puzzle-listaPecas estado))
      (setf indexPeca (+ 1 indexPeca))
      (dotimes (i 2)
        (when (not (descartar-filho? filhos peca i))
        (let ((novaPosicao (getNovaPos peca (estado-puzzle-rectanguloTabuleiro estado) i)))
          (when (not (eq novaPosicao nil))
            (setf filhos (cons (make-piece : width (piece-width peca)
                                : height (piece-height peca)
                                : position novaPosicao
                                : orientation (if (eq i 0) 'H 'V ) ) filhos))
            (let ((novoRectangulo (copy-array (estado-puzzle-rectanguloTabuleiro estado)))
                  (novaLista (atualizaPeca (estado-puzzle-listaPecas estado) indexPeca novaPosicao i)))
              (push (make-estado-puzzle :listaPecas novaLista
                                        :totalPecas tamanhoLista
                                        :rectanguloTabuleiro (atualizaRetangulo peca 
                                                                                novaPosicao
                                                                                novoRectangulo
                                                                                i
                                                                                (estado-puzzle-contadorPecasColocadas estado) )
                                        :contadorPecasColocadas (+ 1 
                                                                   (estado-puzzle-contadorPecasColocadas estado))
                                        :areaUltimaPecaColocada (* (piece-width peca) (piece-height peca))
                                        :tamanhos (copy-list (estado-puzzle-tamanhos estado)))
                    estados)))))))
    (values estados)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Verifica onde e que a nova peca pode     ;
; ser colocada (devolve o ponto lower left);
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun getNovaPos (peca rectangulo indicaOrientacao)
  (cond ((not (eq (piece-position peca) nil))
         (return-from getNovaPos (values nil))))
  
  (let ((larguraPeca (piece-width peca))
        (alturaPeca (piece-height peca))
        (xTemporario)
        (yTemporario)
        (valido? 1)
        (guardaLarguraRect (array-dimension rectangulo 0))
        (guardaAlturaRect (array-dimension rectangulo 1)))
  
    (cond ((eq indicaOrientacao 1)
           (setf alturaPeca (piece-width peca))
           (setf larguraPeca (piece-height peca))))
    
    (dotimes (y (array-dimension rectangulo 1))
      (dotimes (x (array-dimension rectangulo 0))
        (dotimes (x2 larguraPeca)
          (setf xTemporario (+ x x2))
          (cond ((eq xTemporario guardaLarguraRect)
                 (setf valido? nil)
                 (return)))
          (cond ((aref rectangulo xTemporario y)
                 (setf valido? nil)
                 (return)))

          (dotimes (y2 alturaPeca)
            (setf yTemporario (+ y y2))
            (cond ((eq yTemporario guardaAlturaRect)
                   (setf valido? nil)
                   (return)))
            
            (cond ((aref rectangulo xTemporario yTemporario)
                   (setf valido? nil)
                   (return)))))
        
        (cond ((eq valido? 1)
               (return-from getNovaPos (values (list x y)))))
        
        (setf valido? 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Atualiza a posicao da peca          ;
;    Devolve uma nova lista de pecas       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atualizaPeca (listaPecas indexPeca posicao i)
  (let ((novaListaPecas))
    (dolist (elem listaPecas)
      (setf novaListaPecas (concatenate 'list novaListaPecas 
                             (list(make-piece : width (piece-width elem)
                                              : height (piece-height elem)
                                              : position (piece-position elem)
                                              : orientation (piece-orientation elem))))))
    
    (let ((novaPeca (nth indexPeca novaListaPecas)))
      (setf (piece-position novaPeca) posicao)
      (cond ((eq i 0)
             (setf (piece-orientation novaPeca) 'H))
            (t (setf (piece-orientation novaPeca) 'V)))
      (values novaListaPecas))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Atualiza retangulo                  ;
;    Devolve um novo retangulo             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atualizaRetangulo (peca novaPosicao retangulo indicaOrientacao colocadas)
  (let ((larguraPeca (piece-width peca))
        (alturaPeca (piece-height peca)))
    
    (cond ((eq indicaOrientacao 1)
           (setf alturaPeca (piece-width peca))
           (setf larguraPeca (piece-height peca))))

    (dotimes (x larguraPeca)
      (dotimes (y alturaPeca)
        (setf (aref retangulo (+ (first novaPosicao) x) (+ (second novaPosicao) y)) colocadas ))))
  (values retangulo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Heuristica 1                 ;
;  Contar NIL a volta de 1 (baixo e lado)  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-conta-buracos (estado)
  (let ((rectangulo (estado-puzzle-rectanguloTabuleiro estado))
        (contaBuracos 0)
        (nilsNumaLinha 0)
        (xNovo)
        (yNovo)
        (valorFinal 0))
    (let ((larguraRect (array-dimension rectangulo 0))
          (alturaRect (array-dimension rectangulo 1)))
      
  
      (dotimes (y alturaRect)
        (setf nilsNumaLinha 0)
        (dotimes (x larguraRect)
          (cond ((eq (aref rectangulo x y) 1)
                 (setf xNovo (- x 1))
                 (cond ((>= xNovo 0)
                        (cond ((eq (aref rectangulo xNovo y) nil)
                               (incf contaBuracos)))))
                 (setf xNovo (+ x 1))
                 (cond ((< xNovo larguraRect)
                        (cond ((eq (aref rectangulo xNovo y) nil)
                               (incf contaBuracos)))))
                 (setf yNovo (- y 1))
                 (cond ((>= yNovo 0)
                        (cond ((eq (aref rectangulo x yNovo) nil)
                               (incf contaBuracos))))))
                (t
                 (incf nilsNumaLinha))))
        (cond ((eq nilsNumaLinha larguraRect)
               (setf valorFinal (- contaBuracos (* 1 (+ (estado-puzzle-contadorPecasColocadas estado) 1))))
               (return-from h-conta-buracos (values valorFinal)))))
      
      (setf valorFinal (- contaBuracos (* 1 (+ (estado-puzzle-contadorPecasColocadas estado) 1))))
      (values valorFinal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Heuristica 2                 ;
;  Colocar primeiro as peças maiores dim   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 
(defun h-coloca-primeiro-peca-maior (estado)
  (let ((rectangulo (estado-puzzle-rectanguloTabuleiro estado))
       (areaUltimaPeca (estado-puzzle-areaUltimaPecaColocada estado))
       (contaOcupados 0)
        (nilsNumaLinha 0)
        (numLinhasOcupadas 0)
        (valorFinal 0)
        (maxima 0)
        (bonus 0))
    (let ((larguraRect (array-dimension rectangulo 0))
          (alturaRect (array-dimension rectangulo 1)))
      
      (dotimes (y larguraRect)
        (setf nilsNumaLinha 0)
        (setf contaOcupados 0)
        (dotimes (x alturaRect)
          (cond ((not (eq (aref rectangulo y x) nil))
                 (incf contaOcupados))
                (t
                 (incf nilsNumaLinha))))
        (cond ((eq contaOcupados alturaRect)
               (incf numLinhasOcupadas)))
        (cond ((eq nilsNumaLinha alturaRect)
               (setf maxima y)
               (return))))
      
      (cond ((eq (- maxima numLinhasOcupadas) 0)
             (setf bonus 5000)))
      
      
      (setf valorFinal (- 0 (+ (* 1 numLinhasOcupadas) areaUltimaPeca )))
      
      (values valorFinal))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Heuristica 3                 ;
;  Colocar primeiro as peças maiores dim   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-coloca-primeiro-peca-maior-positivo (estado)
    (let ((rectangulo (estado-puzzle-rectanguloTabuleiro estado))
       (contaOcupados 0)
        (nilsNumaLinha 0)
        (numLinhasOcupadas 0)
        (valorFinal 0)
        (maxima 0)
          (bonus nil)
          (resultado)
          (atual))
    (let ((larguraRect (array-dimension rectangulo 0))
          (alturaRect (array-dimension rectangulo 1)))
      
      (dotimes (y larguraRect)
        (setf nilsNumaLinha 0)
        (setf contaOcupados 0)
        (dotimes (x alturaRect)
          (cond ((not (eq (aref rectangulo y x) nil))
                 (incf contaOcupados))
                (t
                 (incf nilsNumaLinha))))
        (cond ((eq contaOcupados alturaRect)
               (incf numLinhasOcupadas)))
        (cond ((eq nilsNumaLinha alturaRect)
               (setf maxima y)
               (return))))
      
      (cond ((eq (- maxima numLinhasOcupadas) 0)
             (setf bonus t)))
      
      (setf atual (atualizaTamanhos estado))
      
      (setf resultado (+ atual (- (estado-puzzle-totalPecas estado) (estado-puzzle-contadorPecasColocadas estado))))
      
      (setf valorFinal (if bonus 0 resultado ))
      (setf valorFinal resultado)
      
      (values valorFinal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Heuristica 4                 ;
;  Escolhe o que tem menos buracos         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-menos-buracos-primeiro (estado)
    (let ((rectangulo (estado-puzzle-rectanguloTabuleiro estado))
       (contaOcupados 0)
        (nilsNumaLinha 0)
        (numLinhasOcupadas 0)
        (valorFinal 0)
        (maxima 0)
          (resultado)
          (abaixo nil)
          (contaBuracos 0)
          (bonus nil)
          (ytemp 0))
    (let ((larguraRect (array-dimension rectangulo 0))
          (alturaRect (array-dimension rectangulo 1)))
      
      (dotimes (y larguraRect)
        (setf nilsNumaLinha 0)
        (setf contaOcupados 0)
        (dotimes (x alturaRect)
          (cond ((not (eq (aref rectangulo y x) nil))
                 (setf ytemp y)
                 (setf abaixo (aref rectangulo y x))
                 (while (>= (- ytemp 1) 0)
                   (setf abaixo (aref rectangulo (- ytemp 1) x))
                   (if (not abaixo) (incf contaBuracos) (return))
                   (decf ytemp)
                   )
                   (incf contaOcupados))
                (t
                 (incf nilsNumaLinha))))
        (cond ((eq contaOcupados alturaRect)
               (incf numLinhasOcupadas)))
        (cond ((eq nilsNumaLinha alturaRect)
               (setf maxima y)
               (return))))
      
      (cond ((eq (- maxima numLinhasOcupadas) 0)
             (if bonus maxima)))
      
      
      (setf resultado (+ contaBuracos (- (estado-puzzle-totalPecas estado) (estado-puzzle-contadorPecasColocadas estado))))
      ;(setf resultado (+ contaBuracos (- larguraRect numLinhasOcupadas)))
      (setf valorFinal resultado)
      
      (values valorFinal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Resolve o problema                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun place-pieces (problema strategy)
  (let ((sol))
  (cond ((or (string-equal strategy "a*.best.heuristic") (string-equal strategy "ILDS") (string-equal strategy "best.approach.satisfaction") (string-equal strategy "best.approach.optimization"))
         (if (setf sol (first(last(first(procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                :estado= #'equal :heuristica #'h-coloca-primeiro-peca-maior-positivo)
                                                 strategy :espaco-em-arvore? T))))) (estado-puzzle-listaPecas sol) nil))
        ((string-equal strategy "best.approach.optimization")
         (if (setf sol (first(last(first(procura (cria-problema (puzzle-estado-inicial (first problema) (list (first (second problema)) 999)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                :estado= #'equal :heuristica #'h-coloca-primeiro-peca-maior-positivo)
                strategy :espaco-em-arvore? T))))) (estado-puzzle-listaPecas sol) nil))
        ((string-equal strategy "a*.best.alternative.heuristic")
         (if (setf sol (first (last(first(procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                :estado= #'equal :heuristica #'h-menos-buracos-primeiro)
                                                  strategy :espaco-em-arvore? T))))) (estado-puzzle-listaPecas sol) nil))
        ((or (string-equal strategy "iterative.sampling.optimization") (string-equal strategy "alternative.approach.optimization"))
         (if (setf sol (first (last(first(procura (cria-problema (puzzle-estado-inicial (first problema) (list (first (second problema)) 999)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                :estado= #'equal)
                        strategy :espaco-em-arvore? T))))) (estado-puzzle-listaPecas sol) nil))
        (t
         (if (setf sol (first (last(first(procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                :estado= #'equal)
                 strategy :espaco-em-arvore? T))))) (estado-puzzle-listaPecas sol) nil)))))


(defun devolve-profundidade (problema) 
  (return-from devolve-profundidade (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                               :estado= #'equal) "profundidade" :espaco-em-arvore? T)))

(defun devolve-a* (problema)
  (return-from devolve-a* (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema))
                                                  (list #' puzzle-operador)
                                                  :objectivo? #'puzzle-objetivo?
                                                  :estado= #'equal :heuristica #'h-menos-buracos-primeiro) "a*"
                                   :espaco-em-arvore? T)))

(defun devolve-largura (problema) 
  (return-from devolve-largura (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                                       :estado= #'equal) "largura" :espaco-em-arvore? nil)))

(defun devolve-profundidade-iterativa (problema) 
  (return-from devolve-profundidade-iterativa (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                                                      :estado= #'equal) "profundidade-iterativa" :espaco-em-arvore? T)))

(defun devolve-ida* (problema) 
  (return-from devolve-ida* (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                               :estado= #'equal :heuristica #'h-menos-buracos-primeiro) "ida*" :espaco-em-arvore? T)))

(defun devolve-sampling (problema)
  (return-from devolve-sampling (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                                        :estado= #'equal) "iterative-sampling" :espaco-em-arvore? T)))


(defun devolve-ilds (problema) 
  (return-from devolve-ilds (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                                    :estado= #'equal :heuristica #'h-menos-buracos-primeiro) "ilds" :espaco-em-arvore? T)))


(defun devolve-ilds-estagios (problema) 
  (return-from devolve-ilds-estagios (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                                             :estado= #'equal :heuristica #'h-coloca-primeiro-peca-maior-positivo) "ilds-estagios" :espaco-em-arvore? T)))

(defun devolve-ilds-meio (problema) 
  (return-from devolve-ilds-meio (procura (cria-problema (puzzle-estado-inicial (first problema) (second problema)) 
                                                        (list #'puzzle-operador) 
                                                        :objectivo? #'puzzle-objetivo? 
                                               :estado= #'equal :heuristica #'h-coloca-primeiro-peca-maior-positivo) "ilds-meio" :espaco-em-arvore? T)))



(defun stepRes (estado n)
  (print (h-coloca-primeiro-peca-maior-positivo estado))
  (dolist (el (puzzle-operador estado)) (print(h-coloca-primeiro-peca-maior-positivo el))) 
  (nth n (puzzle-operador estado)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Procuras                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun iterative-sampling (problema)
  (let ((objectivo? (problema-objectivo? problema))
        (sol nil)
        (comecou (get-universal-time)))
    
      (labels ((procura-samp (estado caminho)
	       ;(block procura-samp
		 
                      (cond ((funcall objectivo? estado) 
                             (list estado))
                      (t 
                        (let((sucessores (problema-gera-sucessores problema estado)))
                          ;(cond ((eq sucessores nil) (procura-samp (problema-estado-inicial problema) nil))
                          (cond ((eq sucessores nil) nil)
                                (t
			          (let ((solucao (procura-samp (nth (random (list-length sucessores)) sucessores) (cons estado caminho))))
			             (when solucao
                                        (return-from procura-samp (cons estado solucao)))))))))))
        (while (and (eq sol nil) (< (- (get-universal-time) comecou) 300))
        ;(while (eq sol nil)
          (setf sol (procura-samp (problema-estado-inicial problema) nil)))
        sol)))



(defun ilds (problema)
  (let ( (objectivo? (problema-objectivo? problema))
        (heuristica (problema-heuristica problema))
        (ini (problema-estado-inicial problema))
        (comecou (get-universal-time))
        (discrepancias  -1)
        (sol nil))
    (labels ((procura-ilds (estado caminho k depth)
                           (cond 
                            ((eq estado nil) nil)
                            ((> (- (get-universal-time) comecou) 300) (return-from procura-ilds nil))
                            ((funcall objectivo? estado)
                                  (list estado))
                                 (t
                                  (let((sucessores (problema-gera-sucessores problema estado))
                                       (dicionario))
                                    (cond ((eq sucessores nil) nil)
                                          (t
                                           (dolist (el sucessores)
                                             (setf dicionario (concatenate 'list dicionario (list (cons el (funcall heuristica el))))))
                                           (setf dicionario (sort dicionario #'(lambda (x y) (< (cdr x) (cdr y)))))
                                           (dotimes (i (+ k 1))
                                             (cond ( (>= (- depth 1) (- k i))
                                             (let ((solucao (procura-ilds (car (nth i dicionario)) (cons estado caminho) (- k i) (- depth 1))))
                                               (when solucao
                                                 (return-from procura-ilds (cons estado solucao))))))))))))))
    (while (and (eq sol nil) (< (- (get-universal-time) comecou) 300))
      (incf discrepancias)
      (setf sol (procura-ilds ini nil discrepancias (estado-puzzle-totalPecas ini))))
      sol)))



(defun ilds-estagios (problema)
    (let ( (objectivo? (problema-objectivo? problema))
        (heuristica (problema-heuristica problema))
        (ini (problema-estado-inicial problema))
        (comecou (get-universal-time))
          (discrepancias  -1)
          (particao (ildsParte (problema-estado-inicial problema)))
          (sol nil))
      (labels ((procura-ilds-d (estado caminho k depth)
                           (cond 
                            ((eq estado nil)
                             nil)
                            ((> (- (get-universal-time) comecou) 300) (return-from procura-ilds-d nil))
                            ((funcall objectivo? estado)
                             (list estado))
                            ((eq depth 0)
                             (cond ( (eq k 0)
                                    (let ((solA nil))
                                 (setf k -1)
                                 (setf depth (car particao))
                                      (while (and (< (- (get-universal-time) comecou) 300) (<= k 3))
                                   (incf k)
                                   (setf solA (procura-ilds-d estado caminho k (+ depth 1)))
                                   (when solA
                                     (return-from procura-ilds-d solA)))
                                    nil))
                                   (t
                                    nil)))
                            (t
                                  (let((sucessores (problema-gera-sucessores problema estado))
                                       (dicionario))
                                    (cond ((eq sucessores nil) nil)
                                          (t
                                           (dolist (el sucessores)
                                             (setf dicionario (concatenate 'list dicionario (list (cons el (funcall heuristica el))))))
                                           (setf dicionario (sort dicionario #'(lambda (x y) (< (cdr x) (cdr y)))))
                                           (dotimes (i (+ k 1))
                                             (let ((solucao (procura-ilds-d (car (nth i dicionario)) (cons estado caminho) (- k i) (- depth 1))))
                                               (when solucao
                                                 (return-from procura-ilds-d (cons estado solucao))))))))))))
            (while (and (eq sol nil) (< (- (get-universal-time) comecou) 300))
              (incf discrepancias)
              (setf sol (procura-ilds-d ini nil discrepancias (+ (car particao) (cdr particao)))))
      sol)))




(defun ilds-meio (problema)
    (let ( (objectivo? (problema-objectivo? problema))
        (heuristica (problema-heuristica problema))
        (ini (problema-estado-inicial problema))
        (comecou (get-universal-time))
          (discrepancias  -1)
          (particao (ildsParte (problema-estado-inicial problema)))
          (sol nil))
      (labels ((procura-ilds-d (estado caminho k depth)
                           (cond 
                            ((eq estado nil)
                             nil)
                            ((> (- (get-universal-time) comecou) 300) (return-from procura-ilds-d nil))
                            ((funcall objectivo? estado)
                             (list estado))
                            ((eq depth 0)
                             (cond ( (eq k 0)
                                    (let ((solA nil))
                                      (setf depth (car particao))
                                      (setf solA (procura-ilds-d estado caminho k (+ depth 1)))
                                      (when solA
                                        (return-from procura-ilds-d solA))
                                    nil))
                                   (t
                                    nil)))
                            (t
                                  (let((sucessores (problema-gera-sucessores problema estado))
                                       (dicionario))
                                    (cond ((eq sucessores nil) nil)
                                          (t
                                           (dolist (el sucessores)
                                             (setf dicionario (concatenate 'list dicionario (list (cons el (funcall heuristica el))))))
                                           (setf dicionario (sort dicionario #'(lambda (x y) (< (cdr x) (cdr y)))))
                                           (dotimes (i (+ k 1))
                                             (let ((solucao (procura-ilds-d (car (nth i dicionario)) (cons estado caminho) (- k i) (- depth 1))))
                                               (when solucao
                                                 (return-from procura-ilds-d (cons estado solucao))))))))))))
            (while (and (eq sol nil) (< (- (get-universal-time) comecou) 300))
              (incf discrepancias)
              (setf sol (procura-ilds-d ini nil discrepancias (+ (car particao) (cdr particao)))))
      sol)))




(defun ilds-optimization (problema)
  (let ( (objectivo? (problema-objectivo? problema))
        (heuristica (problema-heuristica problema))
        (ini problema)
        (comecou (get-universal-time))
        (discrepancias  -1)
        (guardaSolucao nil)
        (melhorAltura 999)
        (novaAltura 0)
        (novoRectangulo nil)
        (sol nil)
        (largura 0)
        (areaPecas 0))
    
    (dolist (peca (estado-puzzle-listaPecas (problema-estado-inicial problema)))
      (setf areaPecas (+ areaPecas (* (piece-width peca) (piece-height peca)))))
    
    (labels ((procura-ilds (estado caminho k depth)
                           (cond 
                            ((eq estado nil) nil)
                            ((> (- (get-universal-time) comecou) 300) (return-from procura-ilds nil))
                            ((funcall objectivo? estado)
                                  (list estado))
                                 (t
                                  (let((sucessores (problema-gera-sucessores problema estado))
                                       (dicionario))
                                    (cond ((eq sucessores nil) nil)
                                          (t
                                           (dolist (el sucessores)
                                             (setf dicionario (concatenate 'list dicionario (list (cons el (funcall heuristica el))))))
                                           (setf dicionario (sort dicionario #'(lambda (x y) (< (cdr x) (cdr y)))))
                                           (dotimes (i (+ k 1))
                                             (cond ( (>= (- depth 1) (- k i))
                                             (let ((solucao (procura-ilds (car (nth i dicionario)) (cons estado caminho) (- k i) (- depth 1))))
                                               (when solucao
                                                 (return-from procura-ilds (cons estado solucao))))))))))))))
      (while (< (- (get-universal-time) comecou) 300)
        (incf discrepancias)
        (setf sol (procura-ilds (problema-estado-inicial ini) nil discrepancias (estado-puzzle-totalPecas (problema-estado-inicial ini))))
        (cond ((eq sol nil)
               nil)
              (t
               (setf discrepancias 0)
               (setf novaAltura (getUltimaLinhaNilsOpti (car(last sol))))
               (cond ((> melhorAltura novaAltura)
                      (setf melhorAltura novaAltura)
                      (setf largura (array-dimension (estado-puzzle-rectanguloTabuleiro (problema-estado-inicial problema)) 0))
                      (setf novoRectangulo (list largura (- novaAltura 1)))
                      (setf ini (cria-problema (puzzle-estado-inicial (estado-puzzle-listaPecas (problema-estado-inicial problema)) novoRectangulo) 
                                                        (list #'puzzle-operador)
                                                        :objectivo? #'puzzle-objetivo? 
                                                                        :estado= #'equal))
                      (setf guardaSolucao sol)))
               (cond ((eq (solucaoPerfeita? (car (last sol)) areaPecas) 1)
                      (return))))))
      (if (eq guardaSolucao nil) (return-from ilds-optimization sol) (return-from ilds-optimization guardaSolucao)))))



(defun iterative-sampling-optimization (problema)
  (let ((objectivo? (problema-objectivo? problema))
        (comecou (get-universal-time))
        (guardaSolucao nil)
        (melhorAltura 999)
        (novaAltura 0)
        (sol nil)
        (areaPecas 0))
    
    (dolist (peca (estado-puzzle-listaPecas (problema-estado-inicial problema)))
      (setf areaPecas (+ areaPecas (* (piece-width peca) (piece-height peca)))))
    
    (labels ((procura-samp (estado caminho)
                           (cond ((funcall objectivo? estado) 
                                  (list estado))
                            (t 
                             (let((sucessores (problema-gera-sucessores problema estado)))
                               (cond ((eq sucessores nil) nil)
                                     (t
                                      (let ((solucao (procura-samp (nth (random (list-length sucessores)) sucessores) (cons estado caminho))))
                                        (when solucao
                                          (return-from procura-samp (cons estado solucao)))))))))))
      
      (while (< (- (get-universal-time) comecou) 300)
        (setf sol (procura-samp (problema-estado-inicial problema) nil))
        (cond ((eq sol nil)
               (return)))
        (setf novaAltura (getUltimaLinhaNilsOpti (car(last sol))))
        (cond ((> melhorAltura novaAltura)
               (setf melhorAltura novaAltura)
               (setf guardaSolucao sol)))
        (cond ((eq (solucaoPerfeita2? (car (last sol)) areaPecas melhorAltura) 1)
                      (return))))
      (return-from iterative-sampling-optimization guardaSolucao))))


	  
(defun alternative-optimization (problema)
  (let ((objectivo? (problema-objectivo? problema))
        (comecou (get-universal-time))
        (guardaSolucao nil)
        (melhorAltura 999)
        (novaAltura 0)
        (novoRectangulo nil)
        (sol nil)
        (novoProblema)
        (largura 0)
        (areaPecas 0))
    
    (dolist (peca (estado-puzzle-listaPecas (problema-estado-inicial problema)))
      (setf areaPecas (+ areaPecas (* (piece-width peca) (piece-height peca)))))
    
    (labels ((procura-samp (estado caminho)
                           (cond ((funcall objectivo? estado) 
                                  (list estado))
                                 (t 
                                  (let((sucessores (problema-gera-sucessores problema estado)))
                                    (cond ((eq sucessores nil) nil)
                                          (t
                                           (let ((solucao (procura-samp (nth (random (list-length sucessores)) sucessores) (cons estado caminho))))
                                             (when solucao
                                               (return-from procura-samp (cons estado solucao)))))))))))
      
      (while (< (- (get-universal-time) comecou) 300)
        ;(print "Altura:" )
        ;(print (array-dimension (estado-puzzle-rectanguloTabuleiro (problema-estado-inicial problema))1))
        (setf sol (procura-samp (problema-estado-inicial problema) nil))
        (cond ((eq sol nil)
               nil)
              (t
               (setf novaAltura (getUltimaLinhaNilsOpti (car(last sol))))
               (cond ((> melhorAltura novaAltura)
                      (setf melhorAltura novaAltura)
                      ;(print "NovaMelhor")
                      (print melhorAltura)
                      (setf largura (array-dimension (estado-puzzle-rectanguloTabuleiro (problema-estado-inicial problema)) 0))
                      (setf novoRectangulo (list largura (- novaAltura 1)))
                      (setf novoProblema (cria-problema (puzzle-estado-inicial (estado-puzzle-listaPecas (problema-estado-inicial problema)) novoRectangulo) 
                                                        (list #'puzzle-operador)
                                                        :objectivo? #'puzzle-objetivo? 
                                                        :estado= #'equal))
                      (setf problema novoProblema)
                      (setf guardaSolucao sol)))
               (cond ((eq (solucaoPerfeita? (car (last sol)) areaPecas) 1)
                      (return))))))
      (return-from alternative-optimization guardaSolucao))))


(defun getUltimaLinhaNilsOpti (estado)
  (let ((retangulo (estado-puzzle-rectanguloTabuleiro estado)))
    (let ((largura (array-dimension retangulo 0))
          (altura (array-dimension retangulo 1))
          (nilsNumaLinha 0)
          (contador 0))
    
      (dotimes (y altura)
        (setf nilsNumaLinha 0)
        (incf contador)
        (dotimes (x largura)
          (cond ((not (eq (aref retangulo x y) nil))
                 (return))
                (t
                 (incf nilsNumaLinha))))
        (cond ((eq contador altura)
               ;(print contador)
               (return-from getUltimaLinhaNilsOpti contador)))
        (cond ((eq nilsNumaLinha largura)
               (return-from getUltimaLinhaNilsOpti y))))))
  
  (return-from getUltimaLinhaNilsOpti 999))

(defun solucaoPerfeita? (estado areaPecas)
  (let ((retangulo (estado-puzzle-rectanguloTabuleiro estado)))
    (let ((larg (array-dimension retangulo 0))
          (alt (array-dimension retangulo 1)))
      (let ((areaTotal (* larg alt)))
        (cond ((and (< (- areaTotal larg) areaPecas) (<= areaPecas areaTotal))
               (return-from solucaoPerfeita? 1))
              (t 
               (return-from solucaoPerfeita? nil)))))))

(defun solucaoPerfeita2? (estado areaPecas y)
  (let ((retangulo (estado-puzzle-rectanguloTabuleiro estado)))
    (let ((larg (array-dimension retangulo 0)))
        (cond ((and (< (- (* y larg) larg) areaPecas) (<= areaPecas (* y larg)))
               (return-from solucaoPerfeita2? 1))
              (t 
               (return-from solucaoPerfeita2? nil))))))






(defun procura (problema tipo-procura
		&key (profundidade-maxima most-positive-fixnum)
		     (espaco-em-arvore? nil))
  "Dado um problema e um tipo de procura devolve uma lista com: a
  solucao para o problema (a lista de estados desde o estado inicial
  ate' ao estado final), ou nil caso nao encontre a solucao; tempo
  gasto na procura (em internal-time-units); numero de nos expandidos;
  numero de nos gerados."

  (flet ((faz-a-procura (problema tipo-procura 
			 profundidade-maxima espaco-em-arvore?)
	   ;; Usamos cond em vez de case porque nao sabemos de que
	   ;; package veem os simbolos (o string-equal funciona com o
	   ;; symbol-name do simbolo e e' "case-insensitive")
	   
	   ;; Actualmente, apenas a procura em largura, o A* e o IDA*
	   ;; estao a aproveitar a informacao do espaco de estados ser
	   ;; uma arvore
	   (cond ((string-equal tipo-procura "largura")
		  (largura-primeiro problema 
				    :espaco-em-arvore? espaco-em-arvore?))
		 ((string-equal tipo-procura "profundidade")
		  (profundidade-primeiro problema profundidade-maxima))
		 ((string-equal tipo-procura "profundidade-iterativa")
		  (profundidade-iterativa problema profundidade-maxima))
		 ((string-equal tipo-procura "a*")
    (a* problema :espaco-em-arvore? espaco-em-arvore?))
          		 ((string-equal tipo-procura "a*.best.heuristic")
              (a* problema :espaco-em-arvore? espaco-em-arvore?))
                    		 ((string-equal tipo-procura "a*.best.alternative.heuristic")
                        (ida* problema :espaco-em-arvore? espaco-em-arvore?))
          ((string-equal tipo-procura "iterative-sampling")
           (iterative-sampling problema))
                    ((string-equal tipo-procura "iterative.sampling.satisfaction")
                     (iterative-sampling problema))
                    ((string-equal tipo-procura "best.approach.satisfaction")
                     (ilds problema))
                    ((string-equal tipo-procura "best.approach.optimization")
                     (alternative-optimization problema))
                    ((string-equal tipo-procura "alternative.approach.optimization")
                     (alternative-optimization problema))
                    ((string-equal tipo-procura "iterative.sampling.optimization")
                               (iterative-sampling-optimization problema))
                    ((string-equal tipo-procura "ilds")
                     (ilds problema))
                              ((string-equal tipo-procura "ILDS")
                               (ilds problema))
                 ((string-equal tipo-procura "ilds-estagios")
                  (ilds-estagios problema))
                 ((string-equal tipo-procura "ilds-meio")
		  (ilds-meio problema))
		 ((string-equal tipo-procura "ida*")
		  (ida* problema :espaco-em-arvore? espaco-em-arvore?)))))

    (let ((*nos-gerados* 0)
	  (*nos-expandidos* 0)
	  (tempo-inicio (get-internal-run-time)))
      (let ((solucao (faz-a-procura problema tipo-procura
				    profundidade-maxima
				    espaco-em-arvore?)))
	(list solucao 
	      (- (get-internal-run-time) tempo-inicio)
	      *nos-expandidos*
	      *nos-gerados*)))))


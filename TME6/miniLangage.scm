;
; Semantique denotationnelle d'un mini-lagage imperatif
;
; Cours MI030 - Ananlyse des programmes et sémantiques
;
; Auteur : Jacques.Malenfant@lip6.fr
;

;****************************************************************************
;
; Catégories syntaxiques et syntaxe abstraite
;
; i  in Instruction
; e  in Expression
; be in BExpression
; v  in Variable
; n  in Numeraux
; c  in Chiffre
;
; i  ::= seq i1 i2 | if be i1 i2 | while be i | v := e
; e  ::= e1 + e2 | e1 - e2 | e1 * e2 | e1 / e2 | n | v
; be ::= be1 & be2 | be1 | be2 | e1 < e2 | e1 = e2 | not be2
; n  ::= c | n c
; c  ::= zero | un | deux | trois | quatre | cinq | six | sept | huit | neuf
;
;****************************************************************************

(define (inst? i)
  (or (seq? i) (si? i) (tant-que? i) (affectation? i)))

(define (e? e)
  (or (plus? e) (moins? e) (muliplie-par? e) (divise-par? e)
      (constante? e) (identificateur? e)))

(define (be? be)
  (or (et? be) (ou? be) (plus-petit? be) (egal? be) (non? be)))

(define (n? n)
  (or (compose? n) (simple? n)))

(define (seq? i)            (equal? (vector-ref i 0) 'seq))
(define (si? i)             (equal? (vector-ref i 0) 'si))
(define (tant-que? i)       (equal? (vector-ref i 0) 'tant-que))
(define (affectation? i)    (equal? (vector-ref i 0) 'affectation))

(define (plus? e)           (equal? (vector-ref e 0) 'plus))
(define (moins? e)          (equal? (vector-ref e 0) 'moins))
(define (multiplie-par? e)  (equal? (vector-ref e 0) 'multiplie-par))
(define (divise-par? e)     (equal? (vector-ref e 0) 'divise-par))
(define (constante? e)      (equal? (vector-ref e 0) 'constante))
(define (identificateur? i) (equal? (vector-ref i 0) 'identificateur))

(define (et? be)            (equal? (vector-ref be 0) 'et))
(define (ou? be)            (equal? (vector-ref be 0) 'ou))
(define (plus-petit? be)    (equal? (vector-ref be 0) 'plus-petit))
(define (egal? be)          (equal? (vector-ref be 0) 'egal))
(define (non? be)           (equal? (vector-ref be 0) 'non))

(define (compose? n)        (equal? (vector-ref n 0) 'compose))
(define (simple? n)         (equal? (vector-ref n 0) 'simple))

(define (chiffre? c)        (equal? (vector-ref c 0) 'chiffre))

(define (make-seq i1 i2)           (vector 'seq i1 i2))
(define (make-si be i1 i2)         (vector 'si be i1 i2))
(define (make-tant-que be i)       (vector 'tant-que be i))
(define (make-affectation i e)     (vector 'affectation i e))

(define (make-plus e1 e2)          (vector 'plus  e1 e2))
(define (make-moins e1 e2)         (vector 'moins  e1 e2))
(define (make-multiplie-par e1 e2) (vector 'multiplie-par  e1 e2))
(define (make-divise-par e1 e2)    (vector 'divise-par  e1 e2))
(define (make-constante n)         (vector 'constante n))
(define (make-identificateur s)    (vector 'identificateur s))

(define (make-et be1 be2)          (vector 'et be1 be2))
(define (make-ou be1 be2)          (vector 'ou be1 be2))
(define (make-plus-petit e1 e2)    (vector 'plus-petit e1 e2))
(define (make-egal e1 e2)          (vector 'egal e1 e2))
(define (make-non be)              (vector 'non be))

(define (make-compose n c)         (vector 'compose n c))
(define (make-simple c)            (vector 'simple c))
(define (make-chiffre c)           (vector 'chiffre c))

(define (seq->i1 i)         (vector-ref i 1))
(define (seq->i2 i)         (vector-ref i 2))
(define (si->be i)          (vector-ref i 1))
(define (si->i1 i)          (vector-ref i 2))
(define (si->i2 i)          (vector-ref i 3))
(define (tant-que->be i)    (vector-ref i 1))
(define (tant-que->i i)     (vector-ref i 2))
(define (affectation->i i)  (vector-ref i 1))
(define (affectation->e i)  (vector-ref i 2))

(define (plus->e1 e)          (vector-ref e 1))
(define (plus->e2 e)          (vector-ref e 2))
(define (moins->e1 e)         (vector-ref e 1))
(define (moins->e2 e)         (vector-ref e 2))
(define (multiplie-par->e1 e) (vector-ref e 1))
(define (multiplie-par->e2 e) (vector-ref e 2))
(define (divise-par->e1 e)    (vector-ref e 1))
(define (divise-par->e2 e)    (vector-ref e 2))
(define (constante->n e)      (vector-ref e 1))

(define (et->be1 be)        (vector-ref be 1))
(define (et->be2 be)        (vector-ref be 2))
(define (ou->be1 be)        (vector-ref be 1))
(define (ou->be2 be)        (vector-ref be 2))
(define (plus-petit->e1 be) (vector-ref be 1))
(define (plus-petit->e2 be) (vector-ref be 2))
(define (egal->e1 be)       (vector-ref be 1))
(define (egal->e2 be)       (vector-ref be 2))
(define (non->be be)        (vector-ref be 1))

(define (compose->n n)        (vector-ref n 1))
(define (compose->c n)        (vector-ref n 2))
(define (simple->c n)         (vector-ref n 1))
(define (chiffre->c c)        (vector-ref c 1))
(define (identificateur->s i) (vector-ref i 1))



;****************************************************************************
;
; Domaines et fonctions semantiques
;
; Z = les entiers de Scheme
; T = les booleens de Scheme
; N = les entiers positifs de Scheme
; Env : Identificateur -> N
; Store : N -> Z
; 
;****************************************************************************

(define (Z? z)  (integer? z))
(define (T? t)  (boolean? t))
(define (N? n)  (and (integer? n) (>= n 0)))

(define emptyEnv (lambda (i) 'bot-N))

(define extendEnv
  (lambda (rho)
    (lambda (i)
      (lambda (n)
        (lambda (i1)
          (if (and (identificateur? i1)
                   (equal? (identificateur->s i) (identificateur->s i1)))
            n
            (rho i1)))))))

(define emptyStore (lambda (loc) 'bot-Z))

(define updateStore
  (lambda (sigma)
    (lambda (n)
      (lambda (z)
        (lambda (n1)
          (if (= n n1)
            z
            (sigma n1)))))))


;****************************************************************************
;
; Equations semantiques
;
; execute : Instruction -> Env -> Store -> Store
; eval : Expression -> Env -> Store -> Z
; valeur : Nombre -> Z
; chiffre : Chiffre -> Z
;
;****************************************************************************

(define (execute i)
  (cond ((seq? i)         (execute-seq i))
        ((si? i)          (execute-si i))
        ((tant-que? i)    (execute-tant-que i))
        ((affectation? i) (execute-affectation i))
        (else             (begin
                            (display "erreur : instruction inconnue : ")
                            (display i) (newline)))))

(define (execute-seq i)
  (lambda (rho)
    (lambda (sigma)
      (((execute (seq->i2 i)) rho) (((execute (seq->i1 i)) rho) sigma)))))

(define (execute-si i)
  (lambda (rho)
    (lambda (sigma)
      (if (((beval (si->be i)) rho) sigma)
          (((execute (si->i1 i)) rho) sigma)
          (((execute (si->i2 i)) rho) sigma)))))

(define (execute-tant-que i)
  (lambda (rho)
    (lambda (sigma)
      (let ((loop (fix
		     (lambda (f)
                       (lambda (sigma)
                         (if (((beval (tant-que->be i)) rho) sigma)
                             (f (((execute (tant-que->i i)) rho) sigma))
                             sigma))))))
        (loop sigma)))))

(define (execute-affectation i)
  (lambda (rho)
    (lambda (sigma)
      (((updateStore sigma) (rho (affectation->i i)))
                              (((eval (affectation->e i)) rho) sigma)))))



(define (eval e)
  (cond ((plus? e)           (eval-plus e))
        ((moins? e)          (eval-moins e))
        ((multiplie-par? e)  (eval-multiplie-par e))
        ((divise-par? e)     (eval-divise-par e))
        ((constante? e)      (eval-constante e))
        ((identificateur? e) (eval-identificateur e))
        (else                (begin
                               (display "erreur : expression inconnue : ")
                               (display e) (newline)))))

(define (eval-plus e)
  (lambda (rho)
    (lambda (sigma)
      (+ (((eval (plus->e1 e)) rho) sigma)
         (((eval (plus->e2 e)) rho) sigma)))))

(define (eval-moins e)
  (lambda (rho)
    (lambda (sigma)
      (- (((eval (moins->e1 e)) rho) sigma)
         (((eval (moins->e2 e)) rho) sigma)))))

(define (eval-multiplie-par e)
  (lambda (rho)
    (lambda (sigma)
      (* (((eval (multiplie-par->e1 e)) rho) sigma)
         (((eval (multiplie-par->e2 e)) rho) sigma)))))

(define (eval-divise-par e)
  (lambda (rho)
    (lambda (sigma)
      (quotient (((eval (divise-par->e1 e)) rho) sigma)
                (((eval (divise-par->e2 e)) rho) sigma)))))

(define (eval-constante e)
  (lambda (rho)
    (lambda (sigma)
      (valeur (constante->n e)))))

(define (eval-identificateur e)
  (lambda (rho)
    (lambda (sigma)
      (sigma (rho e)))))



(define (beval be)
  (cond ((et? be)         (beval-et be))
        ((ou? be)         (beval-ou be))
        ((plus-petit? be) (beval-plus-petit be))
        ((egal? be)       (beval-egal be))
        ((non? be)        (beval-non be))
        (else             (begin
                            (display "erreur : expression boolenne inconnue : ")
                            (display be) (newline)))))

(define (beval-et be)
  (lambda (rho)
    (lambda (sigma)
      (and (((beval (et->be1 be)) rho) sigma)
           (((beval (et->be2 be)) rho) sigma)))))

(define (beval-ou be)
  (lambda (rho)
    (lambda (sigma)
      (or (((beval (ou->be1 be)) rho) sigma)
          (((beval (ou->be2 be)) rho) sigma)))))

(define (beval-plus-petit be)
  (lambda (rho)
    (lambda (sigma)
      (< (((eval (plus-petit->e1 be)) rho) sigma)
         (((eval (plus-petit->e2 be)) rho) sigma)))))

(define (beval-egal be)
  (lambda (rho)
    (lambda (sigma)
      (= (((eval (egal->e1 be)) rho) sigma)
         (((eval (egal->e2 be)) rho) sigma)))))

(define (beval-non be)
  (lambda (rho)
    (lambda (sigma)
      (not (((beval (non->be be)) rho) sigma)))))



(define (valeur n)
  (cond ((compose? n) (valeur-compose n))
        ((simple? n)  (valeur-simple n))
        (else         (begin
                        (display "erreur : nombre inconnu : ")
                        (display n) (newline)))))

(define (valeur-compose n)
  (+ (* 10 (valeur (compose->n n)))
     (chiffre (compose->c n))))

(define (valeur-simple n)
  (chiffre (simple->c n)))

(define (chiffre c)
  (cond ((equal? (chiffre->c c) 'zero)   0)
        ((equal? (chiffre->c c) 'un)     1)
        ((equal? (chiffre->c c) 'deux)   2)
        ((equal? (chiffre->c c) 'trois)  3)
        ((equal? (chiffre->c c) 'quatre) 4)
        ((equal? (chiffre->c c) 'cinq)   5)
        ((equal? (chiffre->c c) 'six)    6)
        ((equal? (chiffre->c c) 'sept)   7)
        ((equal? (chiffre->c c) 'huit)   8)
        ((equal? (chiffre->c c) 'neuf)   9)
        (else (begin
                (display "erreur : chiffre inconnu : ")
                (display c) (newline)))))



;****************************************************************************
; fix
;****************************************************************************

(define fix
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;****************************************************************************
;
;La suite ici
;
;****************************************************************************


;; Q1


(define cinq
	(make-simple (make-chiffre 'cinq)))
(define const5
	(make-constante cinq))

(define dix 
 (make-compose (make-simple (make-chiffre 'un)) (make-chiffre 'zero)))
(define const10 
	(make-constante dix))

(define sept
	(make-simple (make-chiffre 'sept)))
(define const7
	(make-constante sept))

(define exp10plus7 (make-plus const10 const7))
(begin (display "exp10plus7 : ")
			 (display (((eval exp10plus7) emptyEnv) emptyStore)) (newline))

 
(define huit
	(make-simple (make-chiffre 'huit)))
(define const8
	(make-constante huit))

(define exp10pluspetit8 
	(make-plus-petit const10 const8))


(begin (display "exp10pluspetit8 : ")
			 (display (((beval exp10pluspetit8) emptyEnv) emptyStore)) (newline))

(define exp10egal8 
	(make-egal const10 const8))

(define infOuEgal 
	(make-ou exp10pluspetit8 exp10egal8))

(begin (display "infOuEgal : ")
			 (display (((beval infOuEgal) emptyEnv) emptyStore)) (newline))


;;Q2

;; while( 0 < i) i:= i -1


;;Q3
; Variable a
(define addr 0)
(define sigma1 (((updateStore emptyStore) addr) 1))
(define id-a (make-identificateur 'a))
(define rho1 (((extendEnv emptyEnv) id-a) addr))
; Variable b
(define addr1 1)
(define sigma2 (((updateStore sigma1) addr1) 2))
(define id-b (make-identificateur 'b))
(define rho2 (((extendEnv rho1) id-b) addr1))

(define id-c (make-identificateur 'c))
(define my-seq 
		(make-plus id-a id-b))

; Variable c
(define addr2 2)
(define sigma3 (((updateStore sigma2) addr2) (((eval my-seq) rho2) sigma2)))

(define rho3 (((extendEnv rho2) id-c) addr2))

(begin (display "a = ")
			 (display (((eval id-a) rho3) sigma3)) (newline)
			 (display "b = ")
			 (display (((eval id-b) rho3) sigma3)) (newline)
			 (display "c = ")
			 (display (((eval id-c) rho3) sigma3)) (newline))


; Variable x
(define addr3 0)
(define sigma4 (((updateStore sigma3) addr3) 10))
(define id-x (make-identificateur 'x))
(define rho4 (((extendEnv rho3) id-x) addr3))
; Variable y
(define addr4 1)
(define sigma5 (((updateStore sigma4) addr4) 100))
(define id-y (make-identificateur 'y))
(define rho5 (((extendEnv rho4) id-y) addr4))

(define ifif
	(make-si 
		(make-plus-petit id-y id-x)
		(make-affectation id-c id-y)
		(make-affectation id-c id-x)
  ))

(begin (display "x > y then c = x else y = y") (newline)
			 (display "c = ")
			 (let ((sigma (((execute ifif) rho5) sigma5)))
				(display (((eval id-c) rho5) sigma)) (newline)))












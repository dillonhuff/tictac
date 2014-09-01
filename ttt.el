;; Utilities
(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun one-of (set)
  (random-elt set))

(defun other-team (team)
  (cond ((eq team 'O) 'X)
	((eq team 'X) 'O)
	(T '(failure invalid input to other-team))))

(defun legal-moves (board)
  (remove-if-not (lambda (pos) (empty? board pos)) (list 0 1 2 3 4 5 6 7 8)))

(defun legal-move? (board move)
  (if (find move (legal-moves board)) T Nil))

;; Game structure
(defun play-game (board team x-player o-player)
  (let* ((current-player (if (eq 'X team) x-player o-player))
	 (player-move (funcall current-player board team))
	 (new-board (place-piece board player-move team)))
    (if (cats-game? new-board)
	(list 'E new-board)
      (if (check-victory new-board team)
	  (list team new-board)
	(play-game new-board (other-team team) x-player o-player)))))

;; Command line GUI for player
(defun board-line (line-num board)
  (subseq board (* 3 line-num) (+ (* 3 line-num) 3)))

(defun show-piece (piece)
  (cond ((eq 'X piece) "X")
	((eq 'O piece) "O")
	((eq 'E piece) " ")
	(T '(failure in show-piece invalid piece type))))

(defun show-line (line)
  (concatenate 'string (show-piece (car line)) " | " (show-piece (cadr line)) " | " (show-piece (caddr line))))

(defun board-line-str (line-num board)
  (show-line (board-line line-num board)))

(defun print-board (board)
  (format *query-io* "~a~%~a~%~a~%~a~%~a~%" (board-line-str 0 board) "__________" (board-line-str 1 board) "__________" (board-line-str 2 board))
  (force-output))

(defun ask-user (query)
  (format *query-io* "~a: " query)
  (force-output *query-io*)
  (read))

(defun print-st (statement)
  (format *query-io* "~a" statement)
  (force-output *query-io*))

(defun gui-tic-tac-toe (x-player o-player)
  (let* ((winner-and-final-board (play-game (new-board) 'X x-player o-player))
	 (winner (car winner-and-final-board))
	 (final-board (cadr winner-and-final-board)))
    (print-board final-board)
    (cond ((eq 'X winner) (print-st "X Wins!"))
	  ((eq 'O winner) (print-st "O Wins!"))
	  ((eq 'E winner) (print-st "Cats game"))
	  (T '(failed in gui-tic-tac-toe invalid game result)))))

;; Some simple player types
(defun random-legal-move (board)
  (let ((leg-moves (legal-moves board)))
    (nth (random (length leg-moves)) leg-moves)))

(defun random-player (board team)
  (random-legal-move board))

(defun human-player (board team)
  (print-board board)
  (ask-user (concatenate 'string "Next move for " (show-piece team))))

;; Board query functions
(defun empty? (board pos)
  (eq (nth pos board) 'E))

(defun friendly? (board pos team)
  (eq (nth pos board) team))

(defun enemy? (board pos team)
  (eq (nth pos board) (other-team team)))

;; Check for victory / cats games
(defvar *tic-tac-positions* '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))

(defun filled? (space)
  (cond ((eq 'X space) T)
	((eq 'O space) T)
	((eq 'E space) Nil)
	(T '(failure in filled? invalid piece type))))

(defun cats-game? (board)
  (every (lambda (p) (filled? p)) board))

(defun check-positions (board pos-list team)
  (every #'identity (map 'list (lambda (pos) (friendly? board pos team)) pos-list)))

(defun check-victory (board team)
  (some #'identity (map 'list (lambda (pos-list) (check-positions board pos-list team)) *tic-tac-positions*)))

;; Board construction and modification
(defun new-board ()
  '(e e e e e e e e e))

(defun set-nth (list n value)
  (fill (copy-seq list) value :start n :end (1+ n)))

(defun place-piece (board pos piece)
  (cond ((or (> pos 8) (< pos 0)) '(failure in place-piece board index out of range))
	((not (eq (nth pos board) 'E)) '(failure in place-piece trying to place on non-empty square))
	(T (set-nth board pos piece))))

;; Randomly generating simple players
(defun rand-pos ()
  (one-of '(0 1 2 3 4 5 6 7 8)))

(defun rand-team ()
  (one-of '(team (other-team team))))

(defun rand-board ()
  (one-of '(board)))

(defun rand-test-func ()
  (one-of '(empty? friendly? enemy?)))

(defun rand-test ()
  (let ((t-func (rand-test-func))
	(team (rand-team))
	(pos (rand-pos))
	(board (rand-board)))
    (if (eq 'empty? t-func)
	(list t-func board pos)
      (list t-func board pos team))))

(defun rand-if ()
  (let ((test (rand-test))
	(p1 (rand-pos))
	(p2 (rand-pos)))
    (list 'if test p1 p2)))

(defun let-bind (code)
  (cons 'let (cons (cons 'next-move (cons code nil)) nil)))

(defun wrap-code-in-let (code)
  (list 'let (cons (cons 'next-move (cons code nil)) nil) '(if (legal-move? board next-move) next-move (random-legal-move board))))

(defun wrap-in-rand-move-lambda (code)
  (list 'lambda '(board team) (wrap-code-in-let code)))

(defun rand-simple-player ()
  (wrap-in-rand-move-lambda (rand-if)))

;; Type system
(defvar *positions* '(0 1 2 3 4 5 6 7 8 9))

(defvar *expr-types*
  '((if . ((bool position position) position))
    (empty? . ((board position) bool))
    (friendly? . ((board position team) bool))
    (enemy? . ((board position team) bool))
    (other-team . ((team) team))))

(defun atom-type (a)
  (cond ((find a *positions*) 'position)
	((eq a 'board) 'board)
	((eq a 'team) 'team)
	(T nil)))

(defun type-signature (comp-expr)
  (cdr (assoc comp-expr *expr-types*)))

(defun compound-type (expr)
  (let* ((type-sig (type-signature (car expr)))
	(arg-types (car type-sig))
	(ret-type (cadr type-sig)))
    (if (every #'identity (mapcar #'eq arg-types (map 'list #'expr-type (cdr expr))))
	ret-type
      nil)))
	
(defun expr-type (expr)
  (cond ((atom expr) (atom-type expr))
	(T (compound-type expr))))

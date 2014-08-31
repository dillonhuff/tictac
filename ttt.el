;; Utilities
(defun other-team (team)
  (cond ((eq team 'O) 'X)
	((eq team 'X) 'O)
	(T '(failure invalid input to other-team))))

(defun legal-moves (board)
  (remove-if-not (lambda (pos) (empty? board pos)) (list 0 1 2 3 4 5 6 7 8)))

;; Game structure
(defun play-game (board team x-player o-player)
  (let* ((current-player (if (eq 'X team) x-player o-player))
	 (player-move (funcall current-player board team))
	 (new-board (place-piece board player-move team)))
    (if (cats-game? board) (list 'E new-board) (if (check-victory new-board team) (list team new-board) (play-game new-board (other-team team) x-player o-player)))))

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
	  (T '(failed int gui-tic-tac-toe invalid game result)))))

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

(defun cats-game? (board)
  (every (lambda (p) (or (eq 'X p) (eq 'O p))) board))

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

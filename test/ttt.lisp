;;; ttt.lisp --- 
;; 
;; Filename: ttt.lisp
;; Description: 
;; Author: Karthik Kumar
;; Maintainer: 
;; Created: Thu Sep  6 12:16:15 2018 (+0530)
;; Version: v0.1
;; Package-Requires: (common-lisp)
;; Last-Updated: Thu Sep  6 19:09:45 2018 (+0530)
;;           By: Karthik Kumar
;;     Update #: 185
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log: 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;
;; Global Variables ;;
;;;;;;;;;;;;;;;;;;;;;;

(defparameter *computer* 10
  "Computer Player")
(defparameter *user* 1
  "User Player")
(defparameter *right-diagonal* '(1 5 9))
(defparameter *left-diagonal* '(3 5 7))
(defparameter *corners* '(1 3 7 9)
  "Corners of the board.")
(defparameter *sides* '(2 4 6 8)
  "Sides of the squares")

(defparameter *triplets* `((1 2 3) (4 5 6) (7 8 9) ; Horizontal triplets
						   (1 4 7) (2 5 8) (3 6 9) ; Vertical triplets
						   ,*right-diagonal*
						   ,*left-diagonal*) ;Diagonal triplets
  "Eight possible combinations to make three-in-a-row")


;;;;;;;;;;;;;;;;;;;;;
;; Core Functions  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun make-board ()
  "Creates a board. board symbol followed by nine numbers. 0 - empty; 1 - O; 10 - X"
  (list 'board
		0 0 0
		0 0 0
		0 0 0))


(defun number-to-player (element)
  "Converts the numbers in the board to Player symbols. 0 - empty; 1 - O; 10 - X"
  (cond ((equal element 1) "O")
		((equal element 10) "X")
		(t " ")))

(defun print-row (x y z)
  "Prints single row by calling number-to-player function."
  (format t "~&  ~A | ~A | ~A"
		  (number-to-player x)
		  (number-to-player y)
		  (number-to-player z)))


(defun print-board (board)
  "Prints the entire board. Utilizing print-row function."
  (format t "~%")
  (print-row (nth 1 board)
			 (nth 2 board)
			 (nth 3 board))
  (format t "~& -----------")
  (print-row (nth 4 board)
			 (nth 5 board)
			 (nth 6 board))
  (format t "~& -----------")
  (print-row (nth 7 board)
			 (nth 8 board)
			 (nth 9 board))
  (format t "~%~%"))


(defun make-move (player position board)
  "Destructively changing one of the board positions."
  (setf (nth position board) player)
  board)


(defun tcustom-board-create (board custom-positions-list size) 
  "For testing - Creates custom configuration of board using the list given."
  (let ((cnt (- (length board) size)))
	(cond ((zerop size) nil)
		  (t (make-move (first custom-positions-list)
						cnt
						board)
			 (tcustom-board-create board
								   (rest custom-positions-list)
								   (- size 1))))))


(defun triplet-sum (board triplet)
  "Calculates the single row."
  (+ (nth (first triplet)
		  board)
	 (nth (second triplet)
		  board)
	 (nth (third triplet)
		  board)))


(defun board-sum (board)
  "Finds the sum of entire board."
  (mapcar #'(lambda (triplet)
			  (triplet-sum board triplet))
		  *triplets*))


(defun winner-p (board)
  "Predicate checks if there is a winner. if any triplet = (3 or 30) => winner."
  (let ((sum (board-sum board)))
	(or (member (* 3 *computer*)
				 sum)
		(member (* 3 *user*)
				sum))))

(defun board-full-p (board)
  "Checks if there are no empty spaces left on the board."
  (not (member 0 board)))


(defun read-a-legal-move (board)
  "Checks whether the entered move is legal."
  (format t "~&Enter your move: ")
  (let ((position (read)))
	(cond ((not (and (integerp position)
					 (<= 1 position 9)))
		   (format t "~&Invalid move...")
		   (read-a-legal-move board))
		  ((not (zerop (nth position board)))
		   (format t "~&That space is already occupied...")
		   (read-a-legal-move board))
		  (t position))))

(defun user-move (board)
  "Gets move from user."
  (let* ((position (read-a-legal-move board))
		 (new-board (make-move *user*
							   position
							   board)))
	(print-board new-board)
	(cond ((winner-p new-board)
		   (format t "~&You win!"))
		  ((board-full-p new-board)
		   (format t "~&Game is a tie..."))
		  (t (computer-move new-board)))))


(defun find-empty-position (board triplet)
  "Finds whether the given triplet is empty or not."
  (find-if #'(lambda (position)
			   (zerop (nth position board)))
		   triplet))


(defun find-win-or-block-position (board target-sum)
  "Finds the winning or blocking position."
  (let ((potential-triplet (find-if #'(lambda (triplet)
										(equal (triplet-sum board triplet)
											   target-sum))
									*triplets*)))
	(when potential-triplet
	  (find-empty-position board potential-triplet))))


(defun winning-move (board)
  "Makes a move to win the opponent."
  (let ((position (find-win-or-block-position board
											  (* 2 *computer*))))
	(and position
		 (list position
			   "make three in a row."))))


(defun block-opponent-move (board)
  "Blocks the opponent."
  (let ((position (find-win-or-block-position board
											  (* 2 *user*))))
	(and position
		 (list position
			   "Blocking the opponent."))))


(defun pick-random-empty-position (board)
  "Picks a random number from 1 to 9."
  (let ((position (+ 1 (random 9))))
	(if (zerop (nth position board))
		position
		(pick-random-empty-position board))))

(defun random-move (board)
  "Makes a random move in the board."
  (list (pick-random-empty-position board)
		"random-move"))


(defun choose-best-move (board)
  "Picks the best mode and gives the strategy also."
  (or (winning-move board)
	  (block-opponent-move board)
	  (squeeze-move board)
	  (random-move board)))


(defun computer-move (board)
  "Performs move from computer."
  (let* ((best-move (choose-best-move board))
		 (position (first best-move))
		 (strategy (second best-move))
		 (new-board (make-move *computer*
							   position
							   board)))
	
	(format t "~&My move: ~A" position)
	(format t "~&My strategy: ~A~%" strategy)
	(print-board new-board)

	(cond ((winner-p new-board)
		   (format t "~&I win!"))
		  ((board-full-p new-board)
		   (format t "~&Game is a tie..."))
		  (t (user-move new-board)))))


(defun play-one-game ()
  "Initiates the game play."
  (if (y-or-n-p "Would you like to go first? ")
	  (user-move (make-board))
	  (computer-move (make-board))))


(defun squeeze-play-p (board)
  "Checks the diagonals for an O-X-O pattern."
  (if (or (equal (sum-triplets board
								  *right-diagonal*)
					(+ (* 2 *user*)
					   *computer*))
			 (equal (sum-triplets board
								  *left-diagonal*)
					 (+ (* 2 *user*)
						*computer*)))
	  (find-empty-position board *sides*)))



			   

(defun squeeze-play-p (board target-sum)
  "Predicate that checks whether squeeze play has been initiated."
  (let ((triplet (find ))))
  
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Testing the game: ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setf b (make-board))
(tcustom-board-create b
					  '(1 1 0
						0 10 0
						0 0 1)
					  9)
(print-board b)
(board-sum b)
(winner-p b)
(setf pos (squeeze-play-p b))

(defun tmake-move (player position board)
  "testing make move function"
  (setf (nth position board) player)
  board)

(setf e (tmake-move *user* 6 b))


;;;;;;;;;;;;;;;;;;;;
;; End of testing ;;
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ttt.lisp ends here

;;; The Monty Hall problem.
;;; 
;;; Supposed you're on a game show, and you're given the choice of three doors:
;;; Behind one door is a car; behind the others, goats. You pick a door, say No 1.,
;;; and the host, who knows what's behind the doors, opens another door, say
;;; No. 3, which has a goat. He then says to you, "Do you want to pick door No.2?"
;;; Is it to your advantage to switch your choice? - Wikipedia
;;; 

(defpackage #:monty-hall
  (:use #:cl)
  )

(defvar *doors*)
(defvar *choice*)
(defvar *winning-count*)
(defvar *switch*)

(defvar *number-of-simulation-runs* 100)

(defun run-simulation ()
  "Run the Monty-Hall problem simulation."
  (reset-simulation)
  (disable-switching)
  (start-simulation)

  (reset-simulation)
  (enable-switching)
  (start-simulation)
  )

(defun enable-switching ()
  "Sets switching variable to T."
  (setf *switch* T)
  )

(defun disable-switching ()
  "Sets switching variable to nil."
  (setf *switch* '()) 
  )

(defun start-simulation ()
  "Repeats the game show *number-of-simulation-runs* times. Counts number of winning plays."
  (dotimes (n  *number-of-simulation-runs* )
    (game-show)
    (if (string= *choice* "Car")
      (setf *winning-count* (+ *winning-count* 1))
      ) 
    )
  (let ((winning-percentage (* (/ *winning-count* *number-of-simulation-runs*) 100)))
    (if *switch*
      (format t "~&Winning percentage when switching: ~$%~&" winning-percentage)
      (format t "~&Winning percentage when not switching: ~$%~&" winning-percentage) 
      )
    ) 
  )

(defun reset-simulation ()
  "Resets global variable *winning-count* to 0."
  (setf *winning-count* 0)
  )

(defun game-show()
  "Runs one instance of the game show. Simulating putting Goats and a Car behind the doors."
  (setf *doors* (prepare-doors '("Goat 1" "Car" "Goat 2")))
  (setf *choice* (select-first-door))
  (host-opens-first-door)
  (if *switch*
    (change-door)
    )
  )     

(defun prepare-doors (doors)
  "Randomized creation of doors and their contents."
  (let  ((iterations (+ (random 3) 1)))
    (labels ((prepare-door (doors iterations)
               (if (zerop iterations) 
                 doors
                 (prepare-door (append (cdr doors) (list (first doors)))  (- iterations 1))
                 )
               )
             )
      (prepare-door doors iterations))
    )
  )

(defun select-first-door ()
  "Contestant make his/her first selection of a door."
  (elt *doors* (+ (random 2) 1))
  )

(defun host-opens-first-door ()
  "The host of the game show opens a door with a goat behind it."
  (if (string= "Goat 1" *choice*)
    (setf *doors* (remove "Goat 2" *doors*))
    (setf *doors* (remove "Goat 1" *doors*))
    )
  )

(defun change-door ()
  "Contestant changes door."
  (if (= (position *choice* *doors*) 1)
    (setf *choice* (elt *doors* 0))
    (setf *choice* (elt *doors* 1))
    ) 
  )


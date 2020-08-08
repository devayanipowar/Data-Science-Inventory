

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

 (defparameter *Goal* '( 1 2 3 4 5 6 7 8 9 8))      

(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
		       (list (position 9 initial-puzzle-situation)))
	  nil))



(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot1 (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt (first ,puzzle) 9))
(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot1 ,puzzle)))
       ,@body))


(defun make-move (move puzzle)
  ;; use to make successor by using valid moves
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot1 puzzle))))
        (when (find move moves) (swap move (empty-slot1 puzzle) (first puzzle)))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setq ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))




;num-out-of-place First count the tiles that are out of place and enqueue according 
(defun num-out-of-place (state)
  (first(last (let ((count 0))
	      (mapcar (lambda (a)   
			     (if (equal a (elt  state a)) (incf count))count)
				    *Goal*
				    )))))


(defun goal-p (state)
    "Returns T if state is a goal state, else NIL.  Our goal test."
    (if (equalp state *Goal*)
	t))




(defun dfs-enqueuer (state queue)
  "Enqueues in depth-first order"
(setf queue (make-empty-queue))
  (enqueue-at-end queue state)
 queue)


  


(defun bfs-enqueuer (state queue)
    "Enqueues in breadth-first order"
(setf queue (make-empty-queue))
  (enqueue-at-front queue (first state))
 queue
	;; IMPLEMENT ME
)                                         



(defun num-out-enqueuer (state queue)
  "Enqueues by number of tiles out of place"
;;(setf queue (make-empty-queue))
  (enqueue-by-priority queue #'num-out-of-place state)
 queue
    
    )

(defun findN(num state)
       (dotimes (index (-(length state) 1))
                (cond ((equal num (nth index state))
                              (return-from findN index)))
       )
)
(defun manhattanHeuristic (puzzle)
       (let ((Total 0)(Xdist NIL) (Ydist NIL) (goalIndex NIL))  
            (dotimes (index (-( length  puzzle) 1)) 
                      (cond ((not (equal (nth index puzzle) 9)) 
                         (setq goalIndex (findN (nth index puzzle) *Goal*))
                        
                         (setq Xdist (abs (- (mod index 3) (mod goalIndex 3))))
                        
                         (cond ((and (>= (abs (- index goalIndex)) 0) (<= (abs (- index goalIndex)) 2))
                                     (setq Ydist 0))
                               ((and (>= (abs (- index goalIndex)) 3) (<= (abs (- index goalIndex)) 5))
                                    (setq Ydist 1))
                               ((and (>= (abs (- index goalIndex)) 6) (<= (abs (- index goalIndex)) 8))
                                     (setq Ydist 2)))
                         (setq Total (+ Xdist Ydist Total))
                     )))
              (return-from manhattanHeuristic  Total)  ))
       
       
(defun manhattan-enqueuer (state queue)
    "Enqueues by manhattan distance"
(setf queue (make-empty-queue))
  (enqueue-by-priority queue #'manhattanHeuristic state)
 queue
)


(defun general-search (initial-state Goal-test enqueueing-function &optional (maximum-iterations nil))
  (let ((q)
	(his (list 0))
       	(iter 0)
        (p)
	)
    (setf p  initial-state)
     (setf q (make-empty-queue))
	(setf q (funcall enqueueing-function (first initial-state) q))
;        (setf his (append  p))

	
    (loop while (or (< iter maximum-iterations) (funcall #'empty-queue? q ))
       do(progn
	   (incf iter)
	   (setf p (cons (remove-front q) nil))
	   (if (funcall Goal-test p)
	       (progn
		 (print iter)
	       
		 (print p))
	       (foreach-valid-move (m p)
				   (setf q (funcall enqueueing-function (make-move m p) q))
	                           (print q)
;		                   (setf his (append his (queue-front q)))
				   ))))))

#|


Assignment 6

There are 5 enqueing functions to be implemented:
BFS-ENQUEUER    (enqueues in breadth-first order)
DFS-ENQUEUER        (enqueues in depth-first order)
MANHATTAN-ENQUEUER  (enqueues by manhattan distance)
 NUM-OUT-ENQUEUER    (enqueues by number of tiles out of place)

DFS:
This uses enqueue at end queue for enqueuing
 (dfs-enqueuer '(#(9 2 3 1 4 6 7 5 8 0)) 'q)

#S(Q
   :KEY #<FUNCTION IDENTITY>
   :LAST ((#(9 2 3 1 4 6 7 5 8 0)))
   :ELEMENTS ((#(9 2 3 1 4 6 7 5 8 0))))

BFS:
This uses enqueue at front queue for enqueuing
(bfs-enqueuer '(#(9 2 3 1 4 6 7 5 8 0)) 'q)
#S(Q :KEY #<FUNCTION IDENTITY> :LAST NIL :ELEMENTS (#(9 2 3 1 4 6 7 5 8 0)))

NUM-OUT-ENQUEUER:
(num-out-of-place '(#(9 2 3 1 4 6 7 5 8 0)))

7
CL-USER> (num-out-enqueuer '(#(9 2 3 1 4 6 7 5 8 0)) q)

#S(Q
   :KEY #<FUNCTION NUM-OUT-OF-PLACE>
   :LAST NIL
   :ELEMENTS #((#(9 2 3 1 4 6 7 5 8 0))))

MANHATTAN ENQUEUR ANDD NUM-OUT OF PLACE USES PRIORITY QUEUE

(manhattanHeuristic '(#(1 8 3 9 6 5 7 4 2 3)))

9

Initial TRIAL  to generate child nodes using num-out of place
(try '(#(9 2 3 1 4 6 7 5 8 0)) 'q #'num-out-enqueuer )

#S(Q
   :KEY #<FUNCTION NUM-OUT-OF-PLACE>
   :LAST NIL
   :ELEMENTS #(#(1 2 3 9 4 6 7 5 8 3) #(2 9 3 1 4 6 7 5 8 1))) 
(#(9 2 3 1 4 6 7 5 8 0) . #(1 2 3 9 4 6 7 5 8 3)) 






After running (general-search s #'goal-p #'¡¯num-out-
enqueuer)

I got following output

print puzzle wasnot working for me generated valid answers using generate.
This is the puzzle which gave answer with 4 iterations. 

#S(Q
   :KEY #<FUNCTION NUM-OUT-OF-PLACE>
   :LAST NIL
   :ELEMENTS #(#(1 2 3 4 5 6 7 8 9 8) #(1 2 3 4 9 5 7 8 6 4)
               #(1 2 9 4 5 3 7 8 6 2) #(1 2 3 4 9 6 7 5 8 4)
               #(1 9 3 4 2 6 7 5 8 1) #(1 2 3 4 5 6 7 9 8 7)
               #(1 2 9 4 5 3 7 8 6 2) #(1 2 3 7 4 6 9 5 8 6)
               #(1 2 3 4 5 6 9 7 8 6) #(1 2 3 4 6 9 7 5 8 5)
               #(1 2 3 4 5 6 7 9 8 7) #(9 2 3 1 4 6 7 5 8 0)
               #(1 2 3 9 4 6 7 5 8 3) #(2 9 3 1 4 6 7 5 8 1)
               #(1 2 3 4 9 5 7 8 6 4))) 

For further puzzles num-out-enqueuer took more time to give valid answers but manhattan enqueuer worked swiftly  
Following puzzles solves in the moves given when used with manhattan rather then num-out
 8 moves
(setq s (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

16 moves
(setq s (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

24 moves
(setq s (make-initial-state '(
1 8 9
3 2 4
6 5 7)))

;;; easy or hard to solve?  Could not get solution maybe because of wrong valid moves.
(setq s (make-initial-state '(
9 2 3
4 5 6
7 8 1)))
|#

				   
		  
	      
	       



	  

						       

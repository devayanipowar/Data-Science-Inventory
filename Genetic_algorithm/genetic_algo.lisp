
(defparameter *float-vector-length* 3 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")
(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))
(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))
(defun randf()
  (if (= (random 2) 0)(-(random 5.12))(random 5.12)))


 
(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))
(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))


;;;creation
(defun float-vector-creator ()
(generate-list *float-vector-length* #'randf))
(defparameter *crossover-probability* 0.3
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.2
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.2
  "Per-gene mutation variance in gaussian convolution")
;;; crossover
(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."
  ;;; DOTIMES, ELT, and ROTATEF

 
 (loop for i from  0 to (- *float-vector-length* 1) do
 
             (if (< *crossover-probability* (random 1.0))
		 (rotatef (elt ind1 i ) (elt ind2 i ))))
   (list ind1 ind2))


(defun valid(x)
  
   (and (>= x *float-min*) (<= x *float-max*)))
  
(defun repeat (x y)
  (if (zerop y)
      nil
      (cons x
	    (repeat x (- 1 y)))))
(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."
  
  (if (< *mutation-probability* (random 1.0))
      (let ((n (gaussian-random 0  *mutation-variance*)))
    (loop for i in ind 

       
	      
	 while ( equal (valid (+ n i)) t)
	   do (setf n (gaussian-random 0  *mutation-variance*))
		 
	     collect (+ n i)))))
	      
;;	 collect (+ n i))))
;;	       (print (+ (gaussian-random 0  *mutation-variance*) i))))))
;;		 until (equal (valid (+ x i)) t)
;;		collect (list (+ x i))))))

 ;;; tournament element selector
(defparameter *tournament-size* 10)
(defun tournament-select-one (population fitness)
  
  (let (best next)
    (setf best (elt population (random 4 )))
  (loop for i from 2 to *tournament-size* do
       
         

       (setf next (elt population (random 4)))
       
	   (if ( > (funcall fitness next) (funcall fitness best))
	       (setf best next)))
  best)
  )
	       
	       
  (defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))    
       
	
	  
(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))


(defun tournament-selector (num population fitness &optional no-duplicates)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"

 (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall #'tournament-select-one population fitness)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))


		
		 
      
       
 
 
  

(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
  (let (l1 l2)
    (setf l1 (copy-list ind1))
    (setf l2 (copy-list ind2))
    (let((crossed ( uniform-crossover l1 l2)))
      (loop for i in crossed 
	 for x = (gaussian-convolution i)
	   collect x))))
	   
	 

;;;printing   
(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit)))
	    pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))
(defun evolve (popsize generations  )
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
 (let ((population 
  (loop for i from 0 to popsize
     for x =  (float-vector-creator)
     for y = (append x)
     collect y)))
   (loop repeat generations do
	
   (setf selected (tournament-selector *float-vector-length* population 'sphere-f))
   
   (setf x (elt selected (random *float-vector-length*)))
   (setf y (elt selected (random *float-vector-length*)))
	 (let(( modif (float-vector-modifier x y)))
(let ((fitness
       (loop for i in modif
	  for x = (sphere-f i)
	    collect x)))
           (simple-printer modif fitness ))))))
	     


	 
     
     
 



#|
Assignment 3 Genetic programming

The genetic Algorithm statrts with creating a random individual of length 20 bwtween 5.12 and -5.12 . I implemented it by using 
(defun randf()
  (if (= (random 2) 0)(-(random 5.12))(random 5.12)))

and then using generate-list function to create vector of length 20
(float-vector-creator)

(2.3728368 1.5269421 -1.7304578 -0.44056335 -0.15773071 3.2890265 -3.6163433
 2.404146 0.6121423 0.77013546 4.6708174 -4.526598 -1.3514062 -0.9876941
 -4.13606 2.4749181 3.9909253 -2.8697808 -1.1387157 3.781087)
CL-USER> 

There are 4 significant functions necessary for genteic algorithm:
Selecting the fit individual out of the population using fitness function using tournament selection method. In this we just have to find the fitness of the first individual and then compare it with next candidate if the next candidate is better replace with next candidate. I initially thought we have to give range of best candidates but we have to just repaeat it till we get just one best solution in the population. Following is the main function to execute it.

 (if ( > (funcall fitness next) (funcall fitness best))
	       (setf best next))
(tournament-selector 20 '((1 2 3)(2 3 4)(2 3 4)(5 5 9)) 'sum-f)

((5 5 9) (5 5 9) (2 3 4) (5 5 9) (5 5 9) (5 5 9) (2 3 4) (5 5 9) (5 5 9)
 (5 5 9) (2 3 4) (5 5 9) (2 3 4) (5 5 9) (5 5 9) (5 5 9) (5 5 9) (5 5 9)
 (5 5 9) (5 5 9))

Modifying the individual chosen by using crossover and mutation is the next important step. After we select fit individuals and create list of given length. We pass this list to crossover and mutation.
In crossover I used elt and rotatef to swap the elements whenever the probability is 
(if (< *crossover-probability* (random 1.0))
		 (rotatef (elt ind1 i ) (elt ind2 i ))))
(list ind1 ind2))
(uniform-crossover '(1 4 5) '(4 5 8))

((4 4 8) (1 5 5))

 (gaussian-convolution '(1 2 3))

0.8657377 
2.1822743 
2.9089978


Combining both crossover and mutation in a function:
modifier:
(let (l1 l2)
    (setf l1 (copy-list ind1))
    (setf l2 (copy-list ind2))
    (let((crossed ( uniform-crossover l1 l2)))

      (loop for i in crossed do
	    (gaussian-convolution i)))))

The final step is to combine all those to work with the evolve function in order to get the sufficient fit answer using print function provided. Evolve in my case I have used by implementing it as
 (evolve popsize generation)
|#

      
   
       
  

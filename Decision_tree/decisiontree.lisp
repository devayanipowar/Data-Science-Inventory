(defparameter *examples*
  '(((status faculty) (floor 5) (department cs) (size large) (:label t))
    ((status faculty) (floor 3) (department ee) (size large) (:label nil))
    ((status faculty) (floor 4) (department cs) (size medium) (:label nil))))
(defparameter *dexamples*
'(((status faculty) (floor 3) (department ee) (size large) (:label nil))
((status staff) (floor 3) (department ee) (size small) (:label nil))
((status faculty) (floor 4) (department cs) (size medium) (:label t))
((status student) (floor 4) (department ee) (size large) (:label t))
((status staff) (floor 5) (department cs) (size medium) (:label nil))
((status faculty) (floor 5) (department cs) (size large) (:label t))
((status student) (floor 3) (department ee) (size small) (:label t))
((status staff) (floor 4) (department cs) (size medium) (:label nil))))
(defparameter *lab*
  '(t nil))
(defparameter *fdefs*
'((status faculty staff student)
(department ee cs)
(floor 3 4 5)
(size small medium large)))
(defun feature-val-for-example (example feature)
"Returns the value of a given feature as stored in an example"
(second (assoc feature example)))
(defun num-examples-with-label (examples label)
"Returns the number of examples with a given label"
(count-if #'(lambda (example) (equalp (label-of-example example) label)) examples))

(defun normalize (numbers)
"Normalizes a list of numbers by dividing them by their sum. If
the numbers are all 0.0, normalize just returns the original numbers."
(let ((sum (apply #'+ numbers)))
(if (= sum 0.0) numbers
(mapcar #'(lambda (num) (/ num sum)) numbers))))

(defun label-probabilities-for-feature-val (examples feature value labels)
(let ((examples-with-feature-val
       (if (null feature) examples(remove-if-not #'(lambda (x) (equalp (feature-val-for-example x feature) value)) examples))))
(normalize(mapcar #'(lambda (label) (/(num-examples-with-label examples-with-feature-val label)
(length labels)))
labels))))
(defun num-examples-with-feature-val (examples feature value)
"Returns the number of examples with a given value for some feature"
(count-if #'(lambda (example) (equalp(feature-val-for-example example feature) value)) examples))

;;; CALCULATE INFORMATION FUNCTION
(defun information (p)
  ;;information function for the probabilities 
  (-(apply #' +(mapcar #'
		(lambda (x)(if (= x 0) 0 (* x (log x 2)))) p))))
(defun sum(list)
  ;;finds sum of list
  (cond ((null list)0)
	((+ (car list)(sum (cdr list))))))

;;; REMAINDER OF THE PROBABILITIES
(defun remainder (fdef examples lab)
  ;;find remainder using sum of feature probability in examples and label probabilities of feature values
  (let (prob nprob)
   (let((main (loop for i in (rest fdef) do
	 (setq prob (label-probabilities-for-feature-val examples (first fdef) i lab))
	 (setq nprob (num-examples-with-feature-val examples (first fdef) i ))
		 collect  (*(/ nprob (length examples))(information prob)))))
     (sum main))))
 ;;;CHOOSE BEST FEATURE
(defun choose-feature (fdefs examples lab)
  ;; returns best feature from the list which has lowest remainder
  (let ( (extract
  (loop for i in fdefs 
		    for x =  (remainder i examples lab)
		    for y = (cons  x  i)
		    collect y)))
   (let((sorted ( sort extract (lambda (x y)(< (first x) (first y))))))
  (rest (first (first (list(subseq sorted 0 1 ))))))))

    

(defun most-common (elts &key (test #'equalp))
"Returns the elt in the list elts that appears the most often;
ties are broken arbitrarily, probably by picking the elt
which appeared earliest. Two elts are considered to be
the same if they pass the :test (by default, equalp)"
(let* ((unique-elts (remove-duplicates elts :test test))
(unique-nums (mapcar #'(lambda (x) (count x elts :test test)) unique-elts))
(best-elt (first unique-elts))
(best-num (first unique-nums)))
(mapc #'(lambda (elt num) (when (> num best-num)
(setf best-elt elt) (setf best-num num)))
(rest unique-elts) (rest unique-nums))
best-elt))
(defun select ( example feature)
 ;;to select the best subset of the data using choose-feature function; returns (feature(val eg)(val eg)) form of data
(let (final newlist )
     (loop for i in example do
	 (let (( x (feature-val-for-example i  feature)))
              (if (setq newlist (assoc x final))
		  (rplacd newlist (cons i (rest newlist)))
		  (setq final (cons (list x i) final)))))
     (cons feature final)))
       



(defun find-label-for-example (example decision-tree)
"Given a decision tree and an example, uses the decision tree
to determine the expected label for the example"
(if (equalp (first decision-tree) :label)
(second decision-tree) ;; return the label
(find-label-for-example example
(second (assoc
(feature-val-for-example example (first decision-tree))
(rest decision-tree))))))

	     
	 

(defun label-of-example (example)
"Returns the label of an example, as stored in the example"
(second (assoc :label example)))
(defun examples-have-same-label-p (examples)
"Returns t if all examples have the same label, else returns nil"
(let ((label (label-of-example (first examples))))
(dolist (example examples t)
(when (not (equalp label (label-of-example example)))
(return nil)))))
(defun most-common-label (examples)
"Returns the most common label found in examples"
(most-common (mapcar #'label-of-example examples)))

(defun examples-with-feature-val (examples feature val)
"Returns the examples whose feature has value val"
(remove-if-not #'(lambda (x) (equalp (feature-val-for-example x feature) val)) examples))
;;;; dECISION TREE
(let ((unique (gensym)))
	(defun build-decision-tree (examples fdefs lab &optional (default unique))
	  (if (equalp default unique)
	      (setf default (most-common-label examples)))
	  ;;EVERY CASE FOR CHECKING NULL VALUES
          (cond ((equalp (examples-have-same-label-p examples) t)
	     (label-of-example (first examples)))
	      
		 ((equalp fdefs nil)
		  (most-common-label examples))
		 ;; BUILD RECURSIVE TREE USING BEST FEATURE
	  (t
	  
	    (let (( f(choose-feature fdefs examples lab)));; BEST FEATURE
	    (let(( best (select examples (first f))))   
		  (cons (first best) (loop for i in (rest best) collect ;;RECURSION TO SELECT CHILD NODES USING BEST FEATURE SUBSET
					  (list (first i) (build-decision-tree (rest i) (rest fdefs)  lab))))))))))

#|
Decision tree assignment:                
The decision tree has 4 conditions to check for . Firstly , if all the examples have same label then return the label and if the example set is empty return most common label as default using the helper function
Then if the feature set is empty return again the most common label by using commonlabel
 function. Finally we build the recursive tree to build the subtrees.
While building tree we do following three steps,
1. select best feature using choose-feature function
2. make a nonleafnode and select branches based on best feature in order to get following child nodes
3. Call tree recursively by using subset of the best feature selected in the example. I used a select function which gave me a tree like this:

When (feature of the value) function is iteratively called for examples for eg with feature ‘status’ I get values:
(EE EE CS EE CS CS EE CS)
I then 

I get this when I select feature value and aggregate the lists with that values in first and rest of the list:

(DEPARTMENT
 (CS ((STATUS STAFF) (FLOOR 4) (DEPARTMENT CS) (SIZE MEDIUM) (:LABEL NIL))
  ((STATUS FACULTY) (FLOOR 5) (DEPARTMENT CS) (SIZE LARGE) (:LABEL T))
  ((STATUS STAFF) (FLOOR 5) (DEPARTMENT CS) (SIZE MEDIUM) (:LABEL NIL))
  ((STATUS FACULTY) (FLOOR 4) (DEPARTMENT CS) (SIZE MEDIUM) (:LABEL T)))
 (EE ((STATUS STUDENT) (FLOOR 3) (DEPARTMENT EE) (SIZE SMALL) (:LABEL T))
  ((STATUS STUDENT) (FLOOR 4) (DEPARTMENT EE) (SIZE LARGE) (:LABEL T))
  ((STATUS STAFF) (FLOOR 3) (DEPARTMENT EE) (SIZE SMALL) (:LABEL NIL))
  ((STATUS FACULTY) (FLOOR 3) (DEPARTMENT EE) (SIZE LARGE) (:LABEL NIL))))

Best feature when given to select function, segregates subsets of the feature to get two branches 
I then iterated recursively through the first subset and then the second to get best features among those subsets by decreasing the values of features by. ;
Here (first ) of this subset is the name of the feature and saved as the tree node. For example as  department is the feature name and then call build tree recursively for CS part and same for EE part.

Information:
it is find by using its formula:
 (-(apply #' +(mapcar #'
		(lambda (x)(if (= x 0) 0 (* x (log x 2)))) p))))

Remainder initial stages:

We sum the product of the information function and probability of the features of the specific value for which we are finding the information too. 
Here nprob = probability of feature value appearing in the whole set of examples
prob = probability of the labels with that feature value ( for each value of label)

Remainder=
sum  of (*(/ nprob (length examples))(information prob))



Choose feature
Then by using remainder values we build choose-feature function . I made array out of the values and corresponding feature and sorted accordingly to the values and then select the feature with the lowest value of remainder
 

CL-USER> (choose-feature *fdefs* *dexamples* *lab*)
((0.34436095 STATUS FACULTY STAFF STUDENT) (1.0 DEPARTMENT EE CS)
 (0.9387219 FLOOR 3 4 5) (0.9387219 SIZE SMALL MEDIUM LARGE))
; compiling (DEFUN CHOOSE-FEATURE ...)
CL-USER> (choose-feature *fdefs* *dexamples* *lab*)

((0.34436095 STATUS FACULTY STAFF STUDENT) (0.9387219 FLOOR 3 4 5)
 (0.9387219 SIZE SMALL MEDIUM LARGE) (1.0 DEPARTMENT EE CS))

CL-USER>  (choose-feature *rfdefs* *rexamples* *rlab*)

(PATRONS NONE SOME FULL)

CL-USER>  (remainder (fourth *rfdefs*) *rexamples* *rlab*)
  (remainder (fourth *rfdefs*) *rexamples* *rlab*)

0.80429035

Trees I got:
for department example

(build-decision-tree *dexamples* *fdefs* *lab*)

(STATUS (STUDENT T) (STAFF NIL) (FACULTY (DEPARTMENT (EE NIL) (CS T))))

for restaurant example:
(build-decision-tree *rexamples* *rfdefs* *rlab*)

(PATRONS (NONE NIL)
 (FULL
  (HUNGRY (NIL NIL)
   (T (TYPE (BURGER T) (ITALIAN NIL) (THAI (OPEN-FRIDAY (NIL NIL) (T T)))))))
 (SOME T))

|#
           


	  


	      






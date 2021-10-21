(defun print-eval (l)
  (mapcar (function (lambda (ele)
                      (print ele )
                      (terpri)
                      (print (eval ele))
                      (terpri)
                      (terpri)
                      ))
          l))


(defun testing ()

  
(print "The next testcases call TravelAgent")
(terpri)
(print-eval '(
(TravelAgent "newark" "bangor" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************" )
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
   (TravelAgent "denver" "westpalm" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
   (TravelAgent "westpalm" "boston" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
   (TravelAgent "memphis" "westpalm" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
   (TravelAgent "westpalm" "ithaca" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
   (TravelAgent "roanoke" "losangeles" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(print-eval '(
	      (TravelAgent "denver" "ithaca" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP)))

(terpri)
(format t "******************************************************")
(terpri)
(format t "******************************************************" )
(terpri)
(format t "TravelAgent testcases completed")

)






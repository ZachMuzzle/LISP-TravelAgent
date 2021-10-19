(defun create-sample-node (start-city)
  (let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 start-city
                                 ))))
  (setf (get node 'state) start-city)
  (setf (get node  'parent) nil)
  (setf (get node 'action) nil)
  (setf (get  node 'best-path-cost) 0)
  (setf (get node 'cost-to-goal-estimate) 534)
  (setf (get  node `least-cost-estimate) 534)
        
  node))

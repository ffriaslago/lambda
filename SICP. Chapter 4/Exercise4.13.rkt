#lang sicp

(define (make-unbound! var env) ; Arguments: the variable is wanted to unbound and the environment
  
  (let ((frame (first-frame env)))
    (let ((vars (frame-variables frame)))
      (let ((vals (frame-values frame)))

        
        (define (scan pre-vars pre-vals vars vals)
          
          (if (not (null? vars))
              
              (if (eq? var (car vars))
                  
                  (begin (set-cdr! pre-vars (cdr vars))
                         
                         (set-cdr! pre-vals (cdr vals)))
                  
                  (scan vars vals (cdr vars) (cdr vals)))))
        
        (if (not (null? vars)) ; If there are vars in our frame
            
            (if (eq? var (car vars)) ; If the variable is found. There is a match!
                
                (begin (set-cdr! frame (cdr vars))  
                       (set-cdr! frame (cdr vals))) ; The frame is updated without the variable and its value
                
                (scan vars vals (cdr vars) (cdr vals)))))))) ; If not, it is continued 
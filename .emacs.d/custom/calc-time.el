; examples
;
; (middle-velocity-delta 16 "21:36:00" "22:50:00")
; (middle-velocity-time 25.62 "2:09:39")
; 25.62 км
; 15.92 миль

(defun middle-velocity-time (distance delta-time)
  (middle-velocity-calc distance (full-time delta-time)))

(defun middle-velocity-delta (distance time-begin time-end)
  (middle-velocity-calc distance (delta-time time-begin time-end)))

(defun delta-time (time-begin time-end)
  (calc-time (time-subtract (parse-time-string time-end) (parse-time-string time-begin))))

(defun full-time (entered-time)
  (calc-time (parse-time-string entered-time)))  

(defun calc-time (list-time)
  "Печатать каждый элемент СПИСКА на отдельной строке."  
  (progn	
	(set 'time-kind 1)
	(set 'summ-time 0)
	(while (< time-kind 4)         ; проверка-истинна-ложь
	  (setq summ-time (+ summ-time (* (car list-time) (kind-time-multiplier time-kind))))
	  (setq list-time (cdr list-time))
	  (setq time-kind (+ 1 time-kind))          ; инкремент, увеличение
	  ))
  summ-time)

(defun kind-time-multiplier (time-kind)
	(if (= time-kind 1) (set 'result 1)
	  (if (= time-kind 2) (set 'result 60)
		(if (= time-kind 3) (set 'result 3600)))))

(defun middle-velocity-calc (distance delta-time)
  (progn
	(setq full-velocity (/ (/ (float delta-time) 60) distance))
	(setq minutes-velocity (ffloor full-velocity))
	(setq seconds-velocity (* (- full-velocity minutes-velocity) 60))

	(setq minutes-velocity-str (number-to-string minutes-velocity))

	(setq seconds-velocity-str (number-to-string seconds-velocity))

	(concat 
	 (substring minutes-velocity-str 0 (- (length minutes-velocity-str) 2)) 
	 ":" 
	 seconds-velocity-str)
	))

(provide 'calc-time)

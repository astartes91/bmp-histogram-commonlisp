(defun create-histogram (&optional (bmp-file "c:\\test.bmp") (gle-file "c:\\histogram.gle") (dat-file "c:\\histogram.dat"))
  (let ((byte-array (create-byte-array bmp-file)))
  	(if (file-bmp-p byte-array)
      (if (bmp-compressed-p byte-array)
        (format t "BMP is compressed - not supported")
        (if (bmp-24-bit-p byte-array)
          (let ((assoc-array (create-assoc-array byte-array)))
            (sort assoc-array #'compare)
            (write-files gle-file dat-file assoc-array))          
          (format t "BMP is not 24 bit - not supported")))
      (format t "File is not BMP format"))))

(defun create-byte-array (bmp-file)
  (let ((byte-array (make-array 0 :fill-pointer 0 :adjustable t)))
  	(with-open-file (in-stream bmp-file
				  	:element-type '(unsigned-byte 8))
	  (when in-stream
	  	(do ((b
		  	(read-byte in-stream nil in-stream)
		  	(read-byte in-stream nil in-stream)))
		  ((eq b in-stream))
    	  (vector-push-extend b byte-array))))
    (return-from create-byte-array byte-array)))

(defun write-files (gle-file dat-file assoc-array)
  (with-open-file (out-stream dat-file
	  :direction :output
	  :if-exists :supersede
	  :if-does-not-exist :create)
      (when out-stream
      	(loop for item in assoc-array
      	  do (format out-stream "~a ~a~%" (first item) (rest item)))))
  (with-open-file (out-stream gle-file
	  :direction :output
	  :if-exists :supersede
	  :if-does-not-exist :create)
      (when out-stream
        (progn
          (format out-stream "size 10 12~%")
          (format out-stream "begin graph~%")
          (format out-stream "size 10 10~%")
          (format out-stream "yaxis min 0~%")
          (format out-stream "xaxis min -50~%")
          (format out-stream "xaxis max 300~%")
          (format out-stream "data ~a~%" dat-file)
          (format out-stream "d1 line bar~%" dat-file)
          (format out-stream "end graph~%" dat-file)))))

;signature of BMP format is 424D
(defun file-bmp-p (byte-array)
  (if (= (aref byte-array 0) 66)
    (if (= (aref byte-array 1) 77)
      t
      nil)
    nil))

;0 - not compressed
(defun bmp-compressed-p (byte-array)
  (if (= (aref byte-array 30) 0)
    nil
    t))

;24 - 24bit image
(defun bmp-24-bit-p (byte-array)
  (if (= (aref byte-array 28) 24)
 	t
    nil))

(defun create-assoc-array(byte-array)
  (let ((start-address (get-bitmap-data-address byte-array))
        (image-width (get-image-width byte-array))
        (assoc-array '()))
    (loop with list = '() with counter = 0 for i from start-address to (length byte-array)
      do 
      	(progn
          (if (= counter (* image-width 3))
            (progn
              (setf i (+ i (1- (mod image-width 4))))
              (setf counter 0))
            (progn
              (push (aref byte-array i) list)
              (when (= (length list) 3)
          	  	(progn 
                  (let ((y-component (get-y-component list)))
                    (if (null (assoc y-component assoc-array))
                      (setf assoc-array (acons y-component 1 assoc-array))
                      (incf (rest (assoc y-component assoc-array)))))
              	  (setf list '())))
          	  (incf counter)))))
    (loop for i from 0 to 255
      do
        (when (null (assoc i assoc-array))
          (setf assoc-array (acons i 0 assoc-array))))
    (return-from create-assoc-array assoc-array)))

(defun get-bitmap-data-address (byte-array)
  (get-bytes-sequence-value byte-array 10 4))

(defun get-image-width (byte-array)
  (get-bytes-sequence-value byte-array 18 4))

;get YUV Y-component (brightness) by formulae 0.299*r + 0.587*g + 0.114*b
(defun get-y-component (rgb)
  (round (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb)))))

(defun get-bytes-sequence-value (byte-array start-address bytes-number)
  (let ((value 0))
    (loop for i from (1- bytes-number) downto 0
  	  do (setf value (+ (* (aref byte-array (+ start-address i)) (expt 2 (* 8 i))) value)))
    (return-from get-bytes-sequence-value value)))

(defun compare (elem1 elem2)
  (< (first elem1) (first elem2)))
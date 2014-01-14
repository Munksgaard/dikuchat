(use-package :sb-bsd-sockets)

(defun chatclient (address port)
  (let ((sock (make-instance 'inet-socket :type :stream
                             :protocol :tcp))
        (bufflen 512))
    (socket-connect sock (make-inet-address address) port)
    (labels ((send (s)
               (socket-send sock s nil))
             (recv ()
               (multiple-value-bind (s len)
                   (socket-receive sock nil bufflen :dontwait t)
                 (cond ((null s) nil) ; Nothing received
                       ((eq len bufflen) ; There might be more
                        (concatenate 'string s (recv)))
                       (t ; We've got it all
                        (string-right-trim '(#\Null) s))))))
      (list #'send #'recv))))

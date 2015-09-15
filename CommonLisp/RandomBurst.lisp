;;--------------------------------------------------------------------------------------------------
;;
;;  File:       RnadomBurst.lisp
;;
;;  Project:    m+m
;;
;;  Contains:   An example script that sends blocks of random numbers.
;;
;;  Written by: Norman Jaffe
;;
;;  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
;;
;;              All rights reserved. Redistribution and use in source and binary forms, with or
;;              without modification, are permitted provided that the following conditions are met:
;;                * Redistributions of source code must retain the above copyright notice, this list
;;                  of conditions and the following disclaimer.
;;                * Redistributions in binary form must reproduce the above copyright notice, this
;;                  list of conditions and the following disclaimer in the documentation and / or
;;                  other materials provided with the distribution.
;;                * Neither the name of the copyright holders nor the names of its contributors may
;;                  be used to endorse or promote products derived from this software without
;;                  specific prior written permission.
;;
;;              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;;              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
;;              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;              DAMAGE.
;;
;;  Created:    2015-08-05
;;
;;--------------------------------------------------------------------------------------------------

(setq burstSize 1)

(setq scriptDescription "A script that generates random blocks of floating-point numbers")

(setq scriptHelp
      "The first argument is the burst period and the second argument is the burst size")

(defun numberOr1 (inValue)
  (if (numberp inValue) inValue 1))

;; Note that the following function processes both arguments, since it will be called at a
;; specific point in the execution sequence.
(defun scriptInterval ()
  (let* ((argCount (array-dimension mmcl:argv 0)) interval)
    (if (< 1 argCount)
	(setq interval (numberOr1 (let* ((*read-default-float-format* 'double-float))
				    (read-from-string (aref mmcl:argv 1))))
	      burstSize (if (< 2 argCount)
			    (numberOr1 (parse-integer (aref mmcl:argv 2) :junk-allowed t))
			  1))
      (setq burstSize 1 interval 1))
    interval))

(setq scriptOutlets (make-array '(1) :initial-element
				(create-outlet-entry "outgoing" "d+"
						     "One or more numeric values")))

(defun scriptThread ()
  (let* (outList)
    (setq outList (make-array (list burstSize)))
    (dotimes (ii burstSize)
      (setf (aref outList ii) (* 10000 (random 1.0))))
    (sendToChannel 0 outList)))

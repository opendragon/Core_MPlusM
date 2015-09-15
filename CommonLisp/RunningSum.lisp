;;--------------------------------------------------------------------------------------------------
;;
;;  File:       RunningSum.lisp
;;
;;  Project:    m+m
;;
;;  Contains:   An example script that performs a running sum.
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

(setq summing nil)
(setq runningSum 0)

(defun do-a-command (aCommand)
  (cond ((numberp aCommand)
	 (incf runningSum (if summing aCommand 0)))
	((stringp aCommand)
	 (setq bCommand (string-downcase aCommand))
	 (cond ((string= "addtosum" bCommand))
	       ((string= "quit" bCommand) (requestStop))
	       ((string= "resetsum" bCommand) (setq runningSum 0))
	       ((string= "startsum" bCommand) (setq summing t))
	       ((string= "stopsum" bCommand) (setq summing nil))
	       (t (format t "unrecognized~%"))))
	(t (format t "no match for ~A~%" aCommand))))

(defun do-input-sequence (incomingData)
  (cond
   ((stringp incomingData)
    (do-a-command incomingData))
   ((arrayp incomingData)
    (dotimes (ii (array-dimension incomingData 0))
      (do-input-sequence (aref incomingData ii))))
   (t (do-a-command incomingData))))

(defun doRunningSum (portNumber incomingData)
  (do-input-sequence incomingData)
  (sendToChannel 0 runningSum))

(setq scriptDescription "A script that calculates running sums")

(setq scriptInlets (make-array '(1) :initial-element
			       (create-inlet-entry "incoming" "sd*" "A command and data"
						   'doRunningSum)))

(setq scriptOutlets (make-array '(1) :initial-element
				(create-outlet-entry "outgoing" "d" "The running sum")))

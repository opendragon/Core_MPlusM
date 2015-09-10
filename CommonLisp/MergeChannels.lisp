;;--------------------------------------------------------------------------------------------------
;;
;;  File:       MergeChannels.lisp
;;
;;  Project:    m+m
;;
;;  Contains:   An example script that merges all its inlet channels to its outlet.
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

(defun scriptHandleInput (portNumber incomingData)
  (format t "input on port ~S~%" portNumber)
  (setq incomingData (concatenate 'array (list portNumber) incomingData))
  (sendToChannel 0 incomingData))

(setq scriptDescription "A script that merges multiple channels")

(setq scriptHelp "The first argument to the script is the number of inlets to create")

;; The following function will either generate one inlet, called 'incoming' or a set of inlets,
;; called 'incoming#',
(defun scriptInlets ()
  (let* (inlets inletCount anInlet)
    (setq inletCount (cond ((< 1 (array-dimension mmcl:argv 0))
			    (parse-integer (aref mmcl:argv 1) :junk-allowed t))
			   (t 1)))
    (setq inletCount (cond ((numberp inletCount) inletCount)
			   (t 1)))
    (setq inlets (make-array (list inletCount)))
    (cond ((< 1 inletCount)
	   (dotimes (ii inletCount)
	     (let* (scriptInlet)
	       (setq scriptInlet (make-hash-table))
	       (psetf (gethash 'name scriptInlet) (format nil "incoming~D" ii)
		      (gethash 'protocol scriptInlet) "*"
		      (gethash 'protocolDescription scriptInlet) "Anything"
		      (gethash 'handler scriptInlet) 'scriptHandleInput)
	       (setf (aref inlets ii) scriptInlet))))
	  (t (let* (scriptInlet)
	       (setq scriptInlet (make-hash-table))
	       (psetf (gethash 'name scriptInlet) "incoming"
		      (gethash 'protocol scriptInlet) "*"
		      (gethash 'protocolDescription scriptInlet) "Anything"
		      (gethash 'handler scriptInlet) 'scriptHandleInput)
	       (setf (aref inlets 0) scriptInlet))))
    inlets))

(let* (scriptOutlet1)
  (setq scriptOutlet1 (make-hash-table))
  (psetf (gethash 'name scriptOutlet1) "outgoing"
	 (gethash 'protocol scriptOutlet1) "*"
	 (gethash 'protocolDescription scriptOutlet1) "An arbitrary sequence of values")
  (setq scriptOutlets (make-array '(1) :initial-element scriptOutlet1)))

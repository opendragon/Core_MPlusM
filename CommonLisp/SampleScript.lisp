;;--------------------------------------------------------------------------------------------------
;;
;;  File:       SampleScript.lisp
;;
;;  Project:    m+m
;;
;;  Contains:   An example script that meets the requirements for a Common Lisp file to be used with
;;              the CommonLisp filter service.
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

;; Classes available to Common Lisp code:
;;
;; Functions available to Common Lisp code:
;;
;;   (requestStop) - signal that the service should be stopped at the next opportunity
;;
;;   (sendToChannel n x) - converts the value 'x' to YARP format and sends it to the channel
;;                         numbered 'n', with zero being the first outlet channel
;;
;; Global variables available to Common Lisp code:
;;
;;   argv: a list of the arguments passed to the script
;;
;;   tag:  the (optional) tag argument for the CommonLisp filter service
;;
;; Values that must be provided by the Common Lisp code:
;;
;;   scriptDescription: a variable or a function that provides a string describing the script
;;
;; Values that may be provided by the Common Lisp code:
;;
;;   scriptHelp:      a variable or a function that provides a string that can be presented to the
;;                    user when requested by the '?' command; note that it should not end with a
;;                    newline
;;
;;   scriptInlets:    a variable or a function that provides an array of inlet descriptions [name,
;;                    protocol, protocolDescription, handler]; note that this is ignored if
;;                    scriptThread is defined
;;
;;   scriptInterval:  a variable or a function that provides the interval between executions of the
;;                    scriptThread function; note that this is ignored if scriptThread is not
;;                    defined, and it is executed only once, after all the other values have been
;;                    processed
;;
;;   scriptOutlets:   a variable or a function that provides an array of outlet descriptions [name,
;;                    protocol, protocolDescription]
;;
;;   scriptStarting: a function that is called before any inlets are attached or threads started
;;
;;   scriptStopping: a function that is called after all the inlets are detached and threads are
;;                   stopped
;;
;;   scriptThread:   a function that is repeatedly called by the output thread of the service
;;
;; Order of reference / execution:
;;
;;   1)  Script is loaded; all global statements are executed, in order, and the global functions
;;       are defined - functions and variables provided by the service are available, except for
;;       'sendToChannel'
;;   2)  'scriptDescription' is evaluated, if available
;;   3)  'scriptHelp' is evaluated, if available
;;   4)  'scriptThread' is retrieved, if available; this does not involve executing the function
;;   5)  'scriptInlets' is evaluated, if available
;;   6)  'scriptOutlets' is evaluated, if available
;;   7)  'scriptStarting' is retrieved, if available; this does not involve executing the function
;;   8)  'scriptStopping' is retrieved, if available; this does not involve executing the function
;;   9)  'scriptInterval' is evaluated, if available and 'scriptThread' was retrieved
;;  10)  ... configure the service ...; execute the 'scriptStarting' function, if available
;;  11)  ... start service ...; inlets and outlets are created and attached
;;  12)  'sendToChannel' can now be safely called
;;  12a) If 'scriptThread' was retrieved, start a separate thread that executes the 'scriptThread'
;;       function every 'scriptInterval' seconds
;;  12b) If 'scriptThread' was not retrieved, associate the handler function of the 'scriptInlets'
;;       value with its corresponding inlet, for all the inlets
;;  13)  ... service running ...
;;  14)  ... service stopping ...
;;  14a) If 'scriptThread' was retrieved, the thread launched earlier is stopped
;;  14b) If 'scriptThread' was not retrieved, the inlet stream handlers are deactivated
;;  15)  Execute the 'scriptStopping' function, if available; 'sendToChannel' can be called
;;  16)  ... inlet and outlet streams are destroyed, service is stopped ...

;; Some test Common Lisp...

(use-package 'mmcl)

(setq now (multiple-value-list (decode-universal-time (get-universal-time))))
(let ((hh (nth 2 now))
      (mi (nth 1 now))
      (ss (nth 0 now))
      (yy (nth 5 now))
      (mo (nth 4 now))
      (dd (nth 3 now)))
  (setq nowString (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" yy mo dd hh mi ss)))

(format t "Hello world, it is ~A~%" nowString)

(setq ww 4.34 xx 5)

(defun hereWeGo ()
  (format t "inside hereWeGo~%"))

(setq yy nil)

(setq zz (make-hash-table))
(psetf (gethash 'aa zz) "first"
       (gethash 'bb zz) "second"
       (gethash 'cc zz) 42)

(defun andAnotherFunction (aa)
  (format t "argument is ~S~%" aa)
  (hereWeGo))

(andAnotherFunction 42)

(format t "argv:~%~A~%tag is \"~A\"~%" mmcl:argv mmcl:scriptTag)

;; The real stuff:

(defun numberOr1 (inValue)
  (if (numberp inValue) inValue 1))

(defun handleOurInput (portNumber incomingData)
  (format t "input on port ~S~%" portNumber)
  (sendToChannel 0 incomingData))

;; Specific named values required by the C++ code, such as 'scriptDescription' and 'scriptInlets',
;; can be provided by either functions or 'global' variables.

;(setq scriptDescription "An example script")
(defun scriptDescription ()
  "An example script")

(setq scriptHelp "The first argument to the script is the number of inlets to create")

;; The following function will either generate one inlet, called 'incoming' or a set of inlets,
;; called 'incoming#',
(defun scriptInlets ()
  (let ((inletCount (numberOr1 (if (< 1 (array-dimension mmcl:argv 0))
				    (parse-integer (aref mmcl:argv 1) :junk-allowed t)
				 1))))
    (let ((inlets (make-array (list inletCount))))
      (if (< 1 inletCount)
	  (dotimes (ii inletCount)
	    (setf (aref inlets ii)
		  (create-inlet-entry (format nil "incoming~D" ii) "*" "Anything" 'handleOurInput)))
	(setf (aref inlets 0) (create-inlet-entry "incoming" "*" "Anything" 'handleOurInput)))
      inlets)))

(setq scriptOutlets (make-array '(1) :initial-element
				(create-outlet-entry "outgoing" "*" "Anything")))

;; The 'scriptStarting' and 'scriptStopping' functions are optional; if 'scriptStarting' returns
;; the boolean value true, it's OK to proceed. If, instead, it returns something else, the script
;; has indicated that it shouldn't be run, and the return value gets displayed when the service is
;; started - not when it's created or when the script is loaded. Note that 'scriptStarting' is
;; executed before the handlers for the inlets are attached and 'scriptStopping' is executed after
;; all the inlets are detached.
(defun scriptStarting ()
  (format t "script starting~%")
  t)

(defun scriptStopping ()
  (format t "script stopping~%"))

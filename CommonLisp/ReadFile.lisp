;;--------------------------------------------------------------------------------------------------
;;
;;  File:       ReadFile.lisp
;;
;;  Project:    m+m
;;
;;  Contains:   An example script that reads a series of values from a file.
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

(setq in-stream nil)

(setq scriptDescription "A script that reads values from a file")

(setq scriptHelp "The first argument is the path to the input file")

(defun scriptStarting ()
  (format t "script starting~%")
  (if (< 1 (array-dimension mmcl:argv 0))
      (let ((file-path (aref mmcl:argv 1)))
	(setq in-stream (open file-path :direction :input
			       :if-does-not-exist :error))
	(if (open-stream-p in-stream)
	    (progn
	      (format t "in-stream open~%")
	      (do ((in-line (read-line in-stream nil 'eof)
			    (read-line in-stream nil 'eof)))
		  ((eq in-line 'eof))
		  (format t "in-stream read = \"~A\"~%" in-line)))
	  (setq in-stream nil))
	t)
    nil))

(defun scriptStopping ()
  (format t "script stopping ~%")
  (if in-stream (close in-stream)))

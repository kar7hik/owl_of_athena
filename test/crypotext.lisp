;;; crypotext.lisp --- 
;; 
;; Filename: crypotext.lisp
;; Description: 
;; Author: Karthik Kumar
;; Maintainer: 
;; Created: Sat Sep  8 18:52:20 2018 (+0530)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Sat Sep  8 19:11:43 2018 (+0530)
;;           By: Karthik Kumar
;;     Update #: 16
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defparameter *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
	"enlpo pib slafml pvv bfwkj"))

(defparameter *encipher-table* (make-hash-table))
(defparameter *decipher-table* (make-hash-table))


(defun make-subtitution (code clear)
  (setf (gethash clear *encipher-table*) code)
  (setf (gethash code *decipher-table*) clear))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crypotext.lisp ends here

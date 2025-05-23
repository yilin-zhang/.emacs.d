;;; spamemo.el --- Emacs implementation of FSRS algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yilin Zhang
;; Keywords: spaced-repetition, flashcards, learning
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (json "1.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A simple spaced repetition system using the FSRS-5 algorithm.
;;
;; This implementation follows the algorithm described in:
;; https://borretti.me/article/implementing-fsrs-in-100-lines
;;
;; Key features:
;; - Review words with 4 response options (Forgot, Hard, Good, Easy)
;; - Customizable target retrievability to control review frequency
;; - JSON storage for vocabulary and metadata

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defgroup spamemo nil
  "A spaced repetition system using the FSRS algorithm."
  :group 'applications)

(defcustom spamemo-vocab-file (expand-file-name "spamemo/vocab.json" user-emacs-directory)
  "Path to the vocabulary file."
  :type 'file
  :group 'spamemo)

(defcustom spamemo-target-retrievability 0.9
  "Target retrievability (between 0.0 and 1.0) for scheduling reviews.
Higher values (closer to 1.0) mean more frequent reviews.
Lower values allow longer intervals but higher risk of forgetting."
  :type 'float
  :group 'spamemo)

(defcustom spamemo-min-interval 0.0
  "Minimum interval between reviews in days.
This sets a floor value for the calculated interval between reviews.
A value of 0.0 allows multiple reviews on the same day."
  :type 'float
  :group 'spamemo)

(defcustom spamemo-enforce-success t
  "If non-nil, enforce repeated review of failed cards.
When enabled, cards rated as 1 (Forgot) will not have their interval changed,
ensuring they appear again in the next review session.
This forces users to repeat difficult cards until they are learned."
  :type 'boolean
  :group 'spamemo)

(defface spamemo-word-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.5))
  "Face for displaying the current word."
  :group 'spamemo)

(defvar spamemo-deck nil
  "The current deck of words with their metadata.")

(defvar spamemo-due-words nil
  "List of words that are due for review.")

(defvar spamemo-current-word nil
  "The current word being reviewed.")

(defvar spamemo-review-buffer-name "*SpaMemo Review*"
  "Name of the buffer used for reviews.")

(defconst spamemo-weights
  [
   0.40255  ; grade 1 (forgot)
   1.18385  ; grade 2 (good)
   3.173    ; grade 3 (hard)
   15.69105 ; grade 4 (easy)
   7.1949
   0.5345
   1.4604
   0.0046
   1.54575
   0.1192
   1.01925
   1.9395
   0.11
   0.29605
   2.2698
   0.2315
   2.9898
   0.51655
   0.6621
   ]
  "The weights of the FSRS algorithm.")

(defconst spamemo-F 0.2345679 "The F weight of the FSRS algorithm.")  ; 19 / 81

(defconst spamemo-C -0.5 "The C weight of the FSRS algorithm.")

;; FSRS Algorithm implementation

(cl-defstruct (spamemo-word-meta)
  (added-date (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
  (last-review (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
  (stability 0)
  (difficulty 0.0)
  (interval 0)
  (repetitions 0))

(defun spamemo--parse-iso-date (date-string)
  "Parse DATE-STRING as ISO 8601 date to internal time format."
  (encode-time (parse-time-string date-string)))

(defun spamemo--date-to-string (time)
  "Convert TIME to ISO 8601 format string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))

(defun spamemo--round (number decimals)
  "Round NUMBER to DECIMALS decimal places."
  (let ((factor (expt 10 decimals)))
    (/ (round (* number factor)) (float factor))))

(defun spamemo--clamp (value min-val max-val)
  "Clamp VALUE to be between MIN-VAL and MAX-VAL."
  (min max-val (max min-val value)))

(defun spamemo--shuffle-list (list)
  "Shuffle LIST randomly."
  (seq-sort (lambda (_a _b) (< (random 10) 5)) list))

(defun spamemo--days-since (date-string)
  "Calculate days passed since DATE-STRING (in ISO 8601 format) until now.
Returns a float if less than 1 day has passed."
  (let* ((date-time (spamemo--parse-iso-date date-string))
         (current-time (current-time))
         (time-diff (float-time (time-subtract current-time date-time)))
         (days-diff (/ time-diff 86400))) ; 86400 seconds in a day
    (if (< days-diff 1)
        days-diff  ; Return the float directly if less than 1 day
      (floor days-diff))))

(defun spamemo-word-meta-days-since (meta)
  "Calculate days since last review for item with metadata META."
  (spamemo--days-since (spamemo-word-meta-last-review meta)))

(defun spamemo-word-meta-retrievability (meta)
  "Calculate the retrievability of a card with metadata META.
This is the probability of recalling the card after t days without review,
based on the formula R(t) = (1 + F*t/S)^C where:
- t is days since last review
- S is stability
- F and C are algorithm constants"
  (let ((t_ (spamemo-word-meta-days-since meta))
        (F spamemo-F)
        (S (spamemo-word-meta-stability meta))
        (C spamemo-C))
    (expt (+ 1 (* F (/ t_ S))) C)))

(defun spamemo--get-new-stability-first-time (grade)
  "Calculate initial stability for a new item based on response GRADE (1-4)."
  (aref spamemo-weights (- grade 1)))

(defun spamemo--get-new-stability-short-term (meta grade)
  "Calculate new stability for short-term (same-day) review with metadata META.
GRADE is the rating given (1=forgot, 2=hard, 3=good, 4=easy).
Used when cards are reviewed again on the same day (interval < 1)."
  (let ((G grade)
        (S (spamemo-word-meta-stability meta))
        (w_17 (aref spamemo-weights 17))
        (w_18 (aref spamemo-weights 18)))
    (* S (exp (* w_17 (+ (- G 3) w_18))))))

(defun spamemo--get-new-stability-success (meta grade)
  "Calculate new stability after successful recall (GRADE > 1) for META.
GRADE is the rating given (2=hard, 3=good, 4=easy)."
  (let* ((D (spamemo-word-meta-difficulty meta))
         (S (spamemo-word-meta-stability meta))
         (R (spamemo-word-meta-retrievability meta))
         (w_8 (aref spamemo-weights 8))
         (w_9 (aref spamemo-weights 9))
         (w_10 (aref spamemo-weights 10))
         (w_15 (aref spamemo-weights 15))
         (w_16 (aref spamemo-weights 16))
         (t_d (- 11 D))
         (t_s (expt S (* -1 w_9)))
         (t_r (- (exp (* w_10 (- 1 R))) 1))
         (h (if (= grade 2) w_15 1))
         (b (if (= grade 4) w_16 1))
         (alpha (+ 1 (* t_d t_s t_r h b (exp w_8)))))
    (* S alpha)))

(defun spamemo--get-new-stability-failure (meta grade)
  "Calculate new stability after failed recall (GRADE = 1) for META.
GRADE is the rating given (always 1 for failure)."
  (let* ((D (spamemo-word-meta-difficulty meta))
         (S (spamemo-word-meta-stability meta))
         (R (spamemo-word-meta-retrievability meta))
         (w_11 (aref spamemo-weights 11))
         (w_12 (aref spamemo-weights 12))
         (w_13 (aref spamemo-weights 13))
         (w_14 (aref spamemo-weights 14))
         (d_f (expt D (* -1 w_12)))
         (s_f (- (expt (+ S 1) w_13) 1))
         (r_f (exp (* w_14 (- 1 R))))
         (S_f (* d_f s_f r_f w_11)))
    (min S_f S)))

(defun spamemo--get-new-difficulty-first-time (grade)
  "Calculate initial difficulty for a new item based on response GRADE (1-4)."
  (let* ((G grade)
         (w_4 (aref spamemo-weights 4))
         (w_5 (aref spamemo-weights 5))
         (D0 (+ (- w_4 (exp (* w_5 (- G 1)))) 1)))
    (spamemo--clamp D0 1.0 10.0)))

(defun spamemo--get-new-difficulty-nth-time (meta grade)
  "Update difficulty for repeat review with metadata META and GRADE rating (1-4)."
  (let* ((G grade)
         (D (spamemo-word-meta-difficulty meta))
         (w_6 (aref spamemo-weights 6))
         (w_7 (aref spamemo-weights 7))
         (delta_D (* (* -1 w_6) (- G 3)))
         (D_p (+ D (* delta_D (/ (- 10 D) 9))))
         (D0_4 (spamemo--get-new-difficulty-first-time 4))
         (D_pp (+ (* w_7 D0_4) (* (- 1 w_7) D_p))))
    (spamemo--clamp D_pp 1.0 10.0)))

(defun spamemo--get-new-interval (meta)
  "Calculate the new interval for a card with metadata META.
Uses the formula I = (S/F) * (R^(1/C) - 1) where:
- S is the stability
- R is the target retrievability (customizable)
- F and C are algorithm constants"
  (let* ((S (spamemo-word-meta-stability meta))
         (R spamemo-target-retrievability)
         (F spamemo-F)
         (C spamemo-C)
         (interval (/ (* S (- (expt R (/ 1 C)) 1)) F)))
    (max spamemo-min-interval interval)))

(defun spamemo--update-last-review (meta)
  "Update the last review timestamp of META to current time."
  (setf (spamemo-word-meta-last-review meta)
        (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun spamemo--update-interval (meta)
  "Update the interval of META based on its current stability and retrievability."
  (setf (spamemo-word-meta-interval meta)
        (spamemo--get-new-interval meta)))

(defun spamemo--reset-interval (meta)
  "Set the interval to be 0.0."
  (setf (spamemo-word-meta-interval meta) 0.0))

(defun spamemo--update-repetitions (meta)
  "Increment the repetition count for META by 1."
  (let ((rep (spamemo-word-meta-repetitions meta)))
    (setf (spamemo-word-meta-repetitions meta)
          (+ rep 1))))

(defun spamemo--update-word-first-time (meta grade)
  "Update META for first review with given GRADE rating (1-4)."
  (setf (spamemo-word-meta-stability meta)
        (spamemo--get-new-stability-first-time grade))
  (setf (spamemo-word-meta-difficulty meta)
        (spamemo--get-new-difficulty-first-time grade)))

(defun spamemo--update-word-short-term (meta grade)
  "Update META for short-term (same-day) review with given GRADE (1-4).
Used when a card is reviewed on the same day as a previous review (interval < 1).
Updates both stability and difficulty using short-term adjustment algorithms."
  (setf (spamemo-word-meta-stability meta)
        (spamemo--get-new-stability-short-term meta grade))
  (setf (spamemo-word-meta-difficulty meta)
        (spamemo--get-new-difficulty-nth-time meta grade)))

(defun spamemo--update-word-success (meta grade)
  "Update META after successful recall (GRADE > 1)."
  (setf (spamemo-word-meta-stability meta)
        (spamemo--get-new-stability-success meta grade))
  (setf (spamemo-word-meta-difficulty meta)
        (spamemo--get-new-difficulty-nth-time meta grade)))

(defun spamemo--update-word-failure (meta grade)
  "Update META after failed recall (GRADE = 1)."
  (setf (spamemo-word-meta-stability meta)
        (spamemo--get-new-stability-failure meta grade))
  (setf (spamemo-word-meta-difficulty meta)
        (spamemo--get-new-difficulty-nth-time meta grade)))

(defun spamemo--update-word (meta grade)
  "Update the word metadata META based on GRADE rating (1-4).
If `spamemo-enforce-success' is non-nil and grade is 1 (forgot),
the interval will not be changed, forcing immediate review.

This function handles three distinct cases:
1. First time review (repetitions = 0)
2. Short-term (same-day) review (interval < 1 day)
3. Regular review, which is further split into success (grade > 1) or failure"
  ;; Update stability and difficulty based on performance
  (if (= (spamemo-word-meta-repetitions meta) 0) ; first time
      (spamemo--update-word-first-time meta grade)
    (if (< (spamemo-word-meta-interval meta) 1) ; same-day (short-term)
        (spamemo--update-word-short-term meta grade)
      (if (< 1 grade)
          (spamemo--update-word-success meta grade) ; success
        (spamemo--update-word-failure meta grade)  ; forgot
        )))

  ;; Update interval only if grade > 1 or if enforce-success is off
  (spamemo--update-last-review meta)
  (if (and spamemo-enforce-success (= grade 1))
      ;; instead of calculating a new interval by the algorithm
      ;; reset it to zero to enforce an immediate review
      (spamemo--reset-interval meta)
    (spamemo--update-interval meta))
  ;; Always increment repetition count
  (spamemo--update-repetitions meta)
  meta)

(defun spamemo--is-due (meta)
  "Check if a word with metadata META is due for review."
  (let* ((last-review-time (spamemo--parse-iso-date
                            (spamemo-word-meta-last-review meta)))
         (interval-days (spamemo-word-meta-interval meta))
         (due-time (time-add last-review-time
                             (seconds-to-time (* interval-days 86400))))
         (current-time (current-time)))
    (time-less-p due-time current-time)))

;; File operations

(defun spamemo--ensure-vocab-dir ()
  "Ensure the vocabulary directory exists."
  (let ((dir (file-name-directory spamemo-vocab-file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun spamemo--load-deck ()
  "Load the vocabulary deck from file."
  (spamemo--ensure-vocab-dir)
  (if (file-exists-p spamemo-vocab-file)
      (condition-case nil
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (json-data (json-read-file spamemo-vocab-file))
                 (words (gethash "words" json-data)))
            ;; Convert the JSON data to our internal format
            (let ((deck (make-hash-table :test 'equal)))
              (maphash (lambda (word meta-json)
                         (let ((meta (make-spamemo-word-meta
                                      :added-date (gethash "added_date" meta-json)
                                      :last-review (gethash "last_review" meta-json)
                                      :stability (gethash "stability" meta-json)
                                      :difficulty (gethash "difficulty" meta-json)
                                      :interval (gethash "interval" meta-json)
                                      :repetitions (gethash "repetitions" meta-json))))
                           (puthash word meta deck)))
                       words)
              deck))
        (error (make-hash-table :test 'equal)))
    (make-hash-table :test 'equal)))

(defun spamemo--save-deck (deck)
  "Save DECK to the vocabulary file."
  (spamemo--ensure-vocab-dir)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (words-json (make-hash-table :test 'equal))
         (json-data (make-hash-table :test 'equal)))

    ;; Convert the internal format to JSON
    (maphash (lambda (word meta)
               (let ((meta-json (make-hash-table :test 'equal)))
                 (puthash "added_date" (spamemo-word-meta-added-date meta) meta-json)
                 (puthash "last_review" (spamemo-word-meta-last-review meta) meta-json)
                 (puthash "stability" (spamemo-word-meta-stability meta) meta-json)
                 (puthash "difficulty" (spamemo-word-meta-difficulty meta) meta-json)
                 (puthash "interval" (spamemo-word-meta-interval meta) meta-json)
                 (puthash "repetitions" (spamemo-word-meta-repetitions meta) meta-json)
                 (puthash word meta-json words-json)))
             deck)

    (puthash "words" words-json json-data)

    (with-temp-file spamemo-vocab-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode json-data))))))

;; Core functionality

(defun spamemo-get-due-words ()
  "Get list of words that are due for review."
  (unless spamemo-deck
    (setq spamemo-deck (spamemo--load-deck)))

  (let (due-words)
    (maphash (lambda (word meta)
               (when (spamemo--is-due meta)
                 (push word due-words)))
             spamemo-deck)
    (spamemo--shuffle-list due-words)))

(defun spamemo-update-word (word grade)
  "Update WORD with review GRADE (1-4)."
  (when (and spamemo-deck (gethash word spamemo-deck))
    (let ((meta (gethash word spamemo-deck)))
      (puthash word (spamemo--update-word meta grade) spamemo-deck)
      (spamemo--save-deck spamemo-deck))))

;; UI functions

(defun spamemo--setup-review-buffer ()
  "Set up the review buffer."
  (let ((buffer (get-buffer-create spamemo-review-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (spamemo-review-mode)))
    buffer))

(defun spamemo--center-text (text)
  "Center TEXT in the current window width, accounting for different text faces."
  (let* ((wpw (window-pixel-width))
         (text-pixel-width (string-pixel-width text))
         (padding-pixels (max 0 (/ (- wpw text-pixel-width) 2)))
         (padding-string (propertize " " 'display `(space :width (,padding-pixels)))))
    (concat padding-string text)))

(defun spamemo--insert-instructions ()
  "Insert the instructions in the review buffer with centered text."
  (let ((inhibit-read-only t)
        (separator (make-string 30 ?═)))
    (erase-buffer)
    (let* ((lines '("How well do you know this word?"
                    "1 - Forgot"
                    "2 - Hard"
                    "3 - Good"
                    "4 - Easy"
                    "Press 'l' to look up this word in browser"
                    "Press 'n' to skip the current word"
                    ""
                    ""
                    ""))
           (text-n-lines (+ (length lines) 1)) ; another line for word
           (top-n-lines (max 0 (- (floor (window-height) 2) (+ text-n-lines 5)))) ; 5 is an arbitrary number to bring the text up a little
           )
      (insert (make-string top-n-lines ?\n))
      (dolist (line lines)
        (insert (spamemo--center-text line))
        (insert "\n")))))

(defun spamemo--display-current-word ()
  "Display WORD in the review buffer."
  (with-current-buffer (get-buffer spamemo-review-buffer-name)
    (setq header-line-format (spamemo--center-text "SpaMemo Review"))
    (let ((inhibit-read-only t))
      ;; Clean up buffer and re-insert instructions
      (spamemo--insert-instructions)
      ;; Add the word label (centered)
      (goto-char (point-max))
      ;; Create a propertized version of the word first
      (let ((propertized-word (propertize spamemo-current-word 'face 'spamemo-word-face)))
        ;; Then center and insert this propertized word
        (insert (spamemo--center-text propertized-word)))
      ))
  )

(defun spamemo--lookup-word (word)
  "Look up WORD in an online dictionary."
  (browse-url (format "https://www.merriam-webster.com/dictionary/%s" word)))

(defun spamemo-review-next-word ()
  "Review the next due word.
When current list is empty, re-check for due words and continue if any exist."
  (interactive)
  (if (or spamemo-due-words (setq spamemo-due-words (spamemo-get-due-words)))
      (let ((word (pop spamemo-due-words)))
        (setq spamemo-current-word word)
        (spamemo--display-current-word))
    (progn
      (setq spamemo-current-word nil)
      (message "Review finished!")
      (quit-window))))

(defun spamemo-lookup-current-word ()
  (interactive)
  (if (null spamemo-current-word)
      (message "No current word to look up")
    (progn
      (message "lookup word %s" spamemo-current-word)
      (spamemo--lookup-word spamemo-current-word))))

(defun spamemo-quit ()
  "Quit the SpaMemo window."
  (interactive)
  (message "Exiting review")
  (quit-window))

(defun spamemo--handle-grade (grade)
  "Handle user grading of a card with GRADE (1-4)."
  (if (null spamemo-current-word)
      (message "No current word to rate")
    (progn
      (let ((rating-text (cond
                          ((= grade 1) "Forgot")
                          ((= grade 2) "Hard")
                          ((= grade 3) "Good")
                          ((= grade 4) "Easy"))))
        (message "Rated '%s' as (%s)" spamemo-current-word rating-text)
        (spamemo-update-word spamemo-current-word grade))))
  (spamemo-review-next-word))

(defun spamemo-rate-forgot ()
  "Rate the current word as 'forgot' (score=1)"
  (interactive)
  (spamemo--handle-grade 1))

(defun spamemo-rate-hard ()
  "Rate the current word as 'hard' (score=2)"
  (interactive)
  (spamemo--handle-grade 2))

(defun spamemo-rate-good ()
  "Rate the current word as 'good' (score=3)"
  (interactive)
  (spamemo--handle-grade 3))

(defun spamemo-rate-easy ()
  "Rate the current word as 'easy' (score=4)"
  (interactive)
  (spamemo--handle-grade 4))

;;;###autoload
(defun spamemo-review ()
  "Start a review session."
  (interactive)
  (setq spamemo-deck (spamemo--load-deck))
  (setq spamemo-due-words (spamemo-get-due-words))
  (if (null spamemo-due-words)
      (message "No words to review")
    (let ((buffer (spamemo--setup-review-buffer)))
      (switch-to-buffer buffer)
      (spamemo-review-next-word))))

;;;###autoload
(defun spamemo-review-refresh ()
  "Refresh the review window."
  (interactive)
  (when spamemo-current-word
    (let ((buffer (spamemo--setup-review-buffer)))
      (switch-to-buffer buffer)
      (spamemo--display-current-word))))

;;;###autoload
(defun spamemo-add-word (word)
  "Add WORD to the vocabulary deck.
After adding a word, prompts if you want to add another."
  (interactive "sWord to add: ")
  (when (string-empty-p word)
    (error "Word cannot be empty"))

  (unless spamemo-deck
    (setq spamemo-deck (spamemo--load-deck)))

  (let ((status-msg
         (if (gethash word spamemo-deck)
             (format "Word '%s' already exists in the deck. " word)
           (let ((meta (make-spamemo-word-meta)))
             (puthash word meta spamemo-deck)
             (spamemo--save-deck spamemo-deck)
             (format "Added '%s' to the deck. " word)))))

    (let ((choice (read-char-choice
                   (concat status-msg "Add another word? (y/n or ENTER for yes): ")
                   '(?y ?n ?\r))))
      (when (or (eq choice ?y) (eq choice ?\r))
        (call-interactively #'spamemo-add-word)))))

;;;###autoload
(defun spamemo-reload-deck ()
  "Reload the vocab file to spamemo-deck.
Use this command after you changed the vocab file."
  (interactive)
  (setq spamemo-deck (spamemo--load-deck))
  (message "Reloaded SpaMemo deck"))

;;;###autoload
(defun spamemo-open-vocab-file ()
  "Open the vocab file."
  (interactive)
  (find-file spamemo-vocab-file))

(defvar spamemo-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") #'spamemo-rate-forgot)
    (define-key map (kbd "2") #'spamemo-rate-hard)
    (define-key map (kbd "3") #'spamemo-rate-good)
    (define-key map (kbd "4") #'spamemo-rate-easy)
    (define-key map (kbd "l") #'spamemo-lookup-current-word)
    (define-key map (kbd "q") #'spamemo-quit)
    (define-key map (kbd "n") #'spamemo-review-next-word)
    (define-key map (kbd "g") #'spamemo-review-refresh)
    map)
  "Keymap for `spamemo-review-mode'.")

(define-derived-mode spamemo-review-mode special-mode "SpaMemo"
  "Major mode for reviewing vocabulary with FSRS algorithm."
  (setq buffer-read-only t)
  (buffer-disable-undo))

(provide 'spamemo)
;;; spamemo.el ends here

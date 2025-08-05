;;; spamemo-calendar.el --- Calendar integration for spamemo -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Yilin Zhang
;; Keywords: spaced-repetition, calendar
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (spamemo "0.1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides calendar integration for spamemo,
;; visualizing due dates on the Emacs calendar.
;;
;; Key features:
;; - Highlights dates with due cards on the calendar
;; - Different colors indicate the number of due cards
;; - Customizable appearance and behavior

;;; Code:

(require 'spamemo)
(require 'calendar)

(defgroup spamemo-calendar nil
  "Calendar integration for spamemo."
  :group 'spamemo)

(defcustom spamemo-calendar-days-ahead 90
  "Number of days ahead to calculate due cards for calendar display."
  :type 'integer
  :group 'spamemo-calendar)

(defcustom spamemo-calendar-thresholds
  '(1 5 10 20)
  "List of thresholds for highlighting calendar dates with due cards.
Each value represents the minimum number of cards for a new color level.
Default levels: 1-4 cards (level 1), 5-9 (level 2), 10-19 (level 3), 20+ (level 4)."
  :type '(repeat integer)
  :group 'spamemo-calendar)

(defface spamemo-calendar-level-1-face
  '((t :inherit (success bold) :inverse-video t))
  "Face for dates with few due cards (level 1)."
  :group 'spamemo-calendar)

(defface spamemo-calendar-level-2-face
  '((t :inherit (warning bold) :inverse-video t))
  "Face for dates with some due cards (level 2)."
  :group 'spamemo-calendar)

(defface spamemo-calendar-level-3-face
  '((t :inherit (font-lock-keyword-face bold) :inverse-video t))
  "Face for dates with many due cards (level 3)."
  :group 'spamemo-calendar)

(defface spamemo-calendar-level-4-face
  '((t :inherit (error bold) :inverse-video t))
  "Face for dates with very many due cards (level 4)."
  :group 'spamemo-calendar)

;; Internal functions

(defun spamemo-calendar--get-face-for-count (count)
  "Return the appropriate face for COUNT due cards."
  (let ((thresholds spamemo-calendar-thresholds))
    (cond
     ((< count (nth 1 thresholds)) 'spamemo-calendar-level-1-face)
     ((< count (nth 2 thresholds)) 'spamemo-calendar-level-2-face)
     ((< count (nth 3 thresholds)) 'spamemo-calendar-level-3-face)
     (t 'spamemo-calendar-level-4-face))))

(defun spamemo-calendar--get-due-date (meta)
  "Calculate when a card with META will be due.
Returns a time value."
  (let* ((last-review-time (spamemo--parse-iso-date
                            (spamemo-word-meta-last-review meta)))
         (interval-days (spamemo-word-meta-interval meta))
         (due-time (time-add last-review-time
                             (seconds-to-time (* interval-days 86400)))))
    due-time))

(defun spamemo-calendar--time-to-date (time)
  "Convert time value TIME to (month day year) format used by calendar."
  (let ((decoded (decode-time time)))
    (list (nth 4 decoded)  ; month
          (nth 3 decoded)  ; day
          (nth 5 decoded)))) ; year

(defun spamemo-calendar--get-due-cards-by-date ()
  "Get a hash table mapping due dates to card counts.
Dates are in (month day year) format used by calendar.
Looks up to `spamemo-calendar-days-ahead' days into the future."
  (unless spamemo-deck
    (setq spamemo-deck (spamemo--load-deck)))

  (let* ((due-counts (make-hash-table :test 'equal))
         (now (current-time))
         (now-date (spamemo-calendar--time-to-date now))
         (cutoff (time-add (current-time)
                           (seconds-to-time (* 86400 spamemo-calendar-days-ahead)))))

    ;; Go through all words in the deck
    (maphash (lambda (_word meta)
               (let* ((due-time (spamemo-calendar--get-due-date meta))
                      (due-date (spamemo-calendar--time-to-date due-time)))

                 ;; Only include dates that are in the future and before cutoff
                 (when (and (time-less-p now due-time)
                            (time-less-p due-time cutoff))
                   ;; Increment the count for this date
                   (let ((count (gethash due-date due-counts 0)))
                     (puthash due-date (1+ count) due-counts)))

                 ;; If the card is already due, count it for today
                 (unless (time-less-p now due-time)
                   (let ((count (gethash now-date due-counts 0)))
                     (puthash now-date (1+ count) due-counts)))))
             spamemo-deck)
    due-counts))

(defun spamemo-calendar--mark-dates-with-due-cards ()
  "Mark calendar dates with due cards using appropriate faces."
  (let ((due-counts (spamemo-calendar--get-due-cards-by-date)))
    (maphash (lambda (date count)
               (let* ((due-month (nth 0 date))
                      (due-year (nth 2 date))
                      (due-n-month (+ due-month (* 12 due-year)))
                      (max-n-month (+ (1+ displayed-month) (* 12 displayed-year)))
                      (min-n-month (+ (1- displayed-month) (* 12 displayed-year)))
                      )
                 ;; filter out dates that are not currently displayed
                 (when (and (<= due-n-month max-n-month)
                            (<= min-n-month due-n-month))
                   (let ((face (spamemo-calendar--get-face-for-count count)))
                     (calendar-mark-visible-date date face)))))
             due-counts)))

;;;###autoload
(defun spamemo-calendar-mark-due-dates ()
  "Mark dates with due cards on the calendar."
  (interactive)
  ;; First clear any existing marks
  (calendar-unmark)
  ;; Then mark dates with due cards
  (spamemo-calendar--mark-dates-with-due-cards)
  (message "Marked dates with due cards"))

;;;###autoload
(defun spamemo-calendar-show-due-count-at-date ()
  "Show the number of cards due on the selected date."
  (interactive)
  (let* ((date (calendar-cursor-to-date))
         (due-counts (spamemo-calendar--get-due-cards-by-date))
         (count (gethash date due-counts 0)))
    (if (> count 0)
        (message "%d card%s due on %s"
                 count
                 (if (= count 1) "" "s")
                 (calendar-date-string date)))))

;; Define a minor mode for the calendar integration
(define-minor-mode spamemo-calendar-mode
  "Toggle spamemo calendar integration.
When enabled, dates with due cards are highlighted in the calendar."
  :global t
  :lighter " SpaCal"
  :group 'spamemo-calendar
  (if spamemo-calendar-mode
      (progn
        (add-hook 'calendar-today-visible-hook 'spamemo-calendar-mark-due-dates)
        (add-hook 'calendar-today-invisible-hook 'spamemo-calendar-mark-due-dates)
        (add-hook 'calendar-move-hook 'spamemo-calendar-show-due-count-at-date))
    (remove-hook 'calendar-today-visible-hook 'spamemo-calendar-mark-due-dates)
    (remove-hook 'calendar-today-invisible-hook 'spamemo-calendar-mark-due-dates)
    (remove-hook 'calendar-move-hook 'spamemo-calendar-show-due-count-at-date)))

(provide 'spamemo-calendar)
;;; spamemo-calendar.el ends here

;;; macmount.el --- Mount and eject external disks on macOS -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Yilin Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; macmount provides simple commands to manage external storage devices on
;; macOS by calling diskutil:
;;
;; - `macmount-mount`: select an unmounted, mountable external volume.
;; - `macmount-unmount`: unmount an external disk but keep it available.
;; - `macmount-eject`: select a present external disk and eject it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup macmount nil
  "Manage external disks on macOS with diskutil."
  :group 'tools
  :prefix "macmount-")

(defcustom macmount-diskutil-program "diskutil"
  "Path to the diskutil executable."
  :type 'string
  :group 'macmount)

(defcustom macmount-plutil-program "plutil"
  "Path to the plutil executable."
  :type 'string
  :group 'macmount)

(defun macmount--jget (obj key)
  "Get KEY from JSON alist OBJ.
KEY must be a string."
  (alist-get key obj nil nil #'string=))

(defun macmount--mounted-p (info)
  "Return non-nil if INFO represents a mounted volume."
  (let ((mount-point (macmount--jget info "MountPoint")))
    (and (stringp mount-point)
         (not (string-empty-p mount-point)))))

(defun macmount--external-p (info)
  "Return non-nil if INFO represents an external/removable device."
  (let ((flag (macmount--jget info "RemovableMediaOrExternalDevice")))
    (if (null flag)
        (not (macmount--jget info "Internal"))
      flag)))

(defun macmount--run (program &rest args)
  "Run PROGRAM with ARGS and return stdout as string.
Signal an error if command exits non-zero."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process
                            program
                            nil
                            (list t t)
                            nil
                            args)))
      (if (eq exit-code 0)
          (buffer-string)
        (error "%s failed (%s): %s"
               program
               exit-code
               (string-trim (buffer-string)))))))

(defun macmount--plist-xml-to-json (xml)
  "Convert diskutil XML plist string XML into JSON text."
  (with-temp-buffer
    (insert xml)
    (let ((exit-code (call-process-region (point-min)
                                          (point-max)
                                          macmount-plutil-program
                                          t
                                          (current-buffer)
                                          nil
                                          "-convert" "json" "-o" "-" "--" "-")))
      (if (eq exit-code 0)
          (buffer-string)
        (error "Failed to convert plist to JSON via %s"
               macmount-plutil-program)))))

(defun macmount--diskutil-json (&rest args)
  "Run diskutil with ARGS and return parsed JSON as alist/list."
  (let* ((xml (apply #'macmount--run macmount-diskutil-program args))
         (json-text (macmount--plist-xml-to-json xml)))
    (json-parse-string json-text
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

(defun macmount--disk-info (device-id)
  "Return `diskutil info -plist` JSON alist for DEVICE-ID."
  (macmount--diskutil-json "info" "-plist" device-id))

(defun macmount--format-bytes (bytes)
  "Convert BYTES into a compact human-readable string."
  (if (not (numberp bytes))
      "?"
    (let ((units ["B" "KB" "MB" "GB" "TB" "PB"])
          (size (float bytes))
          (idx 0))
      (while (and (>= size 1024.0)
                  (< idx (1- (length units))))
        (setq size (/ size 1024.0)
              idx (1+ idx)))
      (if (= idx 0)
          (format "%d%s" bytes (aref units idx))
        (format "%.1f%s" size (aref units idx))))))

(defun macmount--volume-display (info)
  "Create completion label for a volume INFO alist."
  (let* ((id (or (macmount--jget info "DeviceIdentifier") "<unknown>"))
         (name (or (macmount--jget info "VolumeName")
                   (macmount--jget info "MediaName")
                   "Untitled"))
         (fs (or (macmount--jget info "FilesystemType")
                 (macmount--jget info "FilesystemName")
                 "unknown-fs"))
         (size (macmount--format-bytes
                (or (macmount--jget info "TotalSize")
                    (macmount--jget info "Size")))))
    (format "%s (%s) [%s, %s]" name id fs size)))

(defun macmount--disk-display (info)
  "Create completion label for an external whole-disk INFO alist."
  (let* ((id (or (macmount--jget info "DeviceIdentifier") "<unknown>"))
         (name (or (macmount--jget info "VolumeName")
                   (macmount--jget info "MediaName")
                   "External Disk"))
         (bus (or (macmount--jget info "BusProtocol") "Unknown Bus"))
         (size (macmount--format-bytes
                (or (macmount--jget info "TotalSize")
                    (macmount--jget info "Size")))))
    (format "%s (%s) [%s, %s]" name id bus size)))

(defun macmount--list-tree-external ()
  "Return JSON tree for external disks and related logical volumes."
  (macmount--diskutil-json "list" "-plist" "external"))

(defun macmount--list-tree-external-physical ()
  "Return JSON tree for external physical disks only."
  (macmount--diskutil-json "list" "-plist" "external" "physical"))

(defun macmount--entry-partitions (entry)
  "Return all partition-like children from diskutil ENTRY."
  (append (or (macmount--jget entry "Partitions") '())
          (or (macmount--jget entry "APFSVolumes") '())))

(defun macmount--mounted-apfs-on-whole-disk-p (all-entry whole-disk-id)
  "Return non-nil if ALL-ENTRY maps mounted APFS volume to WHOLE-DISK-ID."
  (let ((stores (macmount--jget all-entry "APFSPhysicalStores"))
        (volumes (macmount--jget all-entry "APFSVolumes")))
    (and (listp stores)
         (listp volumes)
         (cl-some
          (lambda (store)
            (let ((store-id (macmount--jget store "DeviceIdentifier")))
              (and (stringp store-id)
                   (string-prefix-p (concat whole-disk-id "s") store-id))))
          stores)
         (cl-some #'macmount--mounted-p volumes))))

(defun macmount--whole-disk-mounted-p (all-tree whole-disk-id)
  "Return non-nil when WHOLE-DISK-ID has at least one mounted volume.
ALL-TREE is the result of `diskutil list -plist external`."
  (let* ((entries (or (macmount--jget all-tree "AllDisksAndPartitions") '()))
         (direct-entry (cl-find-if
                        (lambda (entry)
                          (string= (or (macmount--jget entry "DeviceIdentifier") "")
                                   whole-disk-id))
                        entries))
         (direct-children (if direct-entry
                               (macmount--entry-partitions direct-entry)
                             '())))
    (or (and direct-entry (macmount--mounted-p direct-entry))
        (cl-some #'macmount--mounted-p direct-children)
        (cl-some
         (lambda (entry)
           (macmount--mounted-apfs-on-whole-disk-p entry whole-disk-id))
         entries))))

(defun macmount--mount-candidates ()
  "Return alist (DISPLAY . DEVICE-ID) for unmounted mountable volumes."
  (let* ((tree (macmount--list-tree-external))
         (entries (or (macmount--jget tree "AllDisksAndPartitions") '()))
         (all-ids
          (delete-dups
           (delq nil
                 (append
                  (mapcar (lambda (entry)
                            (macmount--jget entry "DeviceIdentifier"))
                          entries)
                  (cl-mapcan
                   (lambda (entry)
                     (mapcar (lambda (child)
                               (macmount--jget child "DeviceIdentifier"))
                             (macmount--entry-partitions entry)))
                   entries)))))
         (candidates '()))
    (dolist (id all-ids)
      (let* ((info (macmount--disk-info id))
             (has-fs (or (macmount--jget info "FilesystemType")
                          (macmount--jget info "FilesystemName"))))
        (when (and (macmount--external-p info)
                   has-fs
                   (not (macmount--mounted-p info)))
          (push (cons (macmount--volume-display info) id)
                candidates))))
    (sort candidates (lambda (a b)
                       (string-lessp (car a) (car b))))))

(defun macmount--eject-candidates ()
  "Return alist (DISPLAY . WHOLE-DISK-ID) for present ejectable external disks."
  (let* ((physical-tree (macmount--list-tree-external-physical))
         (whole-disks (or (macmount--jget physical-tree "WholeDisks") '()))
         (candidates '()))
    (dolist (id whole-disks)
      (let ((info (macmount--disk-info id)))
        (when (and (macmount--jget info "Ejectable")
                   (macmount--external-p info))
          (push (cons (macmount--disk-display info) id)
                candidates))))
    (sort candidates (lambda (a b)
                       (string-lessp (car a) (car b))))))

(defun macmount--pick-device-id (prompt candidates)
  "Prompt with PROMPT over CANDIDATES and return selected device id.
CANDIDATES is an alist of (DISPLAY . DEVICE-ID)."
  (let* ((choices (mapcar #'car candidates))
         (selected (completing-read prompt choices nil t)))
    (cdr (assoc selected candidates))))

(defun macmount--mounted-external-candidates ()
  "Return alist (DISPLAY . WHOLE-DISK-ID) for mounted external disks."
  (let* ((all-tree (macmount--list-tree-external))
         (physical-tree (macmount--list-tree-external-physical))
         (whole-disks (or (macmount--jget physical-tree "WholeDisks") '()))
         (candidates '()))
    (dolist (id whole-disks)
      (let ((info (macmount--disk-info id)))
        (when (and (macmount--jget info "Ejectable")
                   (macmount--external-p info)
                   (macmount--whole-disk-mounted-p all-tree id))
          (push (cons (macmount--disk-display info) id)
                candidates))))
    (sort candidates (lambda (a b)
                       (string-lessp (car a) (car b))))))

;;;###autoload
(defun macmount-mount ()
  "Interactively mount an unmounted external volume."
  (interactive)
  (let ((candidates (macmount--mount-candidates)))
    (if (null candidates)
        (user-error "No unmounted mountable external volumes found")
      (let* ((id (macmount--pick-device-id "Mount volume: " candidates))
             (info (macmount--disk-info id))
             (cmd (if (macmount--jget info "WholeDisk") "mountDisk" "mount"))
             (output (macmount--run macmount-diskutil-program cmd id)))
        (message "%s" (string-trim output))))))

;;;###autoload
(defun macmount-unmount ()
  "Interactively unmount a mounted external disk without ejecting it."
  (interactive)
  (let ((candidates (macmount--mounted-external-candidates)))
    (if (null candidates)
        (user-error "No mounted external disks available to unmount")
      (let* ((id (macmount--pick-device-id "Unmount disk: " candidates))
             (output (macmount--run macmount-diskutil-program "unmountDisk" id)))
        (message "%s" (string-trim output))))))

;;;###autoload
(defun macmount-eject ()
  "Interactively eject a present external disk."
  (interactive)
  (let ((candidates (macmount--eject-candidates)))
    (if (null candidates)
        (user-error "No external disks available to eject")
      (let* ((id (macmount--pick-device-id "Eject disk: " candidates))
             (output (macmount--run macmount-diskutil-program "eject" id)))
        (message "%s (reinsert/replug to mount again)" (string-trim output))))))

(provide 'macmount)

;;; macmount.el ends here

;nyquist plug-in
;version 4
;type process
;name "Fill Gaps..."
;action "Filling Gaps..."
;author "Alexander Lopatin"
;copyright "Released under terms of the GNU General Public License version 2"
;control action "Action" choice "Capture background,Fill gaps" "Capture background"

(setq temp-dir "/var/tmp/")
(setq mixer "sox")
(setq min-gap-duration 0.05)
(setq transition-duration 0.5)

(setq input-file (strcat temp-dir "audacity-input.wav"))
(setq output-file (strcat temp-dir "audacity-output.wav"))
(setq background-input-file (strcat temp-dir "audacity-background-input.wav"))

(setq executable-paths
    (strcat "PATH=\"${HOME}/.audacity-files/plug-ins/:" *runtime-path* "../plug-ins/:${PATH}\""))
(setq main-command (format nil "~A fill-gaps.rb -i ~S -b ~S -o ~S -t ~S -g ~S -m ~S -d ~S"
                           executable-paths
                           input-file background-input-file output-file
                           transition-duration min-gap-duration
                           mixer temp-dir))

(setq cleanup-command (strcat "rm -f " background-input-file))

(defun capture-background-sound ()
  (system cleanup-command)
  (s-save *track* background-input-file))

(defun fill-gaps ()
  (s-save *track* input-file)
  (system main-command)
  (sim (s-rest)
       (s-read output-file)))

(if (= action 0)
  (capture-background-sound)
  (fill-gaps))

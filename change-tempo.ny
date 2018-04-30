;nyquist plug-in
;version 1
;type process
;name "Change Tempo..."
;action "Changing Tempo..."
;author "Alexander Lopatin"
;copyright "Released under terms of the GNU General Public License version 2"
;control factor "Factor" float "" 0.2 0.5 2.0

(setq temp-dir "/var/tmp/")
(setq input-file (strcat temp-dir "audacity-input.wav"))
(setq output-file (strcat temp-dir "audacity-output.wav"))

(setq cleanup-command (strcat "rm -f " input-file " " output-file))
(setq main-command (format nil "ffmpeg -v error -i ~S -filter:a atempo=~S -vn ~S" input-file factor output-file))

(system cleanup-command)
(s-save s input-file)
(system main-command)
(s-read output-file)

;nyquist plug-in
;version 1
;type process
;name "Change Tempo"
;action "Changing Tempo..."
;author "Alexander Lopatin"
;copyright "Released under terms of the GNU General Public License version 2"
;debugflags trace

(setq temp-dir "/var/tmp/")
(setq input-file (strcat temp-dir "audacity-input.wav"))
(setq output-file (strcat temp-dir "audacity-output.wav"))

(setq main-command (strcat "ffmpeg -v error -i " input-file " -filter:a atempo=2.0 -vn " output-file))
(setq cleanup-command (strcat "rm -f " input-file " " output-file))

(system cleanup-command)
(s-save s input-file)
(system main-command)
(s-read output-file)

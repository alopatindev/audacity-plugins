; https://github.com/alopatindev/audacity-plugins
; Copyright (C) 2018  Alexander Lopatin

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; nyquist plug-in
; version 4
; type process
; name "Change Tempo..."
; action "Changing Tempo..."
; author "Alexander Lopatin"
; copyright "Released under terms of the GNU General Public License version 2"
; control factor "Factor" float "" 0.2 0.5 2.0

(setq temp-dir "/var/tmp/")
(setq input-file (strcat temp-dir "audacity-input.wav"))
(setq output-file (strcat temp-dir "audacity-output.wav"))

(setq cleanup-command (strcat "rm -f " input-file " " output-file))
(setq main-command (format nil "ffmpeg -y -hide_banner -loglevel quiet -i ~S -filter:a atempo=~S -vn ~S" input-file factor output-file))

(system cleanup-command)
(s-save *track* input-file)
(system main-command)
(s-read output-file)

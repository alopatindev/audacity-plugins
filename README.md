# Plugins

## Change Tempo
Speeds up a sound without producing too much quality loss.

## Fill Gaps
Designed to fill empty spaces with background noise.

# Installation
1. `mkdir -p ~/.audacity-files && git clone https://github.com/alopatindev/audacity-plugins ~/.audacity-files/plug-ins`
2. Run Audacity, click Effect — Add / Remove Plugins...
    - Show: Enabled — Change Tempo — Disable
    - Show: New
        - enable everything that matches the path `${HOME}/.audacity-files/plug-ins/*.ny`

## Dependencies
- POSIX-compatible OS (tested with GNU/Linux)
- a **[patch for Audacity](https://raw.githubusercontent.com/alopatindev/gentoo-overlay-alopatindev/master/media-sound/audacity/files/audacity-9999-xsystem.patch)** which enables [a feature](https://forum.audacityteam.org/viewtopic.php?p=346798#p346798) that was removed for unknown reason
- ffmpeg (tested with 3.3.6)
- sox (tested with 14.4.2)
- ruby (tested with 2.5.1)

# License
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Copyright (C) 2018  Alexander Lopatin <alopatindev ät gmail dot com>

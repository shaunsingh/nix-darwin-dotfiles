#!/usr/bin/env python3

from ImageGoNord import GoNord
import sysconfig
import os, sys
go_nord = GoNord()

go_nord.enable_avg_algorithm()
go_nord.set_palette_lookup_path(sysconfig.get_paths()['purelib']+"/ImageGoNord/palettes/Nord/");
image = go_nord.open_image(os.path.expanduser( sys.argv[1] )) 
go_nord.quantize_image(image, save_path = os.path.expanduser(sys.argv[2]))

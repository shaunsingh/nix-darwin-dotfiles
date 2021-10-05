swayidle \
	timeout 60 '~/.config/sway/lock.sh --grace 10 --fade-in 4' \
	timeout 80 'swaymsg "output * dpms off"' \
	resume 'swaymsg "output * dpms on"; sleep 2; swaymsg "output * enable"' \
	before-sleep 'pgrep swaylock || ~/.config/scripts/lock.sh -f --fade-in 0 && true'

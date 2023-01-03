#!/usr/bin/env bash

monitor="$1"

gib_workspace_names() {
	wmctrl -d |
		awk '{ print $1 " " $2 " " $9 }' |
		grep -v NSP |
		grep "${monitor}_"
}

gib_workspace_yuck() {
	buffered=""
	gib_workspace_names | while read -r id active name; do
		name="${name#*_}"
		if [ "$active" == '*' ]; then
			active_class="active"
		else
			active_class="inactive"
		fi

		if wmctrl -l | grep --regexp '.*\s\+'"$id"'\s\+.*' >/dev/null; then
			button_class="occupied"
			button_name="◆"
		else
			button_class="empty"
			button_name="◇"
		fi
		buffered+="(button :class \"$button_class $active_class\"  :onclick \"wmctrl -s $id\" \"$button_name\")"
		if [ $button_class = "occupied" -o $active_class = "active" ]; then
			echo -n "$buffered"
			buffered=""
		fi
	done
}

xprop -spy -root _NET_CURRENT_DESKTOP | while read -r; do
	echo '(box :orientation "v" :class "workspaces" :space-evenly true :halign "center" :valign "center" :vexpand true '"$(gib_workspace_yuck)"')'
done

for f in *.png; do convert "$f" \( +clone -background black -shadow 40x50+0+36 \) +swap -background transparent -layers merge +repage "${f%.png}-shadow.png"; done

all: force
	R --no-save <oce-demo-1.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-1-thumbnail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-1.png
	R --no-save <oce-demo-2.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-2-thumbnail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-2.png
	R --no-save <oce-demo-3.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-3-thumbnail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-3.png
	R --no-save <oce-demo-4.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-4-thumbnail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-4.png
	R --no-save <oce-demo-5.R
	convert -background white -geometry '150x150' oce-demo-5.png oce-demo-5-thumbnail.png
	R --no-save <oce-demo-6.R
	convert -background white -geometry '150x150' oce-demo-6.png oce-demo-6-thumbnail.png

force:
clean:
	rm -f *~
	rm -f Rplots.pdf
	rm -f *png

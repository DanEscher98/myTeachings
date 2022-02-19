TARGET = jdfedora
EXT = rst

pdf: $(TARGET)/README.$(EXT)
	pandoc $(TARGET)/README.$(EXT) -o README.pdf --standalone --toc
	cp README.pdf $(HOME)/MyWindows/Working/

clear:
	rm -f *.pdf
	clear && ls

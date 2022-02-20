TARGET	= peano
DIR 	= a_arithmetic
FLAGS 	= --toc --standalone #--citeproc

pdf :
	pandoc $(DIR)/$(TARGET).md -o $(TARGET).pdf $(FLAGS)
	cp $(TARGET).pdf $(HOME)/MyWindows/Working/

html :
	pandoc $(TARGET).md -o $(TARGET).html $(FLAGS)

docx :
	pandoc $(TARGET).md -o $(TARGET).docx $(FLAGS)

clear :
	rm -f *.docx *.html *.pdf

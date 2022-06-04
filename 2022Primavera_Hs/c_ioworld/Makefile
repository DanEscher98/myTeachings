CODE = todoList
TARGET	=
DIR 	= .
FLAGS 	= --toc --standalone #--citeproc

$(DIR)/$(CODE).out : $(DIR)/$(CODE).hs
	ghc $(DIR)/$(CODE).hs -o $(DIR)/$(CODE).out

run : $(DIR)/$(CODE).out
	./$(DIR)/$(CODE).out

pdf :
	pandoc $(DIR)/$(TARGET).md -o $(TARGET).pdf $(FLAGS)
	cp $(TARGET).pdf $(HOME)/MyWindows/Working/

clear :
	rm -f $(DIR)/{*.pdf, *.out, *.hi, *.o}


CIR=./miniscm-ir.scm
CAS=./ir-x86.scm
GSC=gsc -exe
RM=rm -f

# Point d'entrée principal
all: miniscm-ir ir-x86 rapport
rapport: rapport.pdf

# Comment construire "ch".
miniscm-ir: source/miniscm-ir.scm lib/reader.scm
	$(GSC) -o bin/miniscm-ir source/miniscm-ir.scm lib/reader.scm

ir-x86: source/ir-x86.scm lib/reader.scm
	$(GSC) -o bin/ir-x86 source/ir-x86.scm lib/reader.scm

clean:
	$(RM) bin/miniscm-ir bin/ir-x86 *.pdf *~ *.log 

.SUFFIXES: .md .pdf

# Règle simpliste pour générer le PDF à partir du source LaTeX.
rapport.pdf: rapport.md
	pandoc rapport.md -o rapport.pdf

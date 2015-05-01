all: laender.txt staedte.txt

laender.txt: DeutschlandMitStaedten.svg
	./extract-laender.sh > laender.txt

staedte.txt: DeutschlandMitStaedten.svg
	./extract-staedte.sh > staedte.txt

clean:
	rm -f laender.txt staedte.txt

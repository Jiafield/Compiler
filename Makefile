P1: P1.tab.o lex.yy.o P1funcs.o
	gcc -g -Wall -o P1 P1.tab.o lex.yy.o P1funcs.o

P1.tab.o: P1.y
	bison -d P1.y
	gcc -g -Wall -c P1.tab.c

lex.yy.o: P1.l
	flex P1.l
	gcc -g -Wall -c lex.yy.c

P1funcs.o: P1funcs.c
	gcc -g -Wall -c P1funcs.c

clean:
	rm -f P1 lex.yy.c P1.tab.c P1.tab.h *.o *~

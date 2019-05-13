a.out: semanttest.o y.tab.o lex.yy.o errormsg.o symbol.o absyn.o env.o types.o semant.o table.o util.o temp.o x86_64frame.o translate.o printtree.o tree.o
	cc -g semanttest.o y.tab.o lex.yy.o errormsg.o symbol.o absyn.o env.o types.o semant.o table.o util.o temp.o x86_64frame.o translate.o printtree.o tree.o

%.o: %.h %.c
	cc -g -c $*.c

y.tab.c: tiger.grm
	yacc -dv tiger.grm

y.tab.h: y.tab.c

lex.yy.c: tiger.lex
	lex tiger.lex

x86_64frame.o: frame.h x86_64frame.c
	cc -g -c x86_64frame.c

clean: 
	rm -f a.out *.o y.tab.* lex.yy.c y.output

all-tests := $(addsuffix .test, $(notdir $(basename $(wildcard ./testcases/*.tig))))

test: $(all-tests)

test49.test:
	@echo "Skip test49.tig"

%.test: ./testcases/%.tig
	./a.out $< > testoutput/$@ 2>&1
	echo "==============================" >> testoutput/$@
	cat $< >> testoutput/$@

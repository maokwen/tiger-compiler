SUBDIRS = chap01 chap02 chap03 chap04 chap05 chap06 chap07 chap08 chap09 chap10 chap11 chap12

clean:
	for dir in $(SUBDIRS); \
	do $(MAKE) -C $$dir clean || exit 1; \
	done

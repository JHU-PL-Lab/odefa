
RESULT=results.txt
ANALYZE=analyze.txt
ARCHIVE=cade_17.tar.gz

all: check_all $(RESULT) $(ANALYZE)

clean:
	rm -f $(RESULT) $(ARCHIVE)

TO_ARCHIVE=$(RESULT) $(ANALYZE) README.md Makefile *.py \
	   $(wildcard **/*.smt2) $(wildcard *.sh)

archive: $(RESULT) $(ANALYZE)
	@echo build archive $(ARCHIVE)
	tar --transform 's,^,smbc_data/,' \
	  -cavf $(ARCHIVE) $(TO_ARCHIVE)

upload_archive: archive
	scp $(ARCHIVE) cedeela.fr:~/simon/root/files/

results.txt:
	@./run_all.sh | tee $@

analyze.txt: $(RESULT)
	@./analyze.py $(RESULT) | tee $@

check_all:
	@echo checking for programs…
	@ls /usr/bin/time
	@which cvc4 > /dev/null
	@which smbc > /dev/null
	@which hbmc > /dev/null
	@which runghc > /dev/null


.PHONY: check_all clean

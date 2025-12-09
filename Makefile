DAYS_INPUT = $(patsubst ./inputs/day%.txt,%,$(wildcard ./inputs/day*.txt))
DAYS_JAVA = $(patsubst ./src/day%/Solution.java,%,$(wildcard ./src/day*/Solution.java))
DAYS_IDR2 = $(patsubst ./src/day%/Solution.idr,%,$(wildcard ./src/day*/Solution.idr))

SHELL=zsh

ifdef DAY
	DAY_SRC := cli
else
	DAY := $(shell date +%-d)
	DAY_SRC := auto
endif

ifdef VALIDATE
	override VALIDATE = -v
endif

ifdef PART2
	override PART2 = -2
endif

.SILENT: list

d%-java: ./src/day%/Solution.java
	ln -sf ../solutions/day$*.txt ./java/solution.txt
	ln -sf ../solutions/day$*-2.txt ./java/solution2.txt
	ln -sf ../inputs/day$*.txt ./java/input.txt
	ln -sf ../examples/day$*.txt ./java/example.txt
	
	ln -sf ../$< ./java/Solution.java
	
	cd ./java ; java Main.java $(VALIDATE) $(PART2)

# Default is to use the example and solution
# set VALIDATE to use the input
d%-idr: ./src/day%/Solution.idr
	ln -sf ../solutions/day$*.txt ./idris/solution.txt
	ln -sf ../solutions/day$*-2.txt ./idris/solution2.txt
	ln -sf ../inputs/day$*.txt ./idris/input.txt
	ln -sf ../examples/day$*.txt ./idris/example.txt
	
	ln -sf ../../$< ./idris/src/Solution.idr
	
	cd ./idris ; pack build && ./build/exec/main $(VALIDATE) $(PART2)

# Could be fun to write a small program that checks
# if the how many puzzles are available, how many input+solution are here
# And how many days are implement and working/failing.
list:
	echo "Java Solutions:"
	for day in $(DAYS_JAVA); do echo Day $$day avalaible; done
	echo ""
	echo "Idris Solutions:"
	for day in $(DAYS_IDR2); do echo Day $$day avalaible; done


time-all:
	@for day in $(DAYS_IDR2); do \
		make select-day DAY=$$day > /dev/null ;\
		cd ./idris > /dev/null ;\
		touch ./src/Solution.idr > /dev/null ;\
		pack build > /dev/null ;\
		echo "\tDay $$day" ;\
		time ./build/exec/main -v ;\
		time ./build/exec/main -v -2 ;\
		cd ./.. > /dev/null ; \
	done

# set the link to the correct day in build
select-day:
	@if [ "$(DAY_SRC)" = "auto" ]; then \
		echo "Day implicitly assumed to be today"; \
		echo "Use DAY=x for explicit assignement"; \
	fi
	@if [ "$(DAY)" -gt 12 ]; then \
		echo "max day is 12"; \
		exit 1; \
	fi
	
	ln -sf ../../src/day$(DAY)/Solution.idr ./idris/src/Solution.idr
	ln -sf ../src/day$(DAY)/Solution.java ./java/Solution.java
	ln -sf ../examples/day$(DAY).txt ./java/example.txt
	ln -sf ../examples/day$(DAY).txt ./idris/example.txt
	ln -sf ../solutions/day$(DAY).txt ./java/solution.txt
	ln -sf ../solutions/day$(DAY)-2.txt ./java/solution2.txt
	ln -sf ../solutions/day$(DAY).txt ./idris/solution.txt
	ln -sf ../solutions/day$(DAY)-2.txt ./idris/solution2.txt
	ln -sf ../inputs/day$(DAY).txt ./java/input.txt
	ln -sf ../inputs/day$(DAY).txt ./idris/input.txt
	@echo "Set links to day $(DAY)"

init-java:
	@if [ "$(DAY)" -gt 12 ]; then \
		echo "max day is 12"; \
		exit 1; \
	fi
	if [ ! -e "./src/day$(DAY)" ]; then mkdir ./src/day$(DAY); fi
	cp ./templates/Solution.java ./src/day$(DAY)/Solution.java
	touch ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt ./solutions/day$(DAY)-2.txt
	vim -p ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt ./solutions/day$(DAY)-2.txt

init-idr:
	@if [ "$(DAY)" -gt 12 ]; then \
		echo "max day is 12"; \
		exit 1; \
	fi
	if [ ! -e "./src/day$(DAY)" ]; then mkdir ./src/day$(DAY); fi
	cp ./templates/Solution.idr ./src/day$(DAY)/Solution.idr
	touch ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt ./solutions/day$(DAY)-2.txt
	vim -p ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt ./solutions/day$(DAY)-2.txt

DAYS_INPUT = $(patsubst ./inputs/day%.txt,%,$(wildcard ./inputs/day*.txt))
DAYS_JAVA = $(patsubst ./src/day%/Solution.java,%,$(wildcard ./src/day*/Solution.java))
DAYS_IDR2 = $(patsubst ./src/day%/Solution.idr,%,$(wildcard ./src/day*/Solution.idr))

SHELL=zsh

.PHONY: $(DAYS_JAVA) $(DAYS_IDR2)
.SILENT: list


d%-java: ./src/day%/Solution.java
	ln -sf ../solutions/day$*.txt ./java/solution.txt
	ln -sf ../inputs/day$*.txt ./java/input.txt
	ln -sf ../examples/day$*.txt ./java/example.txt
	
	ln -sf ../$< ./java/Solution.java
	
	cd ./java ; java Main.java $(VALIDATE)

# Default is to use the example and solution
# set VALIDATE to use the input
d%-idr: ./src/day%/Solution.idr
	ln -sf ../solutions/day$*.txt ./idris/solution.txt
	ln -sf ../inputs/day$*.txt ./idris/input.txt
	ln -sf ../examples/day$*.txt ./idris/example.txt
	
	ln -sf ../../$< ./idris/src/Solution.idr
	
	cd ./idris ; pack build && ./build/exec/main $(VALIDATE)

# Could be fun to write a small program that checks
# if the how many puzzles are available, how many input+solution are here
# And how many days are implement and working/failing.
list:
	echo "Java Solutions:"
	for day in $(DAYS_JAVA); do echo Day $$day avalaible; done
	echo ""
	echo "Idris Solutions:"
	for day in $(DAYS_IDR2); do echo Day $$day avalaible; done

prepare-java:
	@if [ -z $(DAY) ]; then echo "please specify a day with DAY=xx"; exit -1; fi
	cp ./templates/Solution.java ./src/day$(DAY)/Solution.java
	touch ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt
	@echo "created empty files for input,example,solution, please fill them"

prepare-idr:
	@if [ -z $(DAY) ]; then echo "please specify a day with DAY=xx"; exit -1; fi
	cp ./templates/Solution.idr ./src/day$(DAY)/Solution.idr
	touch ./examples/day$(DAY).txt ./inputs/day$(DAY).txt ./solutions/day$(DAY).txt
	@echo "created empty files for input,example,solution, please fill them"

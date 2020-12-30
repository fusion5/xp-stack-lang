SRCFILES = ELFHeader64.hs PEHeader64.hs Main.hs \
 ASM/ASM.hs ASM/Datatypes.hs ASM/Pretty.hs \
 Lang/Lang.hs Lang/Debug.hs Lang/Linux.hs \
 Lang/BasicFunctions.hs \
 Lang/Parsing.hs Lang/EmitCode.hs \
 X86/Datatypes.hs X86/X86.hs X86/Tests.hs

CUTPATH = C:\tools\msys64\usr\bin\cut.exe
XXD="C:\Program Files (x86)\Vim\vim82\xxd.exe"

asmgen: $(SRCFILES)
	ghc Main.hs -o $@

asmgen.exe: $(SRCFILES)
	ghc Main.hs -o $@

# Test x86 code generation: what NASM Generates
# 'cut' is used to remove the first 10 chars of each line
x86_test_nasm.listing: asmgen.exe Makefile
	echo BITS 64 > tmp.nasm
	.\asmgen.exe test_x86_n >> tmp.nasm
	nasm -O0 -fbin tmp.nasm -o tmp.bin
	ndisasm -b 64 tmp.bin | $(CUTPATH) -c 11- > $@
	@REM rm -f tmp.nasm tmp.bin

# Test x86 code generation: what Haskell code generates
x86_test_hs.listing: asmgen.exe Makefile
	.\asmgen.exe test_x86_bin > tmp
	ndisasm -b 64 tmp | $(CUTPATH) -c 11- > $@
	@REM rm -f tmp

# Test x86 code generation: compare Haskell to NASM
x86_test: x86_test_nasm.listing x86_test_hs.listing
	diff --left-column -y $^ && echo "PASS" || echo "FAIL"
	@echo Left: NASM binary. Right: Haskell-generated binary.

# A program that takes space-separated hex bytes and converts them to
# a binary format (e.g. "10 AB CD" -> the 3 bytes in binary)
# assemble.exe: asmgen.exe
#	.\asmgen.exe assemble_x86_windows > $@

# assemble.exe.xxd: assemble.exe
#	$(XXD) -g 1 $< $@

# Build the language REPL binary out of the Haskell-generated HEX values
main.exe: asmgen.exe 
	.\asmgen.exe bootstrap_x86_windows_bin > $@
	@REM chmod +x $@

# Run the language REPL under gdb with trace
gdb_main: main gdbTraceRunParams.gdb
	gdb -command=gdbTraceRunParams.gdb $<
	
# REPL language documentation: includes the assembly code and its context.
main.doc: asmgen.exe
	.\asmgen.exe bootstrap_x86_windows_doc > $@

parser_tests.doc: asmgen
	.\asmgen.exe test_par_doc > $@

parser_testsuite: asmgen.exe assemble.exe
	.\asmgen.exe test_par | ./assemble > $@
	chmod +x $@

# Run the internal parser tests. I tried to test the parsers in the language
# itself by exposing internal parse functions and defining test suites in 
# loadable program files; however, loading program files relies on the parser 
# working in the first place, which makes bugs harder to diagnose!
parser_tests: asmgen parser_testsuite
	@echo RUNNING THE PARSER TEST SUITE
	@echo -----
	@./asmgen test_par_stdin | ./parser_testsuite

# Run REPL scripts to test some programs and language constructs
functional_test: main
	@echo No warnings or errors means PASS.
	@echo -----
	@# echo def a = 1 . run a run dbg_dump_ptop_64 q | ./main
	@# echo def a = plus . q | ./main
	@# echo def a = 1 1 plus . run a run dbg_dump_ptop_64 q | ./main
	@cat test_programs/if0_1.program | ./main
	@cat test_programs/if0_2.program | ./main
	@cat test_programs/factorial.program | ./main

# Run REPL scripts to test the parser primitives
# parser_test: main
#	@echo 0 means PASS, anything else is FAIL.
#	@echo -----
#	./main < test_programs/test_suite_parse_identifier.program 
#	./main < test_programs/test_suite_parse_numeric.program 
#	./main < test_programs/test_suite_parse_stack_param.program 

# Generate the GDB trace of a program run.
%.gdb_trace: %.program main
	(printf "set confirm off \n\
		set logging on \n\
		set logging file $@ \n\
		set logging overwrite on \n\
		set pagination off \n\
		set disassembly-flavor intel \n\
		set disassemble-next-line on \n\
		starti < $< \n\
		while 1 \n\
		stepi \n\
		end" | gdb ./main) > $@


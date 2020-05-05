
asmgen: ELFHeader.hs Main.hs \
 ASM/ASM.hs ASM/Datatypes.hs ASM/Pretty.hs \
 Lang/Datatypes.hs Lang/Lang.hs Lang/Linux.hs Lang/Types.hs \
 X86/Datatypes.hs X86/X86.hs X86/Tests.hs
	ghc Main.hs -o $@

x86_test_nasm.listing: asmgen Makefile
	echo BITS 64 > tmp.nasm
	./asmgen test_x86_n >> tmp.nasm
	nasm -O0 -fbin tmp.nasm -o tmp.bin
	# Remove first 10 chars to remove useless info
	ndisasm -b 64 tmp.bin | cut -c 11- > $@
	rm -f tmp.nasm tmp.bin

x86_test_hs.listing: asmgen Makefile
	./asmgen test_x86 | ./assemble > tmp
	ndisasm -b 64 tmp | cut -c 11- > $@
	rm -f tmp

x86_test: x86_test_nasm.listing x86_test_hs.listing
	diff --left-column -y $^ && echo "PASS" || echo "FAIL"
	# Left: NASM binary. Right: Haskell-generated binary.

main: asmgen assemble
	./asmgen | ./assemble > $@
	chmod +x $@

gdb_runtrace_main: main
	gdb -command=gdbTraceRunParams.gdb $<
	
main.doc: asmgen
	./asmgen doc > $@

functional_test: main
	echo def A = 1 . run A run DBG_DUMP_PTOP_64 | ./main
	echo def A = PLUS . | ./main
	echo def A = 1 1 PLUS . run A run DBG_DUMP_PTOP_64 | ./main
	cat if0_1.program | ./main
	cat if0_2.program | ./main
	cat factorial.program | ./main

%.gdb_trace: %.program
	printf "set confirm off \n\
		set logging on \n\
		set logging file $@ \n\
		set logging overwrite on \n\
		set pagination off \n\
		set disassembly-flavor intel \n\
		set disassemble-next-line on \n\
		starti < $< \n\
		while 1 \n\
		stepi \n\
		end" # | gdb ./main


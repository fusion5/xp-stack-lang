
asmgen: ELFHeader.hs Main.hs \
 ASM/ASM.hs ASM/Datatypes.hs ASM/Pretty.hs \
 Lang/Datatypes.hs Lang/Lang.hs Lang/Linux.hs Lang/Types.hs \
 X86/Datatypes.hs X86/X86.hs
	ghc Main.hs -o $@


main: asmgen assemble
	./asmgen | ./assemble > $@
	chmod +x $@

gdb_runtrace_main: main
	gdb -command=gdbTraceRunParams.gdb $<
	

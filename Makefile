SRCFILES = Main.hs ASM\ASM.hs ASM\Datatypes.hs ASM\Pretty.hs \
		   Lang\BasicFunctions.hs Lang\Datatypes.hs Lang\Debug.hs Lang\Lang.hs \
		   Lang\Linux.hs Lang\Windows.hs \
		   X64\Datatypes.hs X64\PE64.hs X64\TestPlan.hs X64\X64.hs
		

main.exe: $(SRCFILES)
	runghc Main.hs

tmp_nasm_x86.nasm: $(SRCFILES)
	echo BITS 64 > $@
	runghc Main.hs x86_testplan_nasm >> $@

tmp_nasm_x86.bin: tmp_nasm_x86.nasm
	nasm -O0 -fbin $< -o $@

tmp_nasm_x86.listing: tmp_nasm_x86.bin
	ndisasm -b 64 $< > $@

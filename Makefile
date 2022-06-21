
main.exe: 
	stack run

tmp_nasm_x86.nasm:
	echo BITS 64 > $@
	stack run x86_testplan_nasm >> $@

tmp_nasm_x86.bin: tmp_nasm_x86.nasm
	nasm -O0 -fbin $< -o $@

tmp_nasm_x86.listing: tmp_nasm_x86.bin
	ndisasm -b 64 $< > $@

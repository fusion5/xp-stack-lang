module Lang.Lang where

import X64.Datatypes
import X64.X64
import ASM.Datatypes
import Lang.BasicFunctions
import Lang.Debug
import Lang.Datatypes
import Lang.Linux
import Lang.Windows

-- The parameter stack is where functions read & write parameters
pstackSizeBytes   = 0x10000
dictBodySizeBytes = 0x10000

def :: String -> X64 ()
def defName = do
    doc $ "ASM Dictionary entry for " ++ defName
    doc "ASM or term sequence address in memory:"
    let hash = I64 $ fnv1 $ map ascii defName
    doc "Backup the old rDictIdx"
    mov rax rDictIdx

    doc "Allocate a new record"
    add rDictIdx $ I32 24
    
    doc "Set values of the new record"
    mov (derefOffset rDictIdx 0) rax -- prev dict. entry
    mov rax hash
    mov (derefOffset rDictIdx 8) rax -- hash
    mov rax (L64 defName)
    mov (derefOffset rDictIdx 16) rax -- code address (by label ref.)

populateDictionaryKernel = do
    doc "Populate the dictionary with some base functions from the assembly."
    doc "The dictionary is kept as a linked list rather than an array because "
    doc "there might be variable-length records in the future (e.g. the "
    doc "entry name ASCII representation)."
    doc "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    doc "See also: setup"
    doc "The functions exported here can be called in the repl."
    doc "The exposed interface should allows the "
    doc "definition of new parsers (so language extension) and the emission "
    doc "of jit-code."

    def "emit_w8"
    def "emit_w64"
    def "read_w8"
    def "write_w8"

    {-
    def "repl"
    def "dbg_dump_dictionary"
    def "dbg_dump_ptop_w8"
    def "dbg_dump_ptop_w64"

    def "new_def"
    def "emit_w8"
    def "emit_w64"
    def "emit_ppush_w8"  -- Convience dictionary export 
    def "emit_ppush_w64" -- Convience dictionary export

    -- def "read_head_w8"
    -- def "parse_identifier"
    -- def "parse_numeric"
    -- def "parse_if_term"
    def "not"
    def "dec"
    def "drop_w64" -- Not very memory safe but for experiments...
    def "drop_w8"
    -- def "ppeer_w8"
    -- def "ppeer_w64"
    def "dup_w64"
    def "dup_w8"
    def "eq_w64"
    def "eq_w8"
    def "lt_w64"
    def "lt_w8"
    def "gt_w64"
    def "gt_w8"
    def "lte_w64"
    def "lte_w8"
    def "gte_w64"
    def "gte_w8"
    def "minus"
    def "times"
    def "and"
    def "exit"
    def "plus"
    def "push1"
    def "write_char_w8"
    def "write_char_w64"

    -- def "emit_lit_64"
    def "term_hash"
    -- def "read_char_w8"

    -}

baseDefBodies platform = do
    {-
    defineCMPW64 "eq_w64"  je
    defineCMPW64 "lt_w64"  jl
    defineCMPW64 "gt_w64"  jg
    defineCMPW64 "lte_w64" jle
    defineCMPW64 "gte_w64" jge

    defineCMPW8 "eq_w8"  je
    defineCMPW8 "lt_w8"  jl
    defineCMPW8 "gt_w8"  jg
    defineCMPW8 "lte_w8" jle
    defineCMPW8 "gte_w8" jge

    defineBinop "plus"  add
    defineBinop "minus" sub
    defineBinop "times" undefined
    defineBinop "and"   and_

    defineShiftLeft
    defineShiftRight

    -- defineRdChrW8Linux
    defineWrCharW8Linux
    defineWrChrW64Linux

    Lang.Parsing.defineParsers
    -}
    defineEmitFunctions
    defineIOFunctions platform
    
    {-
    defineNewDef
    
    defineREPL
    defineTermLook
    defineTermHash
    defineTermLookNohash

    definePush1
    definePushK

    defineDropW64
    defineDropW8

    defineDUPW64
    defineDUPW8

    defineNOT
    defineDEC
    defineExit
    -}

    {-
    Lang.Debug.defineDbgDumpDictionary
    -}
    Lang.Debug.defineDbgDumpPtop64
    Lang.Debug.defineDbgDumpPtop8


defineEmitFunctions = do
    defineEmitW8
    defineEmitW64

defineWrW8Windows = do
    defFunBasic "write_w8" body
  where
    body = do
    
        comment "Writes on screen a single character from the pstack."
        comment "Consumes the pstack w8."
        comment "Pushes a w64 indicating success or failure."

        comment "Uses the Windows function:"
        comment "WriteFile (HANDLE hFile, LPCVOID lpBuffer, "
        comment "  DWORD numBytesToWrite, LPDWORD lpNumBytesWritten, "
        comment "  LPOVERLAPPED lpOverlapped)"

        comment "Note: Windows functions require 16-bit call stack alignment. "
        comment "This means that all "
        comment "other functions in the system should conform to "
        comment "this alignment requirement..."

        comment "Set rax to the function pointer"
        mov rax (L64 "WriteFile")
        mov rax (deref rax)

        comment "https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention"

        comment "Writing to stdout, place the HANDLE into rcx"
        mov rcx (L64 "stdout")
        mov rcx (deref rcx)

        comment "Write to stdout from buffer: the parameter-stack top"
        mov rdx rPstack

        comment "How many bytes to write: just 1"
        mov r8 (I64 1)

        comment "A pointer to write the number of bytes written"
        mov r9 (L64 "io_result")
        
        comment "Windows functions require 16-bit alignment "
        comment "on the call stack."
        ppushAlignCallStack16bytes

        comment "We use NULL (it's not an overlapped operation)"
        mov rbx (I64 0)

        comment "The fifth function parameter is passed through the stack"
        comment "(cpush advances the stack by 8 bytes)"
        cpush rbx -- Sub rsp 8

        comment "The MS x64 calling convention requires 32 bytes of shadow"
        comment "space to spill registers rcx, rdx, r8 and r9:"
        cpush_x64_32ShadowBytes

        comment "Does this alter rsi? (Yes, it seems that writeFile does)"
        call rax

        comment "Restore the call stack from the saved value on the parameter "
        comment "stack (it can become unaligned). We return to the call-stack state"
        comment "at the ppushAlignCallStack16bytes point."
        ppopRestoreCallStack

        -- comment "Drop the fifth function parameter (8 bytes) that was pushed on the cstack"
        -- cdrop 8

        comment "Drop the top character from the stack - we've just written it."
        pdropW8 1

        comment "Return the result. If the function succeeds, the return value "
        comment "is nonzero."

        comment "TODO: When writing to a non-blocking, byte-mode pipe handle with "
        comment "insufficient buffer space, WriteFile returns TRUE with "
        comment "*lpNumberOfBytesWritten < nNumberOfBytesToWrite"

        comment "Push the WriteFile result, 0 for failure and 1 for success"

        test  rax rax
        xor   rax rax
        setz  al
        ppush rax



defineWrW8Linux :: X64 ()
defineWrW8Linux = defFunBasic "write_w8" body
  where 
    body = do
        mov rdx $ I64 1 -- How many bytes to write?
        mov rax $ I64 $ fromIntegral linux_sys_write
        mov rbx $ I64 $ fromIntegral linux_stdout 
        -- read chars from the top of the params stack, i.e. from our buffer.
        mov rcx rPstack
        int
        pdropW8 1 -- Drop the top of the stack which was the char
                  -- we've just printed.
        ppush rax -- The putchar result, how many bytes we've written

defineRdW8Linux   = undefined
defineRdW8Windows = do
    defFunBasic "read_w8" body
  where
    body = do

        comment "Allocate a new char on the p-stack, the ReadFile output buffer"
        comment "TODO: in the future this should be a buffer > 1 char, maybe"
        comment "like a global variable allocated in .data"
        ppush (I8 0)

        comment "Read a single character from stdin and place it on the pstack."
        comment "Uses the Windows kernel function: "
        comment "BOOL ReadFile(HANDLE hFile, LPVOID lpBuffer, "
        comment "   DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, "
        comment "   LPOVERLAPPED lpOverlapped)"

        comment "Set rax to the function pointer"
        mov rax (L64 "ReadFile")
        mov rax (deref rax)

        comment "HANDLE hFile: reading from stdin, place the HANDLE into rcx"
        mov rcx (L64 "stdin")
        mov rcx (deref rcx)

        comment "Read from stdin into buffer: the char allocated at the top, i.e. "
        comment "the top of the stack"
        mov rdx rPstack

        comment "Number of bytes to read: just 1"
        mov r8 (I64 1)

        comment "A pointer to write the number of bytes read"
        mov r9 (L64 "io_result")


        comment "The order here may seem unnatural/random but this is needed"
        comment "because there are dependencies between registers!"

        comment "The fifth function parameter is passed through the stack"
        comment "(cpush advances the stack by 8 bytes)"
        mov rbx (I64 0)
        cpush rbx

        comment "Windows functions require 16-bit alignment "
        comment "on the call stack."
        ppushAlignCallStack16bytes

        comment "https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention"
        comment "The MS x64 calling convention requires 32 bytes of shadow"
        comment "space to spill registers rcx, rdx, r8 and r9:"
        cpush_x64_32ShadowBytes

        call rax

        comment "Restore the call stack from the saved value on the parameter "
        comment "stack (it can become unaligned). We return to the call-stack state"
        comment "at the ppushAlignCallStack16bytes point."
        ppopRestoreCallStack

        comment "Drop the fifth function parameter (8 bytes) that was pushed on the cstack"
        cdrop 8

        comment "Return a success code. "
        comment "Push the ReadFile result, 0 for failure and 1 for success"

        test  rax rax
        xor   rax rax
        setz  al
        ppush rax


defineIOFunctions Linux   = 
    defineRdW8Linux
    defineWrW8Linux
defineIOFunctions Windows = do
    defineRdW8Windows 
    defineWrW8Windows 
            
defineEmitW8 :: X64 ()
defineEmitW8 = defFunBasic "emit_w8" body 
  where
    body = do
        comment "Takes the w8 value from the top of the stack and emits it"
        comment "in the JIT code generation area. Uses RAX"
        -- cpush rax
        -- xor rax rax

        -- writeMsgHelper ":: emit "
        -- callLabel "dbg_dump_ptop_w8"

        ppop al
        mov (derefOffset rDefBodies 0) al
        inc rDefBodies
        -- cpop rax
 
defineEmitW64 :: X64 ()
defineEmitW64 = defFunBasic "emit_w64" body
  where
    body = do
        -- writeMsgHelper ":: emit "
        -- callLabel "dbg_dump_ptop_w64"
        ppop rax
        mov (derefOffset rDefBodies 0) rax
        add rDefBodies (I32 8)


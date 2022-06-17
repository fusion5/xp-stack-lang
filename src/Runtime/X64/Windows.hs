module Runtime.X64.Windows where

import qualified ASM.Types              as ASM
import qualified Runtime.X64.Debug      as Debug
import qualified Runtime.X64.BasicOps   as X64.Ops
import qualified X64.Types              as X64
import qualified X64.X64                as X64
import qualified Data.Word              as Word

comment :: String -> X64.X64 ()
comment = ASM.comment

windows_stdout_handle, windows_stdin_handle :: Word.Word64
windows_stdout_handle = 0xFFFFFFFFFFFFFFF5 -- -11
windows_stdin_handle  = 0xFFFFFFFFFFFFFFF6 -- -10

cpush_x64_32ShadowBytes :: X64.X64 ()
cpush_x64_32ShadowBytes = do
    X64.sub X64.Ops.rCallStack (X64.I32 0x20)

cpop_x64_32ShadowBytes :: X64.X64 ()
cpop_x64_32ShadowBytes = do
    X64.add X64.Ops.rCallStack (X64.I32 0x20)

ppushAlignCallStack16bytes :: X64.X64 ()
ppushAlignCallStack16bytes = do
    ASM.comment "Backup the current call stack pointer to the p-stack"
    X64.Ops.ppush X64.Ops.rCallStack

    -- Perform the division by 16 bytes, i.e. 16*8 = 128 = 0x80
    ASM.comment "This subtracts from the stack pointer until it's divisible by 128."
    ASM.comment "The result is a 16-byte aligned rPstack."
    ASM.comment "AND(rPstack, imm32 sign-extended to 64-bits):"
    -- and_ rCallStack (I32 0xFFFFFF7F)
    X64.and_ X64.Ops.rCallStack (X64.I32 0xFFFFFF80)

setup :: X64.X64 ()
setup = do
    ASM.comment "Ensure a 16-bit call stack alignment:"
    ASM.comment "The stack needs to be 16-byte aligned before each call."
    ASM.comment "In function bodies the stack begins as non-"
    ASM.comment "aligned, having the 8-byte instruction pointer on top."
    ASM.comment "Therefore, if the function calls other functions, it must"
    ASM.comment "align the stack."
    -- sub rsp (I32 0x08)

    ASM.comment "The PE64 header contains labels to the start and end of the "
    ASM.comment "memory space that the Windows loader reserves for the process "
    ASM.comment "for the purpose of storing parameter stack data and the "
    ASM.comment "dictionary definitions."
    ASM.comment "1. The parameter stack, going from large to small addresses as"
    ASM.comment "it grows:"
    X64.mov X64.Ops.rPstack (X64.L64 "end_stack")

    ASM.comment "2. The dictionary entries: "
    ASM.comment "Initialise the dictionary pointer. The dictionary is a linked list"
    ASM.comment "of entries having fields [prev][hash][address], where:"
    ASM.comment "- prev is a pointer to the previous dictionary element."
    ASM.comment "- hash is the term hash."
    ASM.comment "- address is a pointer to the JIT definitions memory zone, the "
    ASM.comment "  definition contents."
    ASM.comment "Each of these fields is 8 bytes long."
    X64.mov X64.Ops.rDictIdx (X64.L64 "begin_stack")
    ASM.comment "Add a first, dummy, empty term to mark the end of the list."
    ASM.comment "The dictionary grows from small to large addresses:"
    X64.mov (X64.derefOffset X64.Ops.rDictIdx 0)  (X64.I32 0)
    X64.mov (X64.derefOffset X64.Ops.rDictIdx 8)  (X64.I32 0)
    X64.mov (X64.derefOffset X64.Ops.rDictIdx 16) (X64.I32 0)
    ASM.comment "We don't need to add the size of a new record here, because"
    ASM.comment "the add is done by the insert operation"

    ASM.comment "3. The dictionary bodies grow from small addresses to large "
    ASM.comment "addresses. They are situated in the .text segment just after "
    ASM.comment "the program code ends"
    X64.mov X64.Ops.rDefBodies (X64.L64 "begin_runtime_generated_text")

    ASM.comment "4. Initialise the stdin and stdout handlers"
    ASM.comment "HANDLE GetStdHandle (DWORD nStdHandle)"
    ASM.comment "STD_INPUT_HANDLE  = -10"
    ASM.comment "STD_OUTPUT_HANDLE = -11"
    ASM.comment "STD_ERROR_HANDLE  = -12"
    ASM.comment "Aspects of the Windows x64 calling convention that are used in"
    ASM.comment "this program:"
    ASM.comment "* Parameter passing - integer params: registers rcx, rdx, r8, r9"
    ASM.comment "  The rest are passed on the stack"
    ASM.comment "* Return values - in rax if it fits 64 bits"

    ASM.comment "Another GetStdHandle call: get stdout"
    X64.mov X64.rax (X64.L64 "GetStdHandle")
    X64.mov X64.rax (X64.deref X64.rax)
    ASM.comment "Add call stack shadow space (win x64 calling convention):"
    X64.sub X64.rsp (X64.I32 0x20)

    ASM.comment "The first function parameter"
    X64.mov X64.rcx (X64.I64 windows_stdout_handle)
    X64.call X64.rax

    X64.mov X64.rbx (X64.L64 "stdout")
    X64.mov (X64.deref X64.rbx) X64.rax

    ASM.comment "Another GetStdHandle call: get stdin"

    X64.mov X64.rax (X64.L64 "GetStdHandle")
    X64.mov X64.rax (X64.deref X64.rax)

    X64.mov X64.rcx (X64.I64 windows_stdin_handle)
    X64.call X64.rax

    X64.mov X64.rbx (X64.L64 "stdin")
    X64.mov (X64.deref X64.rbx) X64.rax

    ASM.comment "Remove call stack shadow space"
    X64.add X64.rsp (X64.I32 0x20)


mainBody :: X64.X64 ()
mainBody = do
    -- X64.X64.xor rax rax
    setup
    X64.Ops.populateDictionaryKernel

    {-
    ppush (I8 0x32)
    callLabel "write_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    ppush (I8 0x33)
    callLabel "write_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    ppush (I8 0x34)
    callLabel "write_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1
    -}

    X64.Ops.ppush (X64.I8 0x31)
    X64.callLabel "write_w8"
    X64.Ops.ppush (X64.I8 0x32)
    X64.callLabel "write_w8"
    X64.Ops.ppush (X64.I8 0x0D)
    X64.callLabel "write_w8"
    X64.Ops.ppush (X64.I8 0x0A)
    X64.callLabel "write_w8"
    X64.callLabel "dbg_dump_ptop_w64"
    X64.Ops.pdropW64 1
    X64.callLabel "dbg_dump_ptop_w64"
    X64.Ops.pdropW64 1
    X64.callLabel "dbg_dump_ptop_w64"

    {- cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    cpush (I8 0x00) -- Alignment modifier
    -}
    -- cdrop 8 -- Alignment modifier

    {-
    X64.callLabel "write_w8"
    X64.callLabel "dbg_dump_ptop_w64"

    X64.Ops.ppush (X64.I32 0x34)
    X64.callLabel "dbg_dump_ptop_w64"
    X64.callLabel "read_w8"
    X64.callLabel "dbg_dump_ptop_w64"
    X64.Ops.pdropW64 1
    X64.callLabel "write_w8"
    -}

    -- ppush (I8 0x34)
    -- callLabel "write_w8"
    -- ppush (I8 0x34)
    -- callLabel "write_w8"
    -- ppush (I32 0x01)
    -- callLabel "dbg_dump_ptop_w64"
    -- pdrop 1
    -- ppush (I8 0x33)
    -- callLabel "dbg_dump_ptop_w8"

    -- callLabel "dbg_dump_ptop_w8"

    -- callLabel "write_w8"
    -- callLabel "dbg_dump_ptop_w64"
    -- pdrop 1

    -- pdrop 1

    -- pdrop 1

    --ppush (I8 0x11)
    -- callLabel "dbg_dump_ptop_w8"

    -- callLabel "dbg_dump_ptop_w64"
    -- pdrop 1

    {-
    ppush (I8 0x31)
    callLabel "write_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    ppush (I8 0x30)
    callLabel "write_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    callLabel "read_w8"
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    callLabel "dbg_dump_ptop_w8"
    -}

    -- int3
    -- TODO: Find mov/call opcodes that to do this more neatly?
    X64.mov X64.rax (X64.L64 "ExitProcess")
    X64.mov X64.rax (X64.deref X64.rax)

    ASM.comment "Shadow space (win x64 calling convention):"
    X64.sub X64.rsp (X64.I32 0x20)

    ASM.comment "The first function parameter, the exit code."
    X64.mov  X64.rcx (X64.I64 1234)
    X64.call X64.rax
    X64.ret

    ASM.comment "Kernel function bodies (they should come after the main code so"
    ASM.comment "that they aren't executed by the main entry point)"

    Debug.defineDbgDumpPtopW8
    Debug.defineDbgDumpPtopW64
    X64.Ops.defineEmitW8
    X64.Ops.defineEmitW64
    defineRdW8Windows
    defineWrW8Windows


defineRdW8Windows :: X64.X64 ()
defineRdW8Windows = X64.Ops.defFunBasic "read_w8" $ do
  ASM.comment "Allocate a new char on the p-stack, the ReadFile output buffer"
  ASM.comment "TODO: in the future this should be a buffer > 1 char, maybe"
  ASM.comment "like a global variable allocated in .data"
  X64.Ops.ppush (X64.I8 0)

  ASM.comment "Read a single character from stdin and place it on the pstack."
  ASM.comment "Uses the Windows kernel function: "
  ASM.comment "BOOL ReadFile(HANDLE hFile, LPVOID lpBuffer, "
  ASM.comment "   DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, "
  ASM.comment "   LPOVERLAPPED lpOverlapped)"

  ASM.comment "Set rax to the function pointer"
  X64.mov X64.rax (X64.L64 "ReadFile")
  X64.mov X64.rax (X64.deref X64.rax)

  ASM.comment "HANDLE hFile: reading from stdin, place the HANDLE into rcx"
  X64.mov X64.rcx (X64.L64 "stdin")
  X64.mov X64.rcx (X64.deref X64.rcx)

  ASM.comment "Read from stdin into buffer: the char allocated at the top, i.e. "
  ASM.comment "the top of the stack"
  X64.mov X64.rdx X64.Ops.rPstack

  ASM.comment "Number of bytes to read: just 1"
  X64.mov X64.r8 (X64.I64 1)

  ASM.comment "A pointer to write the number of bytes read"
  X64.mov X64.r9 (X64.L64 "io_result")

  ASM.comment "The order here may seem unnatural/random but this is needed"
  ASM.comment "because there are dependencies between registers!"

  ASM.comment "The fifth function parameter is passed through the stack"
  ASM.comment "(cpush advances the stack by 8 bytes)"
  X64.mov X64.rbx (X64.I64 0)
  X64.Ops.cpush X64.rbx

  ASM.comment "Windows functions require 16-bit alignment "
  ASM.comment "on the call stack."
  ppushAlignCallStack16bytes

  ASM.comment "https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention"
  ASM.comment "The MS x64 calling convention requires 32 bytes of shadow"
  ASM.comment "space to spill registers rcx, rdx, r8 and r9:"
  cpush_x64_32ShadowBytes

  X64.call X64.rax

  ASM.comment "Restore the call stack from the saved value on the parameter "
  ASM.comment "stack (it can become unaligned). We return to the call-stack state"
  ASM.comment "at the ppushAlignCallStack16bytes point."
  X64.Ops.ppopRestoreCallStack

  ASM.comment "Drop the fifth function parameter (8 bytes) that was pushed on the cstack"
  X64.Ops.cdrop 8

  ASM.comment "Return a success code. "
  ASM.comment "Push the ReadFile result, 0 for failure and 1 for success"

  X64.test  X64.rax X64.rax
  X64.xor   X64.rax X64.rax
  X64.setz  X64.al
  X64.Ops.ppush X64.rax


defineWrW8Windows :: X64.X64 ()
defineWrW8Windows = X64.Ops.defFunBasic "write_w8" $ do
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
  X64.mov X64.rax (X64.L64 "WriteFile")
  X64.mov X64.rax (X64.deref X64.rax)

  comment "https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention"

  comment "Writing to stdout, place the HANDLE into rcx"
  X64.mov X64.rcx (X64.L64 "stdout")
  X64.mov X64.rcx (X64.deref X64.rcx)

  comment "Write to stdout from buffer: the parameter-stack top"
  X64.mov X64.rdx X64.Ops.rPstack

  comment "How many bytes to write: just 1"
  X64.mov X64.r8 (X64.I64 1)

  comment "A pointer to write the number of bytes written"
  X64.mov X64.r9 (X64.L64 "io_result")

  comment "Windows functions require 16-bit alignment "
  comment "on the call stack."
  ppushAlignCallStack16bytes

  comment "We use NULL (it's not an overlapped operation)"
  X64.mov X64.rbx (X64.I64 0)

  comment "The fifth function parameter is passed through the stack"
  comment "(cpush advances the stack by 8 bytes)"
  X64.Ops.cpush X64.rbx -- Sub rsp 8

  comment "The MS x64 calling convention requires 32 bytes of shadow"
  comment "space to spill registers rcx, rdx, r8 and r9:"
  cpush_x64_32ShadowBytes

  comment "Does this alter rsi? (Yes, it seems that writeFile does)"
  X64.call X64.rax

  comment "Restore the call stack from the saved value on the parameter "
  comment "stack (it can become unaligned). We return to the call-stack state"
  comment "at the ppushAlignCallStack16bytes point."
  X64.Ops.ppopRestoreCallStack

  -- comment "Drop the fifth function parameter (8 bytes) that was pushed on the cstack"
  -- cdrop 8

  comment "Drop the top character from the stack - we've just written it."
  X64.Ops.pdropW8 1

  comment "Return the result. If the function succeeds, the return value "
  comment "is nonzero."

  comment "TODO: When writing to a non-blocking, byte-mode pipe handle with "
  comment "insufficient buffer space, WriteFile returns TRUE with "
  comment "*lpNumberOfBytesWritten < nNumberOfBytesToWrite"

  comment "Push the WriteFile result, 0 for failure and 1 for success"

  X64.test  X64.rax X64.rax
  X64.xor   X64.rax X64.rax
  X64.setz  X64.al
  X64.Ops.ppush X64.rax


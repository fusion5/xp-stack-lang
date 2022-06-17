module Runtime.X64.Windows.PE64 where

import qualified ASM.ASM              as ASM
import qualified ASM.Types            as ASM
-- import qualified Data.Binary.Put      as Put
-- import qualified Data.ByteString.Lazy as BS
import qualified Data.Word            as Word
import qualified X64.Types            as X64
import qualified X64.X64              as X64

comment :: String -> X64.X64 ()
comment = ASM.comment

label :: String -> X64.X64 ()
label = ASM.label

-- emitString :: (Bounded t_addr, Integral t_addr) => String -> ASM.ASM t_addr ()
-- emitString = ASM.stringASM

emit :: [Word.Word8] -> X64.X64 ()
emit = X64.liftX64 . ASM.bytesASM

emitImageLabelDiff32 :: String -> String -> X64.X64 ()
emitImageLabelDiff32 l1 l2 = X64.emitBytes $ X64.imageLabelDiff32 l1 l2

emitLabelDiff32 :: String -> String -> X64.X64 ()
emitLabelDiff32 l1 l2 = X64.emitBytes $ X64.labelDiff32 l1 l2

section_alignment, file_alignment :: Word.Word32
section_alignment = 4096
file_alignment    = 512

withDOSHeader :: X64.X64 () -> X64.X64 ()
withDOSHeader act = do
    label "begin_image_dos_header" -- typedef struct _IMAGE_DOS_HEADER
    dh_e_Constant
    dh_e_cblp
    dh_e_cp
    dh_e_crlc
    dh_e_cparhdr
    dh_e_minalloc
    dh_e_maxalloc
    dh_e_ss
    dh_e_sp
    dh_e_csum
    dh_e_ip
    dh_e_cs
    dh_e_lfarlc
    dh_e_ovno
    dh_e_res -- [4]
    dh_e_oemid
    dh_e_oeminfo
    dh_e_res2 -- [10]
    dh_e_lfanew
    label "end_image_dos_header"
    act
  where
    dh_e_Constant = emit [0x4D, 0x5A] -- Constant number (MZ)
    dh_e_cblp     = emit [0x00, 0x00] -- Bytes on last page of file
    dh_e_cp       = emit [0x00, 0x00] -- Pages in file
    dh_e_crlc     = emit [0x00, 0x00] -- Relocations
    dh_e_cparhdr  = emit [0x00, 0x00] -- Size of headers in paragraphs
    dh_e_minalloc = emit [0x00, 0x00] -- Minimum extra paragraphs needed
    dh_e_maxalloc = emit [0x00, 0x00] -- Maximum extra paragraphs needed
    dh_e_ss       = emit [0x00, 0x00] -- Initial SS value
    dh_e_sp       = emit [0x00, 0x00] -- Initial SP value
    dh_e_csum     = emit [0x00, 0x00] -- Checksum
    dh_e_ip       = emit [0x00, 0x00] -- Initial IP value
    dh_e_cs       = emit [0x00, 0x00] -- Initial (relative) CS value
    dh_e_lfarlc   = emit [0x00, 0x00] -- File address of relocation table
    dh_e_ovno     = emit [0x00, 0x00] -- Overlay number
    dh_e_res      = mapM_ (\_ -> emit [0x00, 0x00]) [0..3 :: Integer] -- [4]
    dh_e_oemid    = emit [0x00, 0x00] -- OEM identifier
    dh_e_oeminfo  = emit [0x00, 0x00] -- OEM information
    dh_e_res2     = mapM_ (\_ -> emit [0x00, 0x00]) [0..9 :: Integer] -- [10]
    dh_e_lfanew   = emitLabelDiff32 "begin_pe64_image" "begin_coff_header"

withCOFFHeader :: X64.X64 () -> X64.X64 ()
withCOFFHeader act = do
  label "begin_coff_header"
  e_Constant
  e_Machine
  e_NumberOfSections
  e_TimeDateStamp
  e_PointerToSymbolTable
  e_NumberOfSymbols
  e_SizeOfOptionalHeader
  e_Characteristics
  label "end_coff_header"
  act
  where
    e_Constant = emit [0x50, 0x45, 0x00, 0x00] -- 'PE\0\0'
    e_Machine  = emit [0x64, 0x86] -- IMAGE_FILE_MACHINE_AMD64 x64
    e_NumberOfSections = emit [0x04, 0x00] -- .text .idata .bss and paramter-stack
    e_TimeDateStamp = emit [0x00, 0x00, 0x00, 0x00] -- Could be the compile time unixtime (C time_t value)
    e_PointerToSymbolTable = emit [0x00, 0x00, 0x00, 0x00] -- The file offset of the COFF symbol table, 0
                                                           -- if no COFF table is present
    e_NumberOfSymbols = emit [0x00, 0x00, 0x00, 0x00] -- The number of COFF symbols in the table
    e_SizeOfOptionalHeader =
      X64.emitBytes $ X64.labelDiff16 "begin_optional_header" "end_optional_header"
      -- The size of the optional header, which is required for
      -- executable files but not for object files.

    e_Characteristics = emit [0x27, 0x02] -- relocations stripped, executable, line numbers stripped,
                   -- large address aware, debugging information removed

withOptionalHeader :: Word.Word64 -> X64.X64 () -> X64.X64 ()
withOptionalHeader image_base act = do
  -- Optional hader, COFF Standard
  label "begin_optional_header"
  e_OptHdr_Constant
  e_OptHdr_MajorLinkerVersion
  e_OptHdr_MinorLinkerVersion
  e_OptHdr_SizeOfCode
  e_OptHdr_SizeOfInitializedData
  e_OptHdr_SizeOfUninitializedData
  e_OptHdr_AddressOfEntryPoint
  e_OptHdr_BaseOfCode

  -- Optional header, NT Additional Fields
  e_OptHdrNT_ImageBase
  e_OptHdrNT_SectionAlignment
  e_OptHdrNT_FileAlignment
  e_OptHdrNT_MajorOSVersion
  e_OptHdrNT_MinorOSVersion
  e_OptHdrNT_MajorImageVersion
  e_OptHdrNT_MinorImageVersion
  e_OptHdrNT_MajorSubsystemVersion
  e_OptHdrNT_MinorSubsystemVersion
  e_OptHdrNT_Reserved1
  e_OptHdrNT_SizeOfImage
  e_OptHdrNT_SizeOfHeaders
  e_OptHdrNT_CheckSum
  e_OptHdrNT_Subsystem
  e_OptHdrNT_DllCharacteristics
  e_OptHdrNT_SizeOfStackReserve
  e_OptHdrNT_SizeOfStackCommit
  e_OptHdrNT_SizeOfHeapReserve
  e_OptHdrNT_SizeOfHeapCommit
  e_OptHdrNT_LoaderFlags
  e_OptHdrNT_NumberOfRvaAndSizes

  -- Export table
  e_OptHdrNT_DirectoryExportTable
  -- Import table
  e_OptHdrNT_DirectoryImportTable
  e_OptHdrNT_14DirectoryList
  label "end_optional_header"
  act
  where
    e_OptHdr_Constant = do
        comment "PE32+:"
        emit [0x0B, 0x02]
    e_OptHdr_MajorLinkerVersion = emit [0x00]
    e_OptHdr_MinorLinkerVersion = emit [0x00]

    -- "The size of the code (.text) section, or the sum of all code sections
    --  if there are multiple sections:"
    e_OptHdr_SizeOfCode = emitLabelDiff32 "begin_text" "end_text"

    e_OptHdr_SizeOfInitializedData   = emit [0x00, 0x00, 0x00, 0x00] -- labelDiff32 "begin_idata" "end_idata"
    e_OptHdr_SizeOfUninitializedData = emit [0x00, 0x00, 0x00, 0x00]

    -- "The address of the entry point relative to the image base when the
    -- executable file is loaded into memory. For program images, this is the
    -- starting address"
    e_OptHdr_AddressOfEntryPoint =
        emitLabelDiff32 "begin_pe64_image" "begin_text"

    -- "The address that is relative to the image base of the
    -- beginning-of-code section when it is loaded into memory"
    e_OptHdr_BaseOfCode = emit [0x00, 0x00, 0x00, 0x00]
        -- emitBytes $ labelDiff32 "begin_pe64_image" "begin_text"
    e_OptHdrNT_ImageBase = -- emit [0x00, 0x00, 0x40, 0x00,  0x00, 0x00, 0x00, 0x00]
        X64.emitBytes $ X64.int64 image_base
    e_OptHdrNT_SectionAlignment = -- emit [0x01, 0x00, 0x00, 0x00]
        X64.emitBytes $ X64.int32 section_alignment
    e_OptHdrNT_FileAlignment    = -- emit [0x01, 0x00, 0x00, 0x00]
        X64.emitBytes $ X64.int32 file_alignment
    e_OptHdrNT_MajorOSVersion   = emit [0x04, 0x00]
    e_OptHdrNT_MinorOSVersion   = emit [0x00, 0x00]
    e_OptHdrNT_MajorImageVersion = emit [0x00, 0x00]
    e_OptHdrNT_MinorImageVersion = emit [0x01, 0x00]
    e_OptHdrNT_MajorSubsystemVersion = emit [0x05, 0x00]
    e_OptHdrNT_MinorSubsystemVersion = emit [0x00, 0x00]
    e_OptHdrNT_Reserved1 = emit [0x00, 0x00, 0x00, 0x00]
    -- "The size (in bytes) of the image, including all headers, as the image
    -- is loaded in memory. It must be a multiple of SectionAlignment"
    e_OptHdrNT_SizeOfImage = emitLabelDiff32 "begin_pe64_image" "end_pe64_image"

    -- "The combined size of an MS-DOS stub, PE header and section headers
    -- rounded up to a multiple of FileAlignment"
    e_OptHdrNT_SizeOfHeaders = emitLabelDiff32 "begin_pe64_image" "section_headers_end"
    e_OptHdrNT_CheckSum  = emit [0x00, 0x00, 0x00, 0x00]
    e_OptHdrNT_Subsystem = emit [0x03, 0x00] -- The Windows Character subsystem
    e_OptHdrNT_DllCharacteristics  = emit [0x00, 0x00]
    e_OptHdrNT_SizeOfStackReserve  = emit [0x00, 0x00, 0x20, 0x00,
                                           0x00, 0x00, 0x00, 0x00]
    e_OptHdrNT_SizeOfStackCommit   = emit [0x00, 0x10, 0x00, 0x00,
                                           0x00, 0x00, 0x00, 0x00]
    e_OptHdrNT_SizeOfHeapReserve   = emit [0x00, 0x00, 0x20, 0x00,
                                           0x00, 0x00, 0x00, 0x00]
    e_OptHdrNT_SizeOfHeapCommit    = emit [0x00, 0x10, 0x00, 0x00,
                                           0x00, 0x00, 0x00, 0x00]
    e_OptHdrNT_LoaderFlags         = emit [0x00, 0x00, 0x00, 0x00]

    e_OptHdrNT_NumberOfRvaAndSizes = do
        comment "The number of data-directory entries in the remainder of the optional"
        comment "header (that follows after this). Each describes a location and size:"
        emit [0x10, 0x00, 0x00, 0x00]

    e_OptHdrNT_DirectoryExportTable = do
        comment "No export table:"
        emit [0x00, 0x00, 0x00, 0x00] -- VirtualAddress
        emit [0x00, 0x00, 0x00, 0x00] -- Size

    e_OptHdrNT_DirectoryImportTable = do
        comment "Import table:"
        emitLabelDiff32 "begin_pe64_image"             "begin_import_directory_table"
        emitLabelDiff32 "begin_import_directory_table" "end_import_directory_table"
    e_OptHdrNT_14DirectoryList = mapM_
        (\_ -> do
            emit [0x00, 0x00, 0x00, 0x00] -- VirtualAddress
            emit [0x00, 0x00, 0x00, 0x00] -- Size
        ) ([0..13 :: Integer])

pe64Header :: Word.Word64
           -> Word.Word64
           -> Word.Word64
           -> X64.X64 () -> X64.X64 ()
pe64Header image_base pstackSizeBytes dictBodySizeBytes e_SectionText = do
    comment "The Portable Executable image is defined to help the bootstrap"
    comment "system to easily start on Windows."
    comment "Thanks to https://keyj.emphy.de/win32-pe/ which clarified lots"
    comment "of things for me!"

    label "begin_pe64_image"
    withDOSHeader $ withCOFFHeader $ withOptionalHeader image_base $ do

      label "section_headers_begin"
      e_SectionHeaderText
      e_SectionHeaderIdata
      e_SectionHeaderStack
      e_SectionHeaderBSS
      label "section_headers_end"

      -- Align the file to file_alignment
      X64.liftX64 $ do
        ASM.alignImage $ fromIntegral file_alignment
        ASM.alignRVA   $ fromIntegral section_alignment

      label "begin_text"
      e_SectionText
      label "begin_runtime_generated_text"
      X64.liftX64 $ ASM.addVirtualBytes dictBodySizeBytes
      label "end_text"

      X64.liftX64 $ do ASM.alignImage $ fromIntegral file_alignment
                       ASM.alignRVA   $ fromIntegral section_alignment
      label "begin_idata"
      e_SectionIData
      label "end_idata"

      X64.liftX64 $ do ASM.alignImage $ fromIntegral file_alignment
                       ASM.alignRVA   $ fromIntegral section_alignment
      label "begin_stack"
      e_SectionStack
      label "end_stack"

      X64.liftX64 $ do ASM.alignImage $ fromIntegral file_alignment
                       ASM.alignRVA   $ fromIntegral section_alignment
      comment "bss is used to hold global in-memory variables"
      label "begin_bss"
      e_SectionBSS
      label "end_bss"

      {-
      runASM $ alignRVA   $ fromIntegral section_alignment
      runASM $ alignImage $ fromIntegral file_alignment
      -}

    label "end_pe64_image"
  where


    e_SectionHeaderText =
        e_SectionHeader ".text" "begin_text" "end_text"
            [0x20, 0x00, 0x00, 0x60] -- Code, readable, executable
    e_SectionHeaderBSS =
        e_SectionHeader ".bss" "begin_bss" "end_bss"
            [0x40, 0x00, 0x00, 0xC0] -- Readable, writable
    e_SectionHeaderIdata =
        e_SectionHeader ".idata" "begin_idata" "end_idata"
            [0x40, 0x00, 0x00, 0xC0] -- Readable, writable
    e_SectionHeaderStack =
        e_SectionHeader "stack" "begin_stack" "end_stack"
            [0x40, 0x00, 0x00, 0xC0]

    e_SectionHeader name lbl_begin lbl_end characteristics = do
        comment "Name"
        X64.emitString $ take 8 $ name ++ repeat '\0'

        comment "VirtualSize"
        emitLabelDiff32 lbl_begin lbl_end

        comment "VirtualAddress (RVA)"
        emitLabelDiff32 "begin_pe64_image" lbl_begin

        comment "SizeOfRawData"
        emitImageLabelDiff32 lbl_begin lbl_end

        comment "PointerToRawData"
        emitImageLabelDiff32 "begin_pe64_image" lbl_begin

        comment "PointerToRelocations"
        emit [0x00, 0x00, 0x00, 0x00]

        comment "PointerToLineNumbers"
        emit [0x00, 0x00, 0x00, 0x00]

        comment "NumberOfRelocations"
        emit [0x00, 0x00]

        comment "NumberOfLineNumbers"
        emit [0x00, 0x00]

        comment "Characteristics"
        emit characteristics -- Readable, writable

    e_SectionBSS = do
        comment "Global in-memory variable space"
        label "stdin"
        comment "stdin keeps a reference to the stdin HANDLE value, from which"
        comment "we read input"
        emit $ take 8 $ repeat 0x00
        label "stdout"
        comment "stdout keeps a reference to the stdin HANDLE value, on which"
        comment "we write data"
        emit $ take 8 $ repeat 0x00
        label "io_result"
        comment "Where the number of bytes of a write operation is stored"
        emit $ take 8 $ repeat 0x00

    e_SectionIData = do
        comment "Import data (.idata) section:"
        comment "This is where the Windows exe loader writes the addresses"
        comment "of requested functions. This mechanism gives the ability"
        comment "to call kernel32.dll functions from within the program."
        comment "This code is typically generated by a linker, but for this "
        comment "program explicit code is used in order to reduce bootstrap "
        comment "dependencies (it saves the user from finding or installing a "
        comment "linker)."
        label "begin_import_directory_table"
        do
            comment "Import of kernel32.dll"
            comment "Import Lookup Table RVA (Read-only)"
            comment "The RVA of the import lookup table. This table contains a name"
            comment "or ordinal for each import. Not used."
            emit [0x00, 0x00, 0x00, 0x00]

            comment "Timestamp:"
            emit [0x00, 0x00, 0x00, 0x00]

            comment "Forwarder chain:"
            emit [0x00, 0x00, 0x00, 0x00]

            comment "Library name (RVA):"
            emitLabelDiff32 "begin_pe64_image" "kernel32_name"

            comment "Import address table (Thunk table). The contents of this "
            comment "table are indentical to the contents of the import lookup"
            comment "table until the image is bound."
            emitLabelDiff32 "begin_pe64_image" "kernel32_IAT"

            comment "Terminator - empty item"
            emit [0x00, 0x00, 0x00, 0x00]
            emit [0x00, 0x00, 0x00, 0x00]
            emit [0x00, 0x00, 0x00, 0x00]
            emit [0x00, 0x00, 0x00, 0x00]
            emit [0x00, 0x00, 0x00, 0x00]

        label "end_import_directory_table"

        label "kernel32_IAT"
        comment "An Import Address Table is an array of 64-bit numbers for PE32+ (aka"
        comment "PE64)."
        comment "The bit 63 (most significant bit) is set to 0 (import by name "
        comment "rather than by ordinal)."
        comment "The bits 0-30 are a pointer to the function name (RVA)"
        comment "During binding, the entries in the import address table are"
        comment "overwritten with the 64-bit addresses of the symbols that are"
        comment "being imported. These addresses are the actual memory addresses "
        comment "of the symbols (MSDN)"

        label "GetStdHandle"
        emitLabelDiff32 "begin_pe64_image" "GetStdHandle_name"
        emit [0x00, 0x00, 0x00, 0x00]

        label "WriteFile"
        emitLabelDiff32 "begin_pe64_image" "WriteFile_name"
        emit [0x00, 0x00, 0x00, 0x00]

        label "ReadFile"
        emitLabelDiff32 "begin_pe64_image" "ReadFile_name"
        emit [0x00, 0x00, 0x00, 0x00]

        label "ExitProcess"
        emitLabelDiff32 "begin_pe64_image" "ExitProcess_name"
        emit [0x00, 0x00, 0x00, 0x00]

        comment "Empty item to signify the kernel32_IAT end"
        emit [0x00, 0x00, 0x00, 0x00]
        emit [0x00, 0x00, 0x00, 0x00]

        label "kernel32_name"
        X64.emitString "KERNEL32.dll\0\0\0\0"

        comment "For Windows we link 4 Kernel API functions: the ones to get the "
        comment "handles for stdin and stdout, the WriteFile one and ReadFile to"
        comment "do IO and ExitProcess to exit."

        label "GetStdHandle_name"
        emit [0x00, 0x00] -- Hint (An index into the export name pointer table)
        X64.emitString "GetStdHandle\0\0" -- Padding necessary to align to an even boundary

        label "WriteFile_name"
        emit [0x00, 0x00]
        X64.emitString "WriteFile\0"

        label "ReadFile_name"
        emit [0x00, 0x00]
        X64.emitString "ReadFile\0\0" -- Padding necessary to align to an even boundary

        label "ExitProcess_name"
        emit [0x00, 0x00]
        X64.emitString "ExitProcess\0" -- Padding necessary to align to an even boundary

    e_SectionStack = do
        comment "This is the memory space reserved for the stack and for the "
        comment "dictionary headers (which is a linked list)"
        X64.liftX64 $ ASM.addVirtualBytes pstackSizeBytes


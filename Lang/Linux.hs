module Lang.Linux where

import Data.Word

linux_sys_write = 4
linux_sys_exit  = 1
linux_sys_read  = 3

linux_stdin     = 0
linux_stdout    = 1
linux_stderr    = 2

linux_sys_brk :: Word64
linux_sys_brk   = 45

linux_sys_mprotect :: Word64
linux_sys_mprotect = 0x7D

linux_prot_none, linux_prot_read, linux_prot_write,linux_prot_exec :: Word64
linux_prot_none  = 0x00
linux_prot_read  = 0x01
linux_prot_write = 0x02
linux_prot_exec  = 0x04

module Lang.Windows where

import Data.Word

windows_stdout_handle, windows_stdin_handle :: Word64
windows_stdout_handle = 0xFFFFFFFFFFFFFFF5 -- -11
windows_stdin_handle  = 0xFFFFFFFFFFFFFFF6 -- -10


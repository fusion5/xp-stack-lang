module Runtime.X64.Parser (
  callRequiredParser
, callOptionalParser
, parse_fail, parse_reject, parse_success
)
where

import qualified X64.Types            as X64
import qualified X64.X64              as X64
import qualified Runtime.X64.BasicOps as BasicOps
import qualified Data.Word            as Word

parse_fail, parse_reject, parse_success :: Word.Word32
parse_fail         = 0 -- Parser failed and parsing should be stopped.
parse_reject       = 1 -- Parser rejected the input. Use the specific character
                       -- that was rejected with the next parser.
parse_success      = 2 -- Parser succeeded and consumed all input.
                       -- Further input should be requested.

callRequiredParser parserName failLabel = do
    X64.callLabel parserName
    BasicOps.ppop X64.rax
    X64.cmp X64.rax $ X64.I32 parse_reject
    X64.jeNear failLabel
    X64.cmp X64.rax $ X64.I32 parse_fail
    X64.jeNear failLabel

callOptionalParser parserName failLabel = do
    X64.callLabel parserName
    BasicOps.ppop X64.rax
    X64.cmp X64.rax $ X64.I32 parse_fail
    X64.jeNear failLabel

def decrement = dec. 
def duplicate = dup.

def factorial = 
    duplicate 
    decrement
    if_top_nz {
        drop_w64
        duplicate decrement
        factorial
        times
        ret
    }
    drop_w64
.

def main = 
    6
    factorial
    dbg_dump_ptop_w64
.

run main
q

(type event A B C D)

(proc ParABC () 
    (interleave
        (prefix A skip)
        (prefix B skip)
        (prefix C skip)))
(proc P ()
    (seq
        (unwind ParABC)
        (prefix D skip)))

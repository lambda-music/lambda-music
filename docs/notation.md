
Notation API for Lambda Music
==============================

Pulsar's `music.scm` defines basic procedures to manipulate musical notes in 
Lambda Music Sequencer.


## n:map


`n:map` is a scheme record which is defined in  srfi-9. `n:map` is a record
to specify the n-mapper.  This record is mainly used to wrap a procedure.

```scheme
 (n note: (n:map (lambda(x) (+ x 1 )))
     (melody "do re mi" ))
```

If a procedure wrapped by `n:map` record is specified on any parameters of `n`  
procedure, the specified procedure was called with every value which was 
preexisting in the specified notes. The code above effectively transposed  the 
specified melody.

[]: # (Sun, 28 Jun 2020 14:14:20 +0900)



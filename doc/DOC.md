# How to use
Load the teriminal.bin to the computer via the parallel port, or burn the file
to the 8kB EPROM.

Use a terminal program on a PC (ex. TeraTerm), connect the computer to the PC
via a nullmodem cable, and open the serial port with 8 bit data, 1 bit stop,
and 9600 baud rate.

After powering on the computer, the following welcome message should appear.
```
---CPU RESET---
ZLED - CDP1802 Terminal.

READY

>
```

## Commands
### print
Prints a stored variable to the terminal.

Example:
```
>print 34
34

>print "example"
example

>print varX
55
```

### let
Assign a value to a variable.

Example:
```
>let example = 7

>print example
7
```

### mem_view
Prints the contents of the memory from a given address in a HEX view format.

### mem_debug

### mem_alloc

### mem_free

### mem_set
Sets the pointer for the mem_write to the given address.

Example:
```
>mem_set 0x1ABC
```
### mem_write
Writes a signle byte or a sequece of bytes to the current memory pointer, set
by the mem_set command. The bytes can be given in hexadecimal and decimal format

Example:
```
>mem_set 0x1ABC

>mem_write 0x1A, 0xFF, 230, 10
```

### dma_set
Sets the DMA pointer to the given address.

Example:
```
>dma_set 0x1ABC
```

### exec
Makes a function call on a given address.

Example:
```
>exec 0x1ABC
```

# Assembly code
## Function call
The function call is performed by these lines of code.
```
;-FUNCTION CALL HELPER-------------------------
;-WHERE TO JUMP-(CALL_REG)---------------------
    org 0100h
    
FCALL
    sex STACK_REG   ;set STACK as X register
    ghi PC_REG      ;saving the PC value to stack
    stxd
    glo PC_REG
    stxd
    
    glo CALL_REG    ;put the new address in the PC
    plo PC_REG
    ghi CALL_REG
    phi PC_REG
    sep PC_REG      ;jumping to PC
    
FRETURN
    inc STACK_REG   ;increment the STACK register
    sex STACK_REG   ;set STACK as X register
    ldxa            ;restoring PC from STACK
    plo PC_REG
    ldx
    phi PC_REG
    sep PC_REG      ;jumping to PC
    br FRETURN
;----------------------------------------------
```

This code will save the program counter's value to the stack and replace it to
the value placed in the CALL_REG.

This part of the code is using its own program counter which is the FCALL_REG.
This register only need to be set when a function is performed, and in that
case, only the lower part of the register need to be set, because this code is
located on the 0x0100 addreess and it fits in a 256 bytes segment, and it never
leaves it, so only the lower part of the FCALL_REG is changing.

After a function call, when the program counter set back to the PC_REG,
FCALL_REG will stay at the "inc STACK_REG" line, so for return, changing the
program counter bact to the FCALL_REG (or to the RETURN which is the same as 
FCALL_REG) will cause to continue the program with the FRETURN part, and it
will load back the stored program counter value to the PC_REG. This ends in a
loop so multiple return can be called after eachother.

Function call example:
```
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call PRINT
```

In this example the PRINT function's address is loaded into the CALL_REG, and
then FCALL_REG lower part is set to the FCALL's address lower part, and then
the program counter register is set to FCALL_REG.

Return example:
```
    sep RETURN
```

This one just sets the program counter to RETURN which is the same as FCALL_REG.
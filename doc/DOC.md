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


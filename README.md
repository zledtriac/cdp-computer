# CDP computer
<div align="center">
  <img src="https://github.com/zledtriac/cdp-computer/blob/main/cdpcomputer.png">
</div>
This computer is based on the CDP1802 CPU, made for practicing and learning 
electronics and programming. The computer is similar to the COSMAC ELF but I 
designed it from scratch. I tried to fully utilize the features of the CPU and 
I hope I was able to implement enough stuff to demonstrate how an old computer
worked back then.

The fully assembled computer has a frontpanel with switches and two row of LEDs.
The first top row indicates the memory address, and then the next row indicates
the data and the control signals. The switches can be used to input data
in the memory and to control the CPU.

## The motherboard
I made the motherboard to be able to run by itself, so for someone who just want
to do some experiments with an old hardware, it is not necessary to build the 
whole computer. The motherboard contains the CPU, RAM, ROM and the clock 
generator.

### The clock generator
The clock runs on 2.4576MHz so if someone want to make a software Serial 
communcation with the Q and EF pins, 9600 baudrate can be achieved with high 
accuracy.

### The memory
The memory is divided into two sections ROM and RAM, but there is an option to
use only RAM for the program too. This is because EPROMs is a bit difficult to
obtain so for those who are just experimenting, the ROM is not necessary to
make the motherboard running, the RAM is enough. If the J5 pins are shorted the
motherboard will only use the RAM, but if those pins are not shorted then the
first 8192 bytes of RAM will be replaced by the ROM. With this feature the
motherboard can handle 64kB of RAM or 8kB of ROM and 56kB of RAM, depending on 
the state of the J5 pins.

### The DMA interface
The board has a DMA interface which can be used to upload programs or snipets of 
data in the memory while the CPU running, or if it is in the LOAD state.

The DMA interface has an 8bit data input port on the J2 pin header, and it has a 
write control signal on the J3 pin header (pin 2.). if there is a rising edge on 
the write pin, a DMA request will be sent to the CPU and the data pesent on the 
J2 pin header will be written in the memory location addressed by the R0 register 
in the CPU.

## The LED driver board
This board is an extension board, to handle the switches and the indicator LEDs. 
Since the CPU can't drive the LEDs directly, it needs a separate board to have 
the necessary ICs to drive the LEDs with relatively high current (5-6mA).

This board contains D-latches and some logic circuits to extend the 8bit address 
bus to 16bit and to generate a selectable 16Hz clock source and a circuit to be 
able to handle the CLEAR, WAIT, and STEP signals.

## The IO board
This board is made to add some input and output capabilities. The board has 3 
parallel input/output which can be configured from software. It is using the 
dedicated IO cotrol of the CPU so it doesn't need any special controlling routine.

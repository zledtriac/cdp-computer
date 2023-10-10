
DMA_REG EQU 0
INTP_REG EQU 1
INTX_REG EQU 2
PC_REG EQU 3
R4 EQU 4
R5 EQU 5
R6 EQU 6
R7 EQU 7
R8 EQU 8
R9 EQU 9
R10 EQU 10
R11 EQU 11
R12 EQU 12
CALL_REG EQU 13
FCALL_REG EQU 14
RETURN EQU 14
STACK_REG EQU 15

STACK_ADDRESS EQU 0FFFFh
DMA_ADDRESS EQU 03000h

INPUT_BUFF EQU 02000h

    org 00h

START
    dis
    idl
    
    seq     ;set Q output to high
    
    ldi STACK_ADDRESS.0 ;setup the STACK pointer
    plo STACK_REG
    ldi STACK_ADDRESS.1
    phi STACK_REG
    
    ldi FCALL.1         ;setup FCALL
    phi FCALL_REG
    
    ldi DMA_INIT.0      ;switch PC to R3 and init DMA
    plo PC_REG
    ldi DMA_INIT.1
    phi PC_REG
    
    sep PC_REG
    
DMA_INIT
    ldi DMA_ADDRESS.0   ;setup DMA address
    plo DMA_REG
    ldi DMA_ADDRESS.1
    phi DMA_REG
    
    lbr MAIN_PROGRAM
    
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

;-SERIAL SEND BYTE-----------------------------
;-DATA-R4.1------------------------------------
SERIAL_SEND_START
    ghi R4      ;R4 high is the data, load it into the D reg
    xri 0FFh    ;invert it
    phi R4      ;put it back
    ldi 085h    ;load 0x85 to the D reg
    shl         ;shift it left so we get 0x0A and set the flag bit
    plo R4      ;put the 0x10 to R4 low, this will be our bit counter

SERIAL_SEND_BIT
    lsnf        ;24 - long skip if the flag is not set
    req         ;16 - reset Q if the flag is set
    lskp        ;24
    seq         ;16 - set Q if the flag is not set
    nop         ;24
    
    dec R4      ;16 - decrement R4, this will only affect the R4 lower 8bit
    glo R4      ;16
    lsnz        ;24 - if R4 lower is not 0 then long skip
    sep RETURN  ;--
    nop         ;--
    ghi R4      ;16 - get R4 high
    shr         ;16 - shift it right to get LSB in the flag
    phi R4      ;16 - put it back
    nop         ;24
    nop         ;24
    nop         ;24
    br SERIAL_SEND_BIT ;16
;----------------------------------------------

;-SERIAL READ BYTE-----------------------------
;-RETURN R5.1----------------------------------
SERIAL_READ_START
    ldi 00h     ;set input data register to 0
    plo R5
    phi R5
    
SERIAL_WAIT_START
    bn1 SERIAL_WAIT_START ;16 - loop until EF1 goes low
    nop         ;24
    nop         ;24
    nop         ;24
    nop         ;24
    ori 00h     ;16 - do nothing for 16 clock cycles

SERIAL_SAMPLE_BIT
    b1 SERIAL_NULL_BIT  ;16 - checking the EF1
    ori 080h            ;16 - if EF1 is high then set MSB to 1 in D register
    br SERIAL_SAVE_BIT  ;16

SERIAL_NULL_BIT
    ori 00h         ;16 - of EF1 is low we do nothing
    ori 00h         ;16

SERIAL_SAVE_BIT
    phi R5          ;16 - put D in R5 high
    inc R5          ;16 - increment R5, this will only affect R5 low 8bit
    glo R5          ;16 - get R5 low to D register
    xri 09h         ;16 - D = D ^ 0x09
    lsnz            ;24 
    sep RETURN      ;--
    nop             ;--
    ghi R5          ;16 - get R5 high
    shr             ;16 - shift it righ
    nop             ;24
    nop             ;24
    nop             ;24
    br SERIAL_SAMPLE_BIT ;16
;----------------------------------------------

;-PRINT----------------------------------------
;-FIRST CHAR-R6--------------------------------
PRINT
    sex R6          ;set X register to R6
    ldxa            ;load data to D and increment R6
    lsnz            ;if it is not zero then long skip
    sep RETURN
    nop
    
    phi R4          ;put D register to R4 to send it
    
    ldi SERIAL_SEND_START.0     ;prepare to call SERIAL_SEND
    plo CALL_REG
    ldi SERIAL_SEND_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG   ;call SERIAL_SEND
    br PRINT
;----------------------------------------------

BACKSPACE_ACTION
    db 8, 32, 8, 0
;-READ LINE------------------------------------
;-WHERE TO READ-R7-----------------------------
;-R8.1-Character counter-------------------------
READLINE
    ldi 0
    plo R8
    
READLINE_LOOP
    ldi SERIAL_READ_START.0     ;prepare calling SERIAL_READ
    plo CALL_REG
    ldi SERIAL_READ_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call SERIAL_READ
    
    ghi R5              ;get the result of SERIAL_READ
    xri 13              ;check if it is 13 (ENTER KEY)
    bnz READLINE_BACKSPACE      ;if not jump to READLINE_BACKSPACE
    
END_LINE
    ldi 00h             ;put 0 to the endof the string
    str R7
    
    ldi NEW_LINE.0      ;load the address of CRLF string
    plo R6
    ldi NEW_LINE.1
    phi R6
    
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call PRINT
    
    sep RETURN          ;RETURN from READLINE
    
READLINE_BACKSPACE
    ghi R5              ;get the result of SERIAL_READ
    xri 127             ;check if it is 127 (DEL)
    bnz STORE_CHAR      ;if not jump to STORE_CHAR
    
    glo R8
    bz READLINE_LOOP
    
    ldi BACKSPACE_ACTION.0      ;load the address of the backspace action
    plo R6
    ldi BACKSPACE_ACTION.1
    phi R6
    
    ldi PRINT.0                 ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG               ;call PRINT
    
    dec R7
    dec R8
    
    br READLINE_LOOP
    
STORE_CHAR
    glo R8
    xri 0FFh
    bz READLINE_LOOP
    
    ghi R5              ;get the result of the SERIAL_READ again
    str R7              ;store it where R7 pointing
    inc R7              ;increment R7
    inc R8
    
    phi R4              ;load the result to R4 to Echo it back
    
    ldi SERIAL_SEND_START.0     ;prepare to call SERIAL_SEND
    plo CALL_REG
    ldi SERIAL_SEND_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call SERIAL_SEND
    br READLINE_LOOP
;----------------------------------------------

;-STR COMPARATOR-------------------------------
;-R8 stringA-R9 stringB------------------------
;-RETURN R10.0---------------------------------
STR_COMPARATOR
    ldi 0           ;R10 low is the result, set it to 0
    plo R10
	
STR_CHARCHECK
    ldn R8          ;load value from RAM[R8] to D register
    sex R9          ;set X pointer to R9
    xor             ;D = D ^ RAM[R9]
    
    bz STR_EQUAL    ;if D == 0 jump to STR_EQUAL
    sep RETURN      ;RETURN
    
STR_EQUAL 
	ldn R8          ;load value from RAM[R8] to D register again
	bnz STR_INC     ;if D != 0 jump to STR_INC
	ldi 1           ;set R10 to 1 and return
	plo R10
	sep RETURN
	
STR_INC
	inc R8          ;increment both address by 1
	inc R9
	br STR_CHARCHECK
;----------------------------------------------

;-INTEGER DIV----------------------------------
;-R4 pointing to divident----------------------
;-R5 pointing to divider-----------------------
;-R6 pointing to quotient----------------------
;-R7 pointing to remainder---------------------
;-R8.0 bitcnt, R8.1 flags----------------------
;-R9 general counter---------------------------
INTEGER_DIV
    sex STACK_REG   ;set STACK_REG as data pointer to store variables in STACK
    ldi 0           ;set bitcnt and flags to 0
    plo R8
    phi R8
    
    inc R5          ;increment R5 to the MSB because we want to store the divider it in little-endian format
    inc R5
    inc R5
    
    ldn R5          ;copying starts here
    stxd
    dec R5
    
    ldn R5
    stxd
    dec R5
    
    ldn R5
    stxd
    dec R5
    
    ldn R5
    stxd            ;+11 divider, +11 shows the start of the data from the STACK pointer
    
    ldi 0           ;storing 0 for the subtractor
    stxd
    stxd
    stxd
    stxd            ;+7 subtractor
    
    ghi R6          ;saving the quotient pointer
    stxd
    glo R6
    stxd            ;+5 quotient pointer

    ghi R7          ;saving the remainder
    stxd
    glo R7
    stxd            ;+3 remainder pointer
    
    ghi R4          ;saving the dividend pointer
    stxd
    glo R4
    stxd            ;+1 dividend pointer
    
    sex R4
    
    ldxa            ;copy dividend to remainder
    str R7
    inc R7
    
    ldxa
    str R7
    inc R7
    
    ldxa
    str R7
    inc R7
    
    ldx
    str R7
    
    ldi 0
    str R6
    inc R6
    str R6
    inc R6
    str R6
    inc R6
    str R6

    glo STACK_REG   ;get the STACK_REG pointer to position it to the divider MSB
    adi 14          ;add 14 to it
    plo R5          ;store the new pointer to R5
    ghi STACK_REG   ;get the STACK_REG high part to add the carry to it
    adci 0          ;adding the carry
    phi R5          ;store the new high part to R5
    
    sex R5          ;set X data pointer to R5
    ldx             ;load data from RAM[R5]
    dec R5          ;decrement R5 to set it back to the divider's LSB
    dec R5
    dec R5
    ani 080h        ;check if sign bit is set
    
    lbz INTEGER_DIV_NEXT_CHECK
    
    ldi 082h        ;load 0x82 to the D register
    shl             ;shit it left so it will set the carry flag and the D registre will be 4
    plo R9          ;store it to R9 low
    
INTEGER_DIV_INVERT_LOOP1
    ldx             ;load data from RAM[R5]
    xri 0FFh        ;xor D with 0xFF to invert it
    adci 0          ;add the carry for to the result (inver+1)
    str R5          ;store the result to RAM[R5]
    irx             ;increment R5

    dec R9          ;decrement R8
    glo R9          ;get R8 low, and if it's not 0 then do the loop
    lbnz INTEGER_DIV_INVERT_LOOP1
    
    ghi R8          ;set the flag
    ori 1
    phi R8
    
INTEGER_DIV_NEXT_CHECK
    glo STACK_REG
    adi 3
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9              ;restore and get remainder MSB
    adi 3
    plo R4
    inc R9
    ldn R9
    adci 0
    phi R4
    
    sex R4
    ldx
    dec R4
    dec R4
    dec R4
    ani 080h            ;check if sign bit is set
    
    lbz INTEGER_SHIFT_DR
    
    ldi 082h
    shl
    plo R9
    
INVERT_LOOP2
    ldx         ;invert byte 3
    xri 0FFh
    adci 0
    str R4
    irx

    dec R9
    glo R9
    bnz INVERT_LOOP2
    
    ghi R8      ;set the flag
    ori 2
    phi R8
    
INTEGER_SHIFT_DR
    glo STACK_REG ;get divider MSB
    adi 11
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    ldi 0
    sex R5
    or
    irx
    or
    irx
    or
    irx
    or
    bz DIVISION_LOOP
    
SHIFT_DR
    glo STACK_REG ;get divider MSB
    adi 14
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R5
    
    ldi 4
    shl
    plo R9
    
CARRY_SHIFTING
    ldx
    shrc
    stxd
    
    dec R9
    glo R9
    bnz CARRY_SHIFTING
    
    inc R8
    
    br INTEGER_SHIFT_DR
    
DIVISION_LOOP
    glo R8              ;get R8 low
    sdi 32              ;if larger than 32 then finish the loop
    lbnf FINAL_CHECK

    glo STACK_REG       ;restore quotient pointer to R6
    adi 5
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R6
    inc R9
    ldn R9
    phi R6
    
    sex R6
    
    ldi 2
    shl
    plo R9
    
SHIFTING_Q
    ldx                 ;shifting quotient to left
    shlc
    str R6
    irx

    dec R9
    glo R9
    bnz SHIFTING_Q
    
    glo STACK_REG    ;restore remainder pointer to R4
    adi 3
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R4
    inc R9
    ldn R9
    phi R4
    
    glo STACK_REG    ;restore subtractor pointer to R5
    adi 7
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R4
    ldi 082h
    shl
    plo R9
    
TRY_SUBTRACTING         ;try subtracting
    ldn R5
    sdb
    irx
    inc R5
    
    dec R9
    glo R9
    bnz TRY_SUBTRACTING
    
    lbnf NEXT_BIT        ;if subtracting unsuccesful, jump to NEXT_BIT
    
    glo STACK_REG
    adi 5
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R6
    inc R9
    ldn R9
    phi R6
    
    ldn R6
    ori 1
    str R6
    
    glo STACK_REG    ;restore remainder pointer
    adi 3
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R4
    inc R9
    ldn R9
    phi R4
    
    glo STACK_REG    ;restore subtractor's pointer to R5
    adi 7
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R4
    
    ldi 082h
    shl
    plo R9
    
SUBTRACT_FOR_REAL
    ldn R5          ;subtract the subtractor from the remainder and store it
    sdb
    str R4
    irx
    inc R5
    
    dec R9
    glo R9
    lbnz SUBTRACT_FOR_REAL
    
NEXT_BIT
    glo STACK_REG    ;restore subtractor pointer to R5 and set it to MSB
    adi 10
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R5
    ldi 2
    shl
    plo R9
    
SHIFT_SUBTRACTOR
    ldx
    shrc
    stxd
    
    dec R9
    glo R9
    bnz SHIFT_SUBTRACTOR
    
    inc R8
    lbr DIVISION_LOOP
    
FINAL_CHECK
    ghi R8
    ani 2
    lbz FINAL_CHECK2
    
    glo STACK_REG    ;restore remainder pointer to R4
    adi 3
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R4
    inc R9
    ldn R9
    phi R4
    
    sex R4
    
    ldi 082h
    shl
    plo R9
    
INVERT_LOOP3
    ldx
    xri 0FFh
    adci 0
    str R4
    irx
    
    dec R9
    glo R9
    lbnz INVERT_LOOP3
    
FINAL_CHECK2
    ghi R8
    lbz THE_END
    xri 3
    lbz THE_END
    
    glo STACK_REG    ;restore quotient pointer to R6
    adi 5
    plo R9
    ghi STACK_REG
    adci 0
    phi R9
    
    ldn R9
    plo R6
    inc R9
    ldn R9
    phi R6
    
    sex R6
    
    ldi 082h
    shl
    plo R9
    
INVERT_LOOP4
    ldx
    xri 0FFh
    adci 0
    str R6
    irx
    
    dec R9
    glo R9
    bnz INVERT_LOOP4

THE_END
    glo STACK_REG       ;setting back the STACK_REG for return
    adi 14
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN          ;RETURN
;----------------------------------------------

;-INTEGER MUL----------------------------------
;-R4-multiplicand pointer----------------------
;-R5-multiplier pointer------------------------
;-R6-result pointer----------------------------
;-R7-general register--------------------------
INTEGER_MUL
    sex STACK_REG
    
    inc R4      ;increment R4 and R5 pointer to MSB
    inc R4      ;to copy the values to the local variables
    inc R4
    
    inc R5
    inc R5
    inc R5
    
    ldn R4      ;copying starts here
    stxd
    dec R4
    
    ldn R4
    stxd
    dec R4
    
    ldn R4
    stxd
    dec R4
    
    ldn R4
    stxd        ;+7 multiplicand
    
    ldn R5
    stxd
    dec R5
    
    ldn R5
    stxd
    dec R5
    
    ldn R5
    stxd
    dec R5
    
    ldn R5
    stxd        ;+3 multiplier
    
    ghi R6
    stxd
    glo R6
    stxd        ;+1 result pointer

    ldi 0       ;zeroing the result
    
    str R6
    inc R6
    
    str R6
    inc R6
    
    str R6
    inc R6
    
    str R6
    
    dec R6
    dec R6
    dec R6

INTEGER_MUL_LOOP
    glo STACK_REG   ;restore multiplier pointer
    adi 3
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R5          ;check if multiplier is 0
    ldi 0
    or
    irx
    or
    irx
    or
    irx
    or
    
    dec R5
    dec R5
    dec R5
    
    lbz INTEGER_MUL_END      ;if 0 then jump to the end
    
    ldn R5                  ;load the multiplier's LSB
    ani 1                   ;filter for the LSb
    bz INTEGER_MUL_SHIFT    ;if zero, skip the addition
    
    glo STACK_REG           ;restore multiplicand pointer
    adi 7
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG           ;restore result pointer
    plo R7
    ghi STACK_REG
    phi R7
    inc R7
    
    ldn R7
    plo R5
    inc R7
    ldn R7
    phi R5
    
    sex R5
    
    ldi 2                   ;prepare addition
    shl
    plo R7
    
INTEGER_MUL_ADD
    ldn R4
    adc
    str R5
    irx
    inc R4
    
    dec R7
    glo R7
    bnz INTEGER_MUL_ADD

INTEGER_MUL_SHIFT

    glo STACK_REG           ;set multiplier's pointer to MSB
    adi 6                   ;to shift it right
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R5
    
    ldi 2
    shl
    plo R7
    
INTEGER_MUL_SHMULTIPLIER
    ldx                     ;shifting right the multiplier
    shrc
    stxd
    
    dec R7
    glo R7
    bnz INTEGER_MUL_SHMULTIPLIER
    
    glo STACK_REG           ;restore multiplicand pointer
    adi 7                   ;to shift it left
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    sex R4
    
    ldi 2
    shl
    plo R7
    
INTEGER_MUL_SHMULTIPLICAND
    ldx                     ;shifting the multiplicand left
    shlc
    str R4
    irx

    dec R7
    glo R7
    lbnz INTEGER_MUL_SHMULTIPLICAND
    
    lbr INTEGER_MUL_LOOP
    
INTEGER_MUL_END
    glo STACK_REG
    adi 10
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN          ;RETURN    
;----------------------------------------------

;-PRINT HEX------------------------------------
;-R4-int pointer-------------------------------
;-R5.0-number of digits------------------------
;-Local registers------------------------------
;-R5.1-flag, R6--------------------------------
HEX_CHARS
    db "0123456789ABCDEF"
;----------------------------------------------
PRINT_HEX
    sex STACK_REG
    
    ghi R5  ;saving the state of the local registers
    stxd
    
    ghi R6
    stxd
    glo R6
    stxd
    
    inc R4  ;making a copy of the Integer
    inc R4
    inc R4
    
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd    ;+2 the integer
    
    ldi 0   ;+1 
    stxd
    
    glo STACK_REG   ;set R4 to point to the Integer
    plo R4
    ghi STACK_REG
    phi R4
    inc R4
    inc R4
    
    ldi 0           ;set R5 flags to 0
    phi R5
    glo R5              
    bnz PRINT_UNTIL_R
    ldi 1           ;if R5 low, the number of digits, is zero, then set R5 flag
    phi R5

PRINT_UNTIL_R
    ghi R5                  ;check the flag if set or not
    bz PRINT_CHECK_DIGIT    
    sex R4              ;checking the integer if it's zero
    ldi 0
    or
    irx
    or
    irx
    or
    irx
    or
    
    dec R4
    dec R4
    dec R4
    
    sex STACK_REG    
    bnz PRINT_DIGIT_LOOP
    br PRINT_HEX_END
    
PRINT_CHECK_DIGIT
    glo R5
    bz PRINT_HEX_END

PRINT_DIGIT_LOOP
    ldn R4
    ani 0Fh
    str STACK_REG
    ldi HEX_CHARS.0
    add
    plo R6
    ldi HEX_CHARS.1
    adci 0
    phi R6
    
    ldn R6
    stxd
    
    ldi 4
    plo R6
    
PRINT_HEX_SHIFT_LOOP1
    inc R4
    inc R4
    inc R4
    
    ldn R4
    shr
    str R4
    dec R4
    
    ldn R4
    shrc
    str R4
    dec R4
    
    ldn R4
    shrc
    str R4
    dec R4
    
    ldn R4
    shrc
    str R4
    
    dec R6
    glo R6
    bnz PRINT_HEX_SHIFT_LOOP1
    
    ghi R5      ;check if R5 flag is set, if set then skip decrementing.
    lsnz
    dec R5
    nop
    br PRINT_UNTIL_R

PRINT_HEX_END
    glo STACK_REG
    plo R6
    ghi STACK_REG
    phi R6
    inc R6
    
    ldi PRINT.0     ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo R6
    adi 4
    plo STACK_REG
    ghi R6
    adci 0
    phi STACK_REG
    
    sex STACK_REG   ;restore local registers
    ldxa
    plo R6
    ldxa
    phi R6
    ldx
    phi R5
    
    sep RETURN
;----------------------------------------------

;-PRINT DEC------------------------------------
;-R4-int pointer-------------------------------
;-R10-flags-------------------------------------
PRINT_DEC
    sex STACK_REG
    
    ldi 0
    plo R10
    phi R10
    
    inc R4
    inc R4
    inc R4
    
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd
    dec R4
    ldn R4
    stxd        ;+10 the integer
    
    ldi 0
    stxd
    stxd
    stxd
    ldi 0Ah
    stxd        ;+6 divider
    
    ldi 0
    stxd
    stxd
    stxd
    stxd        ;+2 remainder
    
    stxd        ;+1 output string buffer end
    
    glo STACK_REG
    plo R12
    ghi STACK_REG
    phi R12
    
    glo R12
    adi 13
    plo R4
    ghi R12
    adci 0
    phi R4
    
    sex R4
    ldx
    ani 080h
    bz PRINT_DEC_MAIN_LOOP
    
    glo R10
    ori 1
    plo R10
    
    dec R4
    dec R4
    dec R4
    
    ldi 082h
    shl
    plo R6
    
PRINT_DEC_INVER_LOOP
    ldx
    xri 0FFh
    adci 0
    str R4
    inc R4
    
    dec R6
    glo R6
    bnz PRINT_DEC_INVER_LOOP
    
PRINT_DEC_MAIN_LOOP    
    glo R12
    adi 10
    plo R4
    plo R6
    ghi R12
    adci 0
    phi R4
    phi R6
    
    glo R12
    adi 6
    plo R5
    ghi R12
    adci 0
    phi R5
    
    glo R12
    adi 2
    plo R7
    ghi R12
    adci 0
    phi R7
    
    ldi INTEGER_DIV.0
    plo CALL_REG
    ldi INTEGER_DIV.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    sex STACK_REG
    
    glo R12
    adi 2
    plo R7
    ghi R12
    adci 0
    phi R7
    
    ldn R7
    adi 48
    stxd
    
    glo R12
    adi 10
    plo R4
    ghi R12
    adci 0
    phi R4
    
    sex R4
    
    ldi 0
    or
    irx
    or
    irx
    or
    irx
    or
    
    bz PRINT_DEC_END
    lbr PRINT_DEC_MAIN_LOOP

PRINT_DEC_END
    glo R10
    ani 1
    bz PRINT_DEC_FINAL
    
    sex STACK_REG
    ldi 45
    stxd

PRINT_DEC_FINAL
    glo STACK_REG
    plo R6
    ghi STACK_REG
    phi R6
    inc R6
    
    ldi PRINT.0
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo R12
    adi 13
    plo STACK_REG
    ghi R12
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-READ DEC-------------------------------------
;-R4-string pointer----------------------------
;-R5-result pointer----------------------------
READ_DEC
    sex STACK_REG
    
    ghi R4
    stxd
    glo R4
    stxd        ;+7 string pointer
    
    ghi R5
    stxd
    glo R5
    stxd        ;+5 result pointer
    
    ldi 0
    stxd
    stxd
    stxd
    ldi 0Ah
    stxd        ;+1 multiplier
    
READ_DEC_MAIN_LOOP
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    ldn R4
    smi 48
    bnf READ_DEC_END
    smi 10
    bdf READ_DEC_END
    
    glo STACK_REG
    adi 5
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    phi R6
    glo R4
    plo R6
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi INTEGER_MUL.0     ;prepare to call INTEGER_MUL
    plo CALL_REG
    ldi INTEGER_MUL.1       
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    glo STACK_REG
    adi 5
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R5
    inc R6
    ldn R6
    phi R5
    
    sex R5
    
    ldn R4
    smi 48
    
    add
    str R5
    inc R5
    
    ldi 0
    adc
    str R5
    inc R5
    
    ldi 0
    adc
    str R5
    inc R5
    
    ldi 0
    adc
    str R5

    inc R4
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    glo R4
    str R6
    inc R6
    ghi R4
    str R6
    
    br READ_DEC_MAIN_LOOP
   
READ_DEC_END
    glo STACK_REG
    adi 5
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sex STACK_REG
    
    ldxa
    plo R5
    ldxa
    phi R5
    ldxa
    plo R4
    ldx
    phi R4
    
    sep RETURN      ;RETURN    
;----------------------------------------------

;-FACTOR---------------------------------------
;-R4-string pointer----------------------------
;-R5-result pointer----------------------------
;-R10-result code------------------------------
FACTOR
    sex STACK_REG
    
    ghi R4
    stxd
    glo R4
    stxd    ;+4 string pt
    
    ghi R5
    stxd
    glo R5
    stxd    ;+2 result pt
    
    ldi 0
    stxd    ;+1 flag, indicates if the factor needs to be negated
    
    
FACTOR_MAIN
    ldn R4
    xri 32
    lbz FACTOR_NEXT_CYCLE       ;if ' '
    ldn R4
    xri 43
    lbz FACTOR_NEXT_CYCLE       ;if '+'
    ldn R4
    xri 45
    lbz FACTOR_NEGATE           ;if '-'
    ldn R4
    xri 40
    lbz FACTOR_PARENTHESIS      ;if '('
    ldn R4
    smi 48
    lbnf FACTOR_END
    smi 10
    lbdf FACTOR_END
    
    ldi READ_DEC.0
    plo CALL_REG
    ldi READ_DEC.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    lbr FACTOR_END
    
FACTOR_PARENTHESIS
    inc R4
    
    ldi EXPRESSION.0
    plo CALL_REG
    ldi EXPRESSION.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG

FACTOR_PARENTHESIS_SKIPSPACES
    lda R4
    xri 32
    bz FACTOR_PARENTHESIS_SKIPSPACES
    dec R4
    
    lda R4
    xri 41
    bz FACTOR_END
    
    lbr FACTOR_MAIN
    
FACTOR_NEGATE
    glo STACK_REG
    plo R6
    ghi STACK_REG
    phi R6
    inc R6
    
    ldn R6
    xri 0FFh
    ani 1
    str R6
    
FACTOR_NEXT_CYCLE
    inc R4
    lbr FACTOR_MAIN
    
FACTOR_END
    glo STACK_REG
    plo R6
    ghi STACK_REG
    glo R6
    inc R6
    
    ldn R6
    lbz FACTOR_FINAL
    
    glo STACK_REG
    adi 2
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R5
    inc R6
    ldn R6
    phi R5
    
    sex R5
    
    ldi 082h
    shl
    plo R6
    
FACTOR_INVERTING
    ldx
    xri 0FFh
    adci 0
    str R5
    irx
    
    dec R6
    glo R6
    bnz FACTOR_INVERTING
    
FACTOR_FINAL
    glo STACK_REG
    adi 5
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-TERM-----------------------------------------
;-R4-string pointer----------------------------
;-R5-result------------------------------------
TERM
    sex STACK_REG
    
    ghi R4
    stxd
    glo R4
    stxd
    
    ghi R5
    stxd
    glo R5
    stxd
    
    ldi FACTOR.0
    plo CALL_REG
    ldi FACTOR.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
TERM_MAIN
    ldn R4
    xri 32
    lbz TERM_NEXT_CYCLE
    ldn R4
    xri 42
    lbz TERM_MULTIPLY       ;if '*'
    ldn R4
    xri 47
    bz TERM_DIVIDE          ;if '/'
    
    lbr TERM_END
    
TERM_DIVIDE
    inc R4
    
    ldi 0
    stxd
    stxd
    stxd
    stxd    ;+5 remainder
    
    stxd
    stxd
    stxd
    stxd    ;+1 divider
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi FACTOR.0
    plo CALL_REG
    ldi FACTOR.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG ;save R4 pointer
    adi 11
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    glo R4
    str R6
    inc R6
    ghi R4
    str R6
    
    glo STACK_REG
    adi 9
    plo R8
    ghi STACK_REG
    adci 0
    phi R8
    
    ldn R8
    plo R4
    plo R6
    inc R8
    ldn R8
    phi R4
    phi R6
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    glo STACK_REG
    adi 5
    plo R7
    ghi STACK_REG
    adci 0
    phi R7
    
    ldi INTEGER_DIV.0
    plo CALL_REG
    ldi INTEGER_DIV.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 11
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    glo STACK_REG
    adi 8
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG

    lbr TERM_MAIN
    
TERM_MULTIPLY
    inc R4
    
    ldi 0
    stxd
    stxd
    stxd
    stxd
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi FACTOR.0
    plo CALL_REG
    ldi FACTOR.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    glo R4
    str R6
    inc R6
    ghi R4
    str R6
    
    glo STACK_REG
    adi 5
    plo R8
    ghi STACK_REG
    adci 0
    phi R8
    
    ldn R8
    plo R4
    plo R6
    inc R8
    ldn R8
    phi R4
    phi R6
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi INTEGER_MUL.0
    plo CALL_REG
    ldi INTEGER_MUL.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG

    lbr TERM_MAIN
    
TERM_NEXT_CYCLE
    inc R4
    lbr TERM_MAIN
    
TERM_END
    glo STACK_REG
    plo R6
    ghi STACK_REG
    phi R6
    inc R6
    
    lda R6
    plo R5
    lda R6
    phi R5
    
    glo STACK_REG
    adi 4
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-EXPRESSION-----------------------------------
;-R4-string pointer----------------------------
;-R5-result pointer----------------------------
EXPRESSION
    sex STACK_REG
    
    ghi R4      ;R4 pointer to input string 
    stxd
    glo R4
    stxd        ;+3 input string pointer
    
    ghi R5      ;R5 pointer to result
    stxd
    glo R5
    stxd        ;+1 result pointer
    
EXPRESSION_SKIPSPACES           ;skip spaces
    lda R4
    xri 32
    bz EXPRESSION_SKIPSPACES
    dec R4
    
    ldi TERM.0                  ;read TERM
    plo CALL_REG
    ldi TERM.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
EXPRESSION_MAIN
    ldn R4
    xri 32                      ;if ' '
    lbz EXPRESSION_NEXT_CYCLE
    ldn R4
    xri 43                      ;if '+'
    bz EXPRESSION_ADD
    ldn R4
    xri 45                      ;if '-'
    lbz EXPRESSION_SUB
    
    lbr EXPRESSION_END
    
EXPRESSION_ADD
    inc R4
    
    ldi 0               ;new result
    stxd
    stxd
    stxd
    stxd
    
    glo STACK_REG       ;set R5 pointer to new result
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi TERM.0          ;read TERM
    plo CALL_REG
    ldi TERM.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG       ; set R6 register to point to the input string pointer
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    glo R4              ;input string pointer = R4
    str R6
    inc R6
    ghi R4
    str R6
    
    glo STACK_REG       ;set R5 register to point to result
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    glo STACK_REG       ;set R6 register to point to the result pointer
    adi 5
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6              ;R4 = result pointer
    plo R4
    inc R6
    ldn R6
    phi R4
    
    sex R4
    
    ldi 2
    shl
    plo R7
    
EXPRESSION_ADD_LOOP     ;*result pointer += result;
    ldn R5
    adc
    str R4
    inc R5
    irx
    
    dec R7
    glo R7
    bnz EXPRESSION_ADD_LOOP
    
    sex STACK_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG

    lbr EXPRESSION_MAIN

EXPRESSION_SUB
    inc R4
    
    ldi 0
    stxd
    stxd
    stxd
    stxd
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi TERM.0
    plo CALL_REG
    ldi TERM.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    glo R4
    str R6
    inc R6
    ghi R4
    str R6
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    glo STACK_REG
    adi 5
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    sex R4
    
    ldi 082h
    shl
    plo R7
    
EXPRESSION_SUB_LOOP
    ldn R5
    sdb
    str R4
    inc R5
    irx
    
    dec R7
    glo R7
    bnz EXPRESSION_SUB_LOOP
    
    sex STACK_REG
    
    glo STACK_REG
    adi 7
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    ldn R6
    plo R4
    inc R6
    ldn R6
    phi R4
    
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG
    inc STACK_REG

    lbr EXPRESSION_MAIN

EXPRESSION_NEXT_CYCLE    
    inc R4
    lbr EXPRESSION_MAIN
    
EXPRESSION_END
    glo STACK_REG
    plo R6
    ghi STACK_REG
    phi R6
    inc R6
    
    lda R6
    plo R5
    lda R6
    phi R5
    
    glo STACK_REG
    adi 4
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-HEXVIEWER------------------------------------
;-R4-Start address-----------------------------
;-R5-Count-------------------------------------
HEX_BASE_SRT
    db "\r\nBase address: 0x",0
HEX_HEADER_STR
    db "\r\n\r\nOffset  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  String\r\n"
    db         "-------------------------------------------------------------------------\r\n",0
;              "0000:   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................"
HEX_LINECNT_SEPARATOR
    db ":   ",0
    
HEXVIEWER
    sex STACK_REG
    
    ldi 0
    stxd
    stxd
    ghi R4
    stxd
    glo R4
    stxd    ;+17 current address
    
    ldi 0
    stxd
    stxd
    ghi R4
    stxd
    glo R4
    stxd    ;+13 start address
    
    ldi 0
    stxd
    stxd
    ghi R5
    stxd
    glo R5
    stxd    ;+9 last address
    
    ldi 0
    stxd
    stxd
    stxd
    stxd    ;+5 line counter
    
    stxd
    stxd
    stxd
    stxd    ;+1 temp for hex digits
    
    glo STACK_REG
    adi 13
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG
    adi 9
    plo R6
    ghi STACK_REG
    adci 0
    phi R6
    
    sex R4
    
    glo R5      ;adding R5 value to start address and store it to last address
    add
    str R6
    irx
    inc R6
    ghi R5
    adc
    str R6
    irx
    inc R6
    ldi 0
    adc
    str R6
    
    ldi HEX_BASE_SRT.0     ;prepare to print base address
    plo R6
    ldi HEX_BASE_SRT.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG       ;prepare to print the address in hex
    adi 13
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    ldi 4               ;with 4 digits
    plo R5
    
    ldi PRINT_HEX.0     ;prepare to call PRINT_HEX
    plo CALL_REG
    ldi PRINT_HEX.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    ldi HEX_HEADER_STR.0     ;prepare to print the header
    plo R6
    ldi HEX_HEADER_STR.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG

HEXVIEWER_MAINLOOP
    glo STACK_REG       ;prepare to print the first address in hex
    adi 5
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    ldi 4               ;with 4 digits
    plo R5
    
    ldi PRINT_HEX.0     ;prepare to call PRINT_HEX
    plo CALL_REG
    ldi PRINT_HEX.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    ldi HEX_LINECNT_SEPARATOR.0     ;prepare to print address separator
    plo R6
    ldi HEX_LINECNT_SEPARATOR.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    ldi 16      ;set R8 low to 16 for counting the bytes
    plo R8
   
HEXVIEWER_PRINT_HEX_LOOP
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG       ;set R5 pointer to last address
    adi 9
    plo R5
    ghi STACK_REG
    adci 0
    phi R5

    sex R4          ;check if current address is >= last address
    
    lda R5
    sd
    irx
    
    lda R5
    sdb
    irx
    
    lda R5
    sdb
    irx
    
    ldn R5
    sdb
    
    lbdf HEXVIEWER_PRINT_HEX_SPACES   ;if current address >= last address then 

    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    lda R4              ;get current address and load it in R5
    plo R5
    lda R4
    phi R5
    
    ldn R5              ;get data from R5 address
    plo R5              ;store data in R5 low
    
    glo STACK_REG       ;set R4 pointer to temp
    plo R4
    ghi STACK_REG
    phi R4
    inc R4
    
    glo R5              ;get data from R5 low
    str R4              ;store data to temp
   
    ldi 2
    plo R5              ;set R5 low to 2 digits
    
    ldi PRINT_HEX.0     ;prepare to call PRINT_HEX
    plo CALL_REG
    ldi PRINT_HEX.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    lbr HEXVIEWER_NEXT_HEX
    
HEXVIEWER_PRINT_HEX_SPACES
    ldi HEX_LINECNT_SEPARATOR+2.0     ;prepare to print two space
    plo R6
    ldi HEX_LINECNT_SEPARATOR+2.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG

HEXVIEWER_NEXT_HEX
    ldi HEX_LINECNT_SEPARATOR+3.0     ;prepare to print a space
    plo R6
    ldi HEX_LINECNT_SEPARATOR+3.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    sex R4
    ldi 1
    add
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    
    dec R8
    glo R8
    lbnz HEXVIEWER_PRINT_HEX_LOOP
    
    ldi HEX_LINECNT_SEPARATOR+3.0     ;prepare to print a space
    plo R6
    ldi HEX_LINECNT_SEPARATOR+3.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG       ;set R5 pointer to start address
    adi 13
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R5
    
    ldxa
    str R4
    inc R4
    
    ldxa
    str R4
    inc R4
    
    ldxa
    str R4
    inc R4
    
    ldxa
    str R4
    
    ldi 16      ;set R8 low to 16 for counting the bytes
    plo R8

HEXVIEWER_PRINT_STR_LOOP
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG       ;set R5 pointer to last address
    adi 9
    plo R5
    ghi STACK_REG
    adci 0
    phi R5

    sex R4          ;check if current address is <= last address
    
    lda R5
    sd
    irx
    
    lda R5
    sdb
    irx
    
    lda R5
    sdb
    irx
    
    ldn R5
    sdb
    
    bpz HEXVIEWER_PRINT_STR_SPACE   ;if current address >= last address then 
    
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    lda R4              ;get current address and load it in R5
    plo R5
    lda R4
    phi R5
    
    ldn R5              ;get data from R5 address
    plo R5              ;store data in R5 low
    
    smi 32
    bm HEXVIEWER_PRINT_STR_DOT
    smi 95
    bpz HEXVIEWER_PRINT_STR_DOT
    
    glo R5
    phi R4
    
    ldi SERIAL_SEND_START.0         ;prepare to call SERIAL_SEND_START to send one char
    plo CALL_REG
    ldi SERIAL_SEND_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    br HEXVIEWER_NEXT_STR
    
HEXVIEWER_PRINT_STR_DOT
    ldi 46
    phi R4
    
    ldi SERIAL_SEND_START.0         ;prepare to call SERIAL_SEND_START to send one char
    plo CALL_REG
    ldi SERIAL_SEND_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    br HEXVIEWER_NEXT_STR

HEXVIEWER_PRINT_STR_SPACE
    ldi 32
    phi R4
    
    ldi SERIAL_SEND_START.0         ;prepare to call SERIAL_SEND_START to send one char
    plo CALL_REG
    ldi SERIAL_SEND_START.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
HEXVIEWER_NEXT_STR
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    sex R4
    ldi 1
    add
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    
    dec R8
    glo R8
    bnz HEXVIEWER_PRINT_STR_LOOP
    
    ldi NEW_LINE.0     ;prepare to print a space
    plo R6
    ldi NEW_LINE.1
    phi R6
	
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG       ;set R5 pointer to last address
    adi 9
    plo R5
    ghi STACK_REG
    adci 0
    phi R5

    sex R4          ;check if current address is >= last address
    
    lda R5
    sd
    irx
    
    lda R5
    sdb
    irx
    
    lda R5
    sdb
    irx
    
    ldn R5
    sdb
    
    bpz HEXVIEWER_END
    
    glo STACK_REG       ;set R4 pointer to line counter
    adi 5
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    sex R4
    
    ldi 16
    add
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    irx
    
    ldi 0
    adc
    str R4
    
    glo STACK_REG       ;set R4 pointer to current address
    adi 17
    plo R4
    ghi STACK_REG
    adci 0
    phi R4
    
    glo STACK_REG       ;set R5 pointer to start address
    adi 13
    plo R5
    ghi STACK_REG
    adci 0
    phi R5
    
    sex R4
    
    ldxa
    str R5
    inc R5
    
    ldxa
    str R5
    inc R5
    
    ldxa
    str R5
    inc R5
    
    ldxa
    str R5
    
    lbr HEXVIEWER_MAINLOOP

HEXVIEWER_END
    glo STACK_REG
    adi 20
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-HEXVIEW_CALLER-------------------------------
HEXVIEW_CALLER
    sex STACK_REG
    
    ldi 0
    stxd
    stxd
    stxd
    stxd    ;+1 result
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi EXPRESSION.0
    plo CALL_REG
    ldi EXPRESSION.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    lda R5
    plo R4
    lda R5
    phi R4
    
    ldi 0
    plo R5
    ldi 1
    phi R5
    
    ldi HEXVIEWER.0
    plo CALL_REG
    ldi HEXVIEWER.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 4
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-TEST FUNCTION--------------------------------
TEST_STR1
    db "Result: ",0

FUNC_TEST
    sex STACK_REG
    
    ldi 0
    stxd
    stxd
    stxd
    stxd    ;+1 result
    
    glo STACK_REG
    plo R5
    ghi STACK_REG
    phi R5
    inc R5
    
    ldi EXPRESSION.0
    plo CALL_REG
    ldi EXPRESSION.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    plo R4
    ghi STACK_REG
    phi R4
    inc R4

    ldi 0
    plo R5
    
    ldi PRINT_DEC.0     ;prepare to print the Decimal result.
    plo CALL_REG
    ldi PRINT_DEC.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    ldi NEW_LINE.0  ;print newline
    plo R6
    ldi NEW_LINE.1
    phi R6
	
    ldi PRINT.0     ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo STACK_REG
    adi 4
    plo STACK_REG
    ghi STACK_REG
    adci 0
    phi STACK_REG
    
    sep RETURN
;----------------------------------------------

;-COMMAND-CHECK--------------------------------
;-R4-input string------------------------------
COMMAND_CHECK
    ldi COMMAND_FUNC_LIST.0
    plo R5
    ldi COMMAND_FUNC_LIST.1
    phi R5
    
    ldi COMMAND_LIST.0
    plo R9
    ldi COMMAND_LIST.1
    phi R9
    
    glo R4
    plo R6
    ghi R4
    phi R6
    
COMMAND_CHECK_GOTOSPACE
    lda R6
    lbz COMMAND_CHECK_INSERTZERO
    xri 32
    lbz COMMAND_CHECK_REPLACESPACE
    lbr COMMAND_CHECK_GOTOSPACE
    
COMMAND_CHECK_INSERTZERO
    str R6
COMMAND_CHECK_REPLACESPACE
    dec R6
    str R6
    inc R6
    
COMMAND_CHECK_LOOP
    glo R4
    plo R8
    ghi R4
    phi R8
    
    ldn R9
    lbz COMMAND_CHECK_END

    ldi STR_COMPARATOR.0
    plo CALL_REG
    ldi STR_COMPARATOR.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    glo R10
    lbnz COMMAND_CHECK_EXEC
    
COMMAND_CHECK_STR_END
    ldn R9
    inc R9
    bnz COMMAND_CHECK_STR_END
    
    inc R5
    inc R5
    lbr COMMAND_CHECK_LOOP
    
COMMAND_CHECK_EXEC
    glo R6
    plo R4
    ghi R6
    phi R4
    
    lda R5
    plo CALL_REG
    ldn R5
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    
    sep RETURN
    
COMMAND_CHECK_END
    ldi UNKNOWN_COMMAND.0  ;set the address of the boot msg in R6
    plo R6
    ldi UNKNOWN_COMMAND.1
    phi R6
	
    ldi PRINT.0     ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG   ;call PRINT
    
    sep RETURN
;----------------------------------------------

;-MAIN-----------------------------------------
MAIN_PROGRAM
    ldi BOOT_MSG.0  ;set the address of the boot msg in R6
    plo R6
    ldi BOOT_MSG.1
    phi R6
	
    ldi PRINT.0     ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG   ;call PRINT

ASK_INPUT
    ldi INPUT_BUFF.0    ;load the address of the buffer to R7
    plo R7
    ldi INPUT_BUFF.1
    phi R7
    
    ldi READLINE.0      ;prepare to call READLINE
    plo CALL_REG
    ldi READLINE.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call READLINE
    
    ;Program goes here
    ldi INPUT_BUFF.0
    plo R4
    ldi INPUT_BUFF.1
    phi R4
    
    ldi COMMAND_CHECK.0      ;prepare to call COMMAND_CHECK
    plo CALL_REG
    ldi COMMAND_CHECK.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG
    ;Program ends here
    
    ldi ASK_IN.0        ;load the ASK_IN string's address to R6
    plo R6
    ldi ASK_IN.1
    phi R6
    
    ldi PRINT.0         ;prepare to call PRINT
    plo CALL_REG
    ldi PRINT.1
    phi CALL_REG
    
    ldi FCALL.0
    plo FCALL_REG
    sep FCALL_REG       ;call PRINT
	
    lbr ASK_INPUT
;----------------------------------------------

;-CONSTANT DATA--------------------------------
BOOT_MSG
    db "ZLED - CDP1802 Terminal.\r\n\r\nREADY\r\n"
ASK_IN
    db "\r\n>",0
NEW_LINE
    db "\r\n",0
COMMAND_LIST
    db "print",0,"mem_view",0,0
COMMAND_FUNC_LIST
    db FUNC_TEST.0,FUNC_TEST.1
    db HEXVIEW_CALLER.0,HEXVIEW_CALLER.1
UNKNOWN_COMMAND
    db "Unknown command.\r\n",0
TEST_RESP
    db "Everything is ok!",0

    end
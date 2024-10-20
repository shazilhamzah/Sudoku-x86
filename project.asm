[org 0x0100]
jmp start

; WELCOME AND BYE PAGES PRINTING
tone_divisors: dw 150
message: db "Welcome to Sudoku", 0                  ; Null-terminated string
continue_msg: db "Press any key to continue...", 0  ; Null-terminated string
thanks_msg db "Thanks for playing...", 0           ; Null-terminated string

; GRID - LINE PRINTING
horizontalLine: db 196 ; string to be printed
verticalLine: db 124
emptyString: db " "
length: dw 1 ; length of the string

; GRID - NUMBERS PRINTING
numbers: dw '1','2','3','4','5','6','7','8','9',0
row1: dw '8','2','6','4',' ','9','5',' ',' ',0
row2: dw '9',' ','5','2','6','7','8','4','3',0
row3: dw '4','3','7','1','5',' ',' ','6','2',0
row4: dw '6',' ','1','9','4','2','7',' ','8',0
row5: dw '7','9','2','5',' ','3','6','1','4',0
row6: dw '3','4',' ',' ','7','1','2','5','9',0
row7: dw '1','6','4','8','9','5','3','2','7',0
row8: dw '5','8','3','7','2',' ',' ','9','6',0
row9: dw '2','7',' ','3','1','6',' ','8','5',0
rowAddress: dw 0

; SCORE SCREEN PRINTING
time: dw 'Time: 00:00',0
scoreString: dw 'Score: 0',0




printstr: 
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80 ; load al with columns per row
    mul byte [bp+10] ; multiply with y position
    add ax, [bp+12] ; add x position
    shl ax, 1 ; turn into byte offset
    mov di,ax ; point di to required location
    mov si, [bp+6] ; point si to string
    mov cx, [bp+4] ; load length of string in cx
    mov ah, [bp+8] ; load attribute in ah

nextchar: 
    mov al, [si] ; load next char of string
    mov [es:di], ax ; show this char on screen
    add di, 2 ; move to next screen location 
    add si, 1 ; move to next char in string
    loop nextchar ; repeat the operation cx times
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 10 

settingVideoModeSubroutine:
    mov ah, 00h
    mov al, 03h  ; Mode 03h: 80x25 color text mode
    int 10h
    ret

blackBackgroundSubroutine:
    mov ah, 09h  ; BIOS function: Write character with attribute
    mov al, ' '  ; Space character
    mov bh, 00h  ; Page number
    mov bl, 0x00 ; Attribute: yellow background (0xE0 = yellow BG, black FG)
    mov cx, 2000 ; 80x25 screen = 2000 characters
    int 10h      ; Write to the screen 
    ret 

printColsSubroutine:
    mov cx, 3
    printCols:
        mov bx, 0
    loopCol:
        mov ax, cx
        push ax                 ; push x position
        mov ax, bx
        push ax                 ; push y position

        ; Check if cx is 27 or 51 for blue attribute and empty string, else use yellow
        cmp cx, 27
        je handleBlueEmptyString    ; If cx is 27, jump to handle blue and empty string
        cmp cx, 51
        je handleBlueEmptyString    ; If cx is 51, jump to handle blue and empty string

        ; Default case: use yellow attribute and verticalLine
    yellowAttribute:
        mov ax, 0x0E             ; Yellow on black attribute
        jmp pushVerticalLine     ; Jump to push verticalLine

    handleBlueEmptyString:
        mov ax, 0x70             ; Blue on black attribute
        push ax                  ; Push the attribute

        ; Push emptyString
        mov ax, emptyString      ; Address of emptyString
        push ax                  ; Push the string address
        push word [length]   ; Push the length of the empty string
        call printstr            ; Call the printstr subroutine
        jmp nextColumn           ; Jump to handle the next column

    pushVerticalLine:
        push ax                  ; Push the attribute
        mov ax, verticalLine     ; Address of verticalLine
        push ax                  ; Push the string address
        push word [length]       ; Push the length of verticalLine
        call printstr            ; Call the printstr subroutine

    nextColumn:
        inc bx                   ; Increment y position
        cmp bx, 25               ; Compare for the end of screen (25 rows)
        jle loopCol              ; If not yet, loop back to print next row

        add cx, 8                ; Increment x position (next column block)
        cmp cx, 81               ; Compare for the end of screen width (80 columns)
        jle printCols            ; If not yet, loop back to print next column block
        ret

printRowsSubroutine:
    mov bx, 0               ; Start at row 0
    printRows:
        mov cx, 3               ; Start at column 3
    loopRow:
        mov ax, cx
        push ax                 ; Push x (column) position
        mov ax, bx
        push ax                 ; Push y (row) position

        ; Check if bx is 9 or 18 for blue attribute and empty string
        cmp bx, 9
        je handleBlueEmptyString1  ; If bx is 9, jump to handle blue and empty string
        cmp bx, 18
        je handleBlueEmptyString1  ; If bx is 18, jump to handle blue and empty string

        ; Default case: use yellow attribute and horizontalLine
    yellowAttribute1:
        mov ax, 0x0E            ; Yellow on black attribute
        jmp pushHorizontalLine1 ; Jump to push horizontalLine

    handleBlueEmptyString1:
        mov ax, 0x7F            ; Blue on black attribute
        push ax                 ; Push the attribute

        ; Push emptyString
        mov ax, emptyString      ; Address of emptyString
        push ax                 ; Push the string address
        push word [length]  ; Push the length of the empty string
        call printstr           ; Call the printstr subroutine
        jmp nextRow             ; Jump to handle the next row

    pushHorizontalLine1:
        push ax                 ; Push the attribute
        mov ax, horizontalLine  ; Address of horizontalLine
        push ax                 ; Push the string address
        push word [length]      ; Push the length of horizontalLine
        call printstr           ; Call the printstr subroutine

    nextRow:
        add cx, 1               ; Increment column position (cx)
        cmp cx, 75              ; Compare for the end of row width (75 is arbitrary, set for spacing)
        jle loopRow             ; If not yet, loop back to print next column

        add bx, 3               ; Increment y position (next row)
        cmp bx, 24              ; Compare for the end of screen (25 rows)
        jle printRows           ; If not yet, loop back to print next row
        ret

printBorder:
    mov cx,3
    rowTop:
        mov ax, cx
        push ax ; push x position
        mov ax, 0
        push ax ; push y position
        mov ax, 0x40 
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,75
        jle rowTop

    mov cx,3
    rowBottom:
        mov ax, cx
        push ax ; push x position
        mov ax, 24
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,75
        jle rowBottom
    
    mov cx,0
    colLeft:
        mov ax, 3
        push ax ; push x position
        mov ax, cx
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,25
        jle colLeft

    mov cx,0
    colRight:
        mov ax, 75
        push ax ; push x position
        mov ax, cx
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,25
        jle colRight
    ret

printNumbers:
    mov ax, row1
    mov [rowAddress], ax
    mov cx, 2        ; y pos
    printOuterLoop:
        mov bx, 0        ; counter
        mov dx, 7        ; x pos
    printInnerLoop:
        mov ax, dx
        push ax          ; push x position
        mov ax, cx
        push ax          ; push y position
        mov ax, 0x0F
        push ax          ; push attribute
        mov si, [rowAddress]
        add si, bx
        push si          ; push address of message
        push word 2      ; push message length
        call printstr    ; call the printstr subroutine
        add bx, 2
        add dx, 8
        cmp bx, 18
        jle printInnerLoop

        add cx, 3        ; Move to next row
        add word [rowAddress], 20  ; Move to next row's data
        cmp cx, 29
        jle printOuterLoop
        ret

systemPauseSubroutine:
    mov ah,00h
    int 16h
    ret

delay:
    mov cx, 0xFFFF              
    mov dx, 0xFFFF              
    delay_loop:
        loop delay_loop            
        ret
        
        
        
    delay2:
        mov cx, 0x04AA             
        mov dx, 0xFFFF             
    delay_loop2:
        loop delay_loop2            
        ret

clrscr:
    push es
    push ax
    push di 
    mov ax, 0xb800
    mov es, ax
    mov di, 0

nextloc:
    mov word [es:di], 0x0720 
    add di, 2
    cmp di, 4000
    jne nextloc

    pop di
    pop ax
    pop es
    ret

clrscr_page1:
    push es            ; Save ES register
    push ax            ; Save AX register
    push di            ; Save DI register

    mov ax, 0xb800     ; Set segment to video memory
    mov es, ax         ; ES = 0xB800 (video memory segment)
    mov di, 0x1000     ; Start at offset 0x1000 (beginning of page 1)

nextloc_page1:
    mov word [es:di], 0x0720  ; Store space character (0x20) with attribute (0x07)
    add di, 2                 ; Move to the next character position (2 bytes per character)
    cmp di, 0x2000            ; 0x2000 = 4000 in hex, end of page 1 memory
    jne nextloc_page1         ; If not done, keep clearing

    pop di            ; Restore DI register
    pop ax            ; Restore AX register
    pop es            ; Restore ES register
    ret               ; Return from the subroutine

clrscrwithdelay:
    push es
    push ax
    push di 
    mov ax, 0xb800
    mov es, ax
    mov di, 0

nextlocwithdelay:
    mov word [es:di], 0x0720 
    add di, 2

	call delay2
	
    cmp di, 4000
    jne nextlocwithdelay

    pop di
    pop ax
    pop es
    ret

startingscreen:
    
    mov ax, 0B800h
    mov es, ax 
    mov di, 80 * 24 * 2         
    mov cx, 160                 

    fill_bottom_line:
        mov al, '*'                 ; Character to display
        mov ah, 0x0E              
        stosw                       
        loop fill_bottom_line

        ; Fill the screen from bottom to top
        mov cx, 24                ; 24 rows to fill (excluding the bottom row)
        mov si, 80 * 24 * 2        ; Source offset for the bottom line
        mov di, 80 * 23	* 2     ; Destination offset for the second-to-last line

    fill_screen_upwards:                     
        mov bx, 80                  ; 80 columns to copy

copy_row:
    ; Copy the row from the last filled row
    mov ax, [es:si]             
    mov [es:di], ax             
    add si, 2                   
    add di, 2                  
    dec bx                      ; Decrement column count
    jnz copy_row  
	
    ; Move up to the previous row
    sub si, 160                 ; Move SI to the previous row 
    sub di, 160    
   
   
   call delay
   call play_tone_sequence
   sub di,160
   cmp di,0
   je endofrows
   sub cx  ,1   
   jnz fill_screen_upwards  ; Repeat for all rows above
   ret
   
    endofrows:
    ret

play_tone_sequence:
    mov cx,1
        ; Load the next frequency divisor from the tone_divisors array
    mov bx, cx           ; Copy cx to bx because we need to modify bx
    shl bx, 1            ; Multiply bx by 2 (shifting left by 1 is equivalent to multiplying by 2)
    sub bx, 2            ; Subtract 2 from bx (adjust for 0-based index)
    mov ax, [tone_divisors + bx] ; Now access the array using the calculated index    

    call play_tone
     ; Play the tone
    dec cx                           ; Decrease tone count
    jnz play_tone_sequence           ; If not zero, play next tone

    ; Exit program (you'll need to replace this with an appropriate exit routine)
    ret

play_tone:
    ; Set the PIT to mode 3 (square wave generation)
    mov al, 0b10110110  ; Channel 2, mode 3, binary mode
    out 0x43, al          ; Send the control word to the PIT

    ; Send the divisor to the PIT for the current tone
    mov al, al             ; Low byte of the divisor
    out 0x42, al           ; Send low byte to the channel 2 data port (0x42)
    mov al, ah             ; High byte of the divisor
    out 0x42, al           ; Send high byte to the channel 2 data port (0x42)

    ; Enable the PC speaker (turn it on)
    in al, 0x61            ; Read port 0x61 (PC speaker control)
    or al, 00000011b       ; Set bits 0 and 1 (enable speaker)
    out 0x61, al           ; Write back to port 0x61
   
    ; Delay to hold the tone for a short while (this acts as the duration of the tone)
    ;mov dx, 0FFFFh         ; Delay counter
	ret

  turnoffspeakers:
    in al, 0x61            ; Read port 0x61
    and al, 11111100b      ; Clear bits 0 and 1 (disable speaker)
    out 0x61, al           ; Write back to port 0x61
    ret  

display_message_effect:
    push es
    push ax
    push di

    mov ax, 0xb800              
    mov es, ax
    mov si, message             ; Point SI to the message
    mov cx, 18                  ; Length of mssg
    mov di, (10 * 80 + 30) * 2  ; Moving two rows above the middle 

print_next_char:
    lodsb                       ; Load next character into AL
    or al, al
    jz done                     

    mov ah, 0x0E               
    mov [es:di], ax             
    add di, 2                   ; Move to the next character position

    call delay2_char   
    call delay2_char  
    call delay2_char   ; Small delay between characters
    loop print_next_char        

done:
    pop di
    pop ax
    pop es
    ret

display_continue_msg:
    push es
    push ax
    push di

    mov ax, 0xb800              
    mov es, ax
    mov si, continue_msg        ; Point SI to the continue message
    mov cx, 27                  ; Length of mssg
    mov di, (12 * 80 + 28 - 2) * 2  

print_continue_char:
    lodsb                       ; Load next character into AL
    or al, al
    jz done_continue            ; If null terminator, we're done

    mov ah, 0x0E                
    mov [es:di], ax            
    add di, 2                   ; Move to the next character position
    call delay2_char
    call delay2_char
    call delay2_char
    loop print_continue_char    

    done_continue:
        pop di
        pop ax
        pop es
        ret

    delay2_char:
        mov cx, 0xFFFF
        mov dx, 0xFFFF
    delay_loop2_char:
        loop delay_loop2_char
        ret	
	
display_thanks_message_effect:
    push es
    push ax
    push di

    mov ax, 0xb800             
    mov es, ax
    mov si, thanks_msg          
    mov cx, 20                  ; Length of mssg
    mov di, (12 * 80 + 30) * 2  ; Position in the middle of the screen 

    print_thanks_char:
        lodsb                       
        or al, al
        jz done_thanks              
        mov ah, 0x0E                
        mov [es:di], ax             
        add di, 2                  

        call delay2_char            ; Small delay for the typing effect
        call delay2_char
        call delay2_char
        loop print_thanks_char      

    done_thanks:
        pop di
        pop ax
        pop es
        ret

toPage0Subroutine:
    mov ax,0500h
    int 10h
    ret

toPage1Subroutine:
    mov ax,0501h
    int 10h
    ret

timerPrintSubroutine:
    call clrscr_page1
    mov ah, 0x13 ; service 13 - print string
    mov al, 1 ; subservice 01 – update cursor
    mov bh, 1 ; output on page 0
    mov bl, 7 ; normal attrib
    mov dx, 0x0B24 ; row 11 column 36
    mov cx, 11 ; length of string
    push cs
    pop es ; segment of string
    mov bp, time ; offset of string
    int 0x10 ; call BIOS video service

scorePrintSubroutine:
    mov ah, 0x13 ; service 13 - print string
    mov al, 1 ; subservice 01 – update cursor
    mov bh, 1 ; output on page 0
    mov bl, 7 ; normal attrib
    mov dx, 0x0C26 ; row 12 column 38
    mov cx, 8 ; length of string
    push cs
    pop es ; segment of string
    mov bp, scoreString ; offset of string
    int 0x10 ; call BIOS video service
    call toPage1Subroutine
    ret

gameSubroutine:
    ; WELCOME SCREEN
    WELCOME:
        call clrscr             
        call startingscreen    
        call clrscrwithdelay
        call turnoffspeakers
        call display_message_effect  
        call display_continue_msg 
        call systemPauseSubroutine

    ; GAME GRID
    GAMEGRID:
        call clrscr 
        call settingVideoModeSubroutine
        call blackBackgroundSubroutine
        call printRowsSubroutine
        call printColsSubroutine
        call printBorder
        call printNumbers
        call systemPauseSubroutine
        call timerPrintSubroutine
        call systemPauseSubroutine
        call toPage0Subroutine

    ; END SCREEN
    FINAL:
        call clrscr
        call display_thanks_message_effect
        ret

start:
    call gameSubroutine
    jmp end

end:
    mov ax, 0x4C00
    int 21h
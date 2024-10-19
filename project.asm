[org 0x0100]
jmp main

horizontalLine: db 196 ; string to be printed
verticalLine: db 124
emptyString: db " "
length: dw 1 ; length of the string
numbers: db 1,2,3,4,5,6,7,8,9

clrscr: 
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov di, 0 ; point di to top left column

nextloc: 
    mov word [es:di], 0x0720 ; clear next char on screen
    add di, 2 ; move to next screen location
    cmp di, 4000 ; has the whole screen cleared
    jne nextloc ; if no clear next position
    pop di
    pop ax
    pop es
    ret

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

            ; Check if bx is 9 or 18 for blue attribute
            cmp bx, 9
            je blueAttribute1       ; If bx is 9, set blue attribute
            cmp bx, 18
            je blueAttribute1       ; If bx is 18, set blue attribute

            ; Default case: use yellow attribute
        yellowAttribute1:
            mov ax, 0x0E            ; Yellow on black attribute
            jmp pushAttribute1      ; Jump to push the attribute

        blueAttribute1:
            mov ax, 0x7F            ; Blue on black attribute

        pushAttribute1:
            push ax                 ; Push the attribute
            
            mov ax, horizontalLine         ; Load address of message
            push ax                 ; Push address of message
            push word [length]      ; Push message length
            call printstr           ; Call the printstr subroutine
            add cx, 1               ; Increment column position (cx)
            cmp cx, 75              ; Compare for the end of row width (75 is arbitrary, set for spacing)
            jle loopRow             ; If not yet, loop back
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
    ret

systemPauseSubroutine:
    mov ah,00h
    int 16h
    ret

start:
    ; CLEARING SCREEN
    call clrscr 

    ; SETTING VIDEO MODE TO 03h (80x25 COLOR TEXT MODE)
    call settingVideoModeSubroutine

    ; FILL SCREEN WITH A CHARACTER AND YELLOW BACKGROUND
    call blackBackgroundSubroutine

    ; PRINTING ROWS
    call printRowsSubroutine
    
    ; PRINTING COLUMNS
    call printColsSubroutine
    
    ; PRINTING BORDER
    call printBorder

    ; PRINT NUMBERS
    call printNumbers

    ; SYSTEM PAUSE
    call systemPauseSubroutine

    ; EXIT
    jmp end

main:
    jmp start

end:
    mov ax,0x4c00
    int 0x21
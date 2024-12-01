[org 0x0100]
jmp start

; GRID - NUMBERS PRINTING
numbers: dw '1','2','3','4','5','6','7','8','9',0
; row1: dw '8','2','6','4',' ','9','5',' ',' ',0
; row2: dw '9',' ','5','2','6','7','8','4','3',0
; row3: dw '4','3','7','1','5',' ',' ','6','2',0
; row4: dw '6',' ','1','9','4','2','7',' ','8',0
; row5: dw '7','9','2','5',' ','3','6','1','4',0
; row6: dw '3','4',' ',' ','7','1','2','5','9',0
; row7: dw '1','6','4','8','9','5','3','2',' ',0
; row8: dw '5','8','3','7','2','4','1','9',' ',0
; row9: dw '2','7',' ','3','1','6','4','8',' ',0

row1: dw ' ','2','6','4','3','9','5','7','1',0
row2: dw ' ','1','5','2','6','7','8','4','3',0
row3: dw '4','3','7','1','5','8','9','6','2',0
row4: dw '6','5','1','9','4','2','7','3','8',0
row5: dw '7','9','2','5','8','3','6','1','4',0
row6: dw '3','4','8','6','7','1','2','5','9',0
row7: dw '1','6','4','8','9','5','3','2','7',0
row8: dw '5','8','3','7','2','4','1','9','6',0
row9: dw '2','7','9','3','1','6','4','8','5',0

mrow1: dw '8',' ','6','4',' ','9','5',' ',' ',0
mrow2: dw '9',' ',' ','2','6','7','8','4','3',0
mrow3: dw '4','3','7','1','5',' ',' ','6','2',0
mrow4: dw '6',' ',' ','9','4','2','7',' ','8',0
mrow5: dw '7','9','2','5',' ','3','6','1','4',0
mrow6: dw '3','4',' ',' ','7',' ','2','5','9',0
mrow7: dw '1','6','4','8','9','5','3','2','7',0
mrow8: dw ' ','8','3','7','2',' ',' ',' ','6',0
mrow9: dw '2','7',' ','3','1','6',' ','8',' ',0

hrow1: dw '8','2','6','4',' ','9','5',' ',' ',0
hrow2: dw '9',' ','5',' ',' ',' ','8','4','3',0
hrow3: dw '4','3','7',' ','5',' ',' ','6','2',0
hrow4: dw '6',' ','1','9','4',' ','7',' ',' ',0
hrow5: dw '7',' ','2','5',' ',' ','6','1','4',0
hrow6: dw '3','4',' ',' ','7','1','2','5','9',0
hrow7: dw ' ','6',' ',' ','9','5','3','2',' ',0
hrow8: dw '5','8',' ',' ','2',' ',' ','9','6',0
hrow9: dw ' ','7',' ','3','1',' ',' ','8','5',0

;texts
you_won_text: db '!','!','!', ' ','Y', 'O', 'U', ' ', 'W', 'O', 'N',' ','!','!','!'
you_lost_text db '!','!','!', 'Y', 'O', 'U', ' ', 'L', 'O','S','T','!','!','!'

;sound variable
sound_index:dw 0


; GAME PLAYING VARIABLES
rowAddress: dw 0
difficulty: dw 0
cursorRow: dw 0
cursorRowRemain: dw 0
cursorCol: dw 0
cursorColRemain: dw 0
currentPage: dw 0
isValid: dw 0
isWin: db 0

; GRID - LINE PRINTING
horizontalLine: db 196 ; string to be printed
verticalLine: db 124
emptyString: db " "
length: dw 1           ; length of the string
noofboxes: dw 1
countofnumbers: dw 9, 9, 9, 9, 9, 9, 9, 9, 9

; WELCOME AND BYE PAGES PRINTING
tone_divisors: dw 150
message: db "Welcome to Sudoku", 0              
continue_msg: db "Press any key to continue...", 0
thanks_msg db "Thanks for playing...", 0

; SCORE SCREEN PRINTING
time: dw 'Time: 00:00',0
scoreString: dw 'Score:',0
score: dw 0
mistakes: dw 0
gamestate: db 1
hrs: dw 0
min: dw 0
sec: dw 0
ms:  dw 0
ticks: dw 0
lastUpdateTime: dw 0
ms_per_cycle equ 55 

; PREV STATUS FOR UNDO
prevCursorRow dw 0
prevCursorCol dw 0
prevCursorRowRemain dw 0
prevCursorColRemain dw 0
prevPage db 0
prevCardsRemaining: dw 0,0,0,0,0,0,0,0,0


; LOSING/WINING SCREENS
play_loosing_sound:
    .loop:

                ; send DSP Command 10h
                mov dx, 22ch
                mov al, 10h
                out dx, al

                ; send byte audio sample
                mov si, [sound_index]
                mov al, [sound_data + si]
                out dx, al

                mov cx, 175 ;500 good
            .delay:
                nop
                loop .delay

                inc word [sound_index]
                cmp word [sound_index], 12374
                jb .loop
                mov word[sound_index],0
                ret
        sound_data:
            incbin "gov.wav"

checkerboard_loosing_screen:
    pusha

   
    mov ax, 0xB800
    mov es, ax

    ; Initialize variables
    xor di, di               
    mov cx, 2000              


draw_checkerboard2:
    mov ax, 0x0201          
    test di, 2                ; Check if current cell is even or odd
    jz alternate_color2
    mov ax, 0xD201          

    alternate_color2:
    stosw                     ; Write to video memory
    loop draw_checkerboard2

    ; Calculate the starting position for the border and text
    mov di, 12 * 160 + 32 * 2 
    ; Draw top border
        mov cx, 16                ;length of border
        mov ax, 0x2A0F            ; '*' character
    draw_top_border2:
        stosw
        loop draw_top_border2

    ; Draw sides and text
    mov di, 12 * 160 + 34 * 2  
	
    mov cx, 3                 ; Number of rows for sides

    draw_sides_and_text2:
    push cx                   ; Save the row count

    ; Left border
    mov ax, 0x2A0F            ; '*' character 

    ; Display "!!!YOU lost!!!" on the middle row
    cmp cx, 2
    jne draw_right_border2     
    mov si, you_lost_text
    mov cx, 14                ; Length of "!!!YOU lost!!!"

print_text2:
    lodsb                     
    mov ah, 0xF0             
    stosw                     ; Write character and attribute to video memory
    loop print_text2

draw_right_border2:
    mov ax, 0x2A0F            ; '*' 
    stosw                  

    add di, 160 - 4         ; Move to the next row 
    pop cx                    ; Restore row count
    loop draw_sides_and_text2

    ; Draw bottom border
    mov di, 14 * 160 + 32 * 2 
    mov cx, 15                ; Length of the border

draw_bottom_border2:
    mov ax, 0x2A0F            ; '*' character 
    stosw
    loop draw_bottom_border2
	
	  mov di, 16 * 160 +46  * 2 
	mov si,scoreString
	mov cx,8

print_score2:
    lodsb
    mov ah, 0xF0
    stosw
    loop print_score2

    ; push 2668
    ; push word [score]
    ; mov ax,0xb800
    ; mov es,ax
    ; call printnum

    ; mov di,2668        ; same position
    ; mov cx,4           ; assuming score is 1 digit - adjust if needed
    ; mov ah,0xF0        ; white background (0xF) and blinking black (0x0)
    ; attr_loop1:
    ;     inc di         ; move to attribute byte
    ;     mov [es:di],ah ; set attribute
    ;     inc di         ; move to next character
    ;     loop attr_loop1
    ;     mov ah,' '
    ;     mov [es:di],ah

    ; score printing
    cmp word [score], 0
    jge print_pos1       ; if score >= 0, print normally
    
    ; Handle negative score
    mov ax, 0xb800
    mov es, ax          ; point es to video base
    
    ; Print minus sign first
    mov di, 2664        ; Your desired position
    mov byte [es:di], '-'
    mov byte [es:di+1], 0x07  ; attribute
    add di, 2           ; move position past minus sign
    
    ; Convert score to positive
    mov ax, [score]
    neg ax              ; make positive
    push 2666          ; position after minus sign
    push ax            ; push positive number
    jmp do_print1

    print_pos1:
        mov ax, 0xb800
        mov es, ax          ; point es to video base
        push 2666
        push word [score]

    do_print1:
        call printnum
    

    mov di,2664        ; same position
    mov cx,5           ; assuming score is 1 digit - adjust if needed
    mov ah,0xF0        ; white background (0xF) and blinking black (0x0)
    attr_loop1:
        inc di         ; move to attribute byte
        mov [es:di],ah ; set attribute
        inc di         ; move to next character
        loop attr_loop1

    mov di, 16 * 160 + 24 * 2 
	
    mov si, time
    mov cx, 11                ; Length of "Time: 00:00"

print_time2:
    lodsb
    mov ah, 0xF0
    stosw
    loop print_time2

    popa
    ret
	
wrapper_make_loosing_screen:
	  call checkerboard_loosing_screen

loptiloop2:
    call play_loosing_sound
    jmp loptiloop2
	ret
    ;///////////////////////////////////////////////winning screen/////////////////////////

checkerboard_winning_screen:
    pusha

    mov ax, 0xB800
    mov es, ax

    ; Initialize variables
    xor di, di                
    mov cx, 2000             

draw_checkerboard:
    mov ax, 0x0201          
    test di, 2                ; Check if current cell is even or odd
    jz alternate_color
    mov ax, 0xD201            

alternate_color:
    stosw                     ; Write to video memory
    loop draw_checkerboard


    mov di, 12 * 160 + 32 * 2 ; Position for the top-left corner of the border
    
    ; Draw top border
    mov cx, 17                ; Length of the border
    mov ax, 0x2A0F            ; '*' character

draw_top_border:
    stosw
    loop draw_top_border

    ; Draw sides and text
    mov di, 12 * 160 + 32 * 2  
	
    mov cx, 3                 ; Number of rows for sides

draw_sides_and_text:
    push cx                  

    ; Left border
    mov ax, 0x2A0F            ; '*' 
    stosw

    ; Display "!!!YOU WON!!!" on the middle row
    cmp cx, 2
    jne draw_right_border     
    mov si, you_won_text
    mov cx, 15                ; Length of "!!!YOU WON!!!"

print_text:
    lodsb                     
    mov ah, 0xF0
    stosw                    
    loop print_text

draw_right_border:
    mov ax, 0x2A0F            ; '*' character 
    stosw                    
    add di, 160 - 4         ; Move to the next row 
    pop cx                   
    loop draw_sides_and_text

    ; Draw bottom border
    mov di, 14 * 160 + 32 * 2 
    mov cx, 15                ; Length of the border

draw_bottom_border:
    mov ax, 0x2A0F            ; '*' character 
    stosw
    loop draw_bottom_border
   
    mov di, 16 * 160 +46  * 2
	mov si,scoreString
	mov cx,6

    print_score:
        lodsb
        mov ah, 0xF0
        stosw
        loop print_score

        push 2668
        push word [score]
        mov ax,0xb800
        mov es,ax
        call printnum

        mov di,2668        ; same position
        mov cx,4           ; assuming score is 1 digit - adjust if needed
        mov ah,0xF0        ; white background (0xF) and blinking black (0x0)
        attr_loop:
            inc di         ; move to attribute byte
            mov [es:di],ah ; set attribute
            inc di         ; move to next character
            loop attr_loop
            mov ah,' '
            mov [es:di],ah

    mov di, 16 * 160 + 24 * 2 ; Position for time
    
    mov si, time
    mov cx, 11                ; Length of "Time: 00:00"

    print_time:
        lodsb
        mov ah, 0xF0
        stosw
        loop print_time
        popa
        ret

play_winning_sound:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6
    out 0x43, al

    ; High pitch tone
    mov ax, 1193          ; Divider for ~1000 Hz
    call set_frequency
    call delay1

    ; Medium pitch tone
    mov ax, 1493          ; Divider for ~800 Hz
    call set_frequency
    call delay1

    ; Higher pitch tone
    mov ax, 995           ; Divider for ~1200 Hz
    call set_frequency
    call delay1

    ; Return to medium pitch 
    mov ax, 1493
    call set_frequency
    call delay1

    ; Finish with a low pitch 
    mov ax, 2387          ; Divider for ~400 Hz
    call set_frequency
    call delay1

    ; Turn off the speaker
    call turnoffspeakers
    ret
	wrapper_make_winning_screen:
    call checkerboard_winning_screen
    loptiloop:
    call play_winning_sound
    jmp loptiloop
    ret



; SOUND AND TIMER
correct_input:
    pusha
    call play_correct_input_sound   ;//////////////idk may be thori lambi ho but it can be adjusted ///// can also be played for row completion
    popa
    ret
    ;this will play for row and completion it will have like a 1 sec delay in the game
    ;we can directly use this or have a warapper

play_on_rowncol_completion:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6          ; Set PIT to mode 3 (square wave)
    out 0x43, al          ; Send control byte to port 0x43

    ; First note: C4 (261 Hz)
    mov ax, 4776          ; Divider for ~261 Hz
    call set_frequency
    call delay1          
    call delay1           
    call delay1           
    call delay1           
    call delay1           
    call delay1          

    ; Second note: D4 (294 Hz)
    mov ax, 4286          ; Divider for ~294 Hz
    call set_frequency
    call delay1
    call delay1
	call delay1          
    call delay1           
    call delay1           
    call delay1           
    call delay1           
    call delay1           


    ; Third note: E4 (329 Hz)
    mov ax, 3654          ; Divider for ~329 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1           
    call delay1           

    ; Fourth note: C4 (261 Hz)
    mov ax, 4776          ; Divider for ~261 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1           
    call delay1           

    ; Fifth note: G4 (392 Hz)
    mov ax, 3412          ; Divider for ~392 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1           
    call delay1           

    ; Sixth note: E4 (329 Hz)
    mov ax, 3654          ; Divider for ~329 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1           
    call delay1           

    ; Seventh note: C4 (261 Hz)
    mov ax, 4776          ; Divider for ~261 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1          
    call delay1           

    ; Eighth note: D4 (294 Hz)
    mov ax, 4286          ; Divider for ~294 Hz
    call set_frequency
    call delay1
    call delay1
    call delay1           
    call delay1           

    in al, 0x61         
    and al, 0xFC         
    out 0x61, al        

    ret
	
	;we will just need to call this function when we have a incorrect INPUT

play_incorrect_input_sound:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6          ; Set PIT to mode 3 (square wave)
    out 0x43, al          ; Send control byte to port 0x43

    ; First note: A3 (220 Hz)
    mov ax, 5454          ; Divider for ~220 Hz
    call set_frequency
    call delay1
    call delay1

    ; Second note: G3 (196 Hz)
    mov ax, 5734          ; Divider for ~196 Hz
    call set_frequency
    call delay1
    call delay1

    ; Third note: F3 (174 Hz)
    mov ax, 6550          ; Divider for ~174 Hz
    call set_frequency
    call delay1
    call delay1
    
    ; Fourth note: E3 (164 Hz)
    mov ax, 6826          ; Divider for ~164 Hz
    call set_frequency
    call delay1
    call delay1
    
    ; Fifth note: D3 (146 Hz)
    mov ax, 7519          ; Divider for ~146 Hz
    call set_frequency
    call delay1
    call delay1
    
    ; Stop sound
    in al, 0x61         
    and al, 0xFC         ; Turn off the speaker
    out 0x61, al         

    ret	

delay1:
	pusha
	mov cx,0xFFFF
	;mov dx,0xF000
	loop2:
	;dec dx
		dec cx
		jnz loop2
	popa
	ret

set_frequency:
    ; Sends frequency to PIT channel 2
    out 0x42, al
    mov al, ah            
    out 0x42, al          

    in al, 0x61           
    or al, 0x03         
    out 0x61, al          

    ret

play_correct_input_sound:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6          
    out 0x43, al        

    ; High pitch tone (~800 Hz)
    mov ax, 1428          ; Divider for ~800 Hz
    call set_frequency
    call delay1
    call delay1

    ; Medium pitch tone (~400 Hz)
    mov ax, 2387          ; Divider for ~400 Hz
    call set_frequency
    call delay1
    call delay1

    ; Low pitch tone (~200 Hz)
    mov ax, 4773          ; Divider for ~200 Hz
    call set_frequency
    call delay1
    call delay1

    
    in al, 0x61          
    or al, 0x03          
    out 0x61, al        


    ret

tapsound:
    ; Set up PIT for sound frequency
    mov al, 010110110b         ; Command byte for channel 2, mode 3 (square wave)
    out 0x43, al

    mov ax, 1193182 / 600       ; Load frequency divisor for ~600 Hz (typewriter sound)
    out 0x42, al                ; Send lower byte of divisor to PIT channel 2
    mov al, ah
    out 0x42, al                ; Send upper byte of divisor

    ; Enable PC speaker
    in al, 0x61
    or al, 00000011b            ; Set bits 0 and 1 to turn on speaker
    out 0x61, al

    ; Short delay to sustain the sound
    mov cx, 1000

delayloop:
    loop delayloop

    ; Disable PC speaker
    in al, 0x61
    and al, 11111100b           ; Clear bits 0 and 1 to turn off speaker
    out 0x61, al
    ret 

skip_timer_update:
    ; Restore registers and return from interrupt
    mov al, 0x20
    out 0x20, al
    pop di
    pop bx
    pop ax
    iret

timerHook:
    mov ax, 0
    mov es, ax

    ; Initialize time variables
    mov word [sec], 0
    mov word [min], 0
    mov word [hrs], 0
    mov word [ticks], 0
    mov word [lastUpdateTime], 0

    ; Hook the timer interrupt
    cli                             ; Disable interrupts
    mov word [es:8 * 4], onTimer    ; Set ISR address at vector 8
    mov [es:8 * 4 + 2], cs
    sti                             ; Enable interrupts
    ret
    
onTimer:
    push ax
    push bx
    push di
	
	
    add word [ticks], 1            ; Increment ticks
    add word [ms], ms_per_cycle    ; Add 55 ms to total milliseconds
    cmp word [ms], 1000            ; Check if 1000 ms (1 second) passed
    jl skip_sec                    ; If less than 1000 ms, continue

    mov word [ms], 0               ; Reset ms counter
    inc word [sec]                 ; Increment seconds
    cmp word [sec], 60
    jl skip_min

    mov word [sec], 0              ; Reset seconds
    inc word [min]                 ; Increment minutes
    cmp word [min], 60
    jl skip_hour

    mov word [min], 0              ; Reset minutes
    inc word [hrs]                 ; Increment hour
   
skip_hour:
    ;;

skip_min:
    ;;

skip_sec:

	call update_time_string
    ; Check game state
    cmp byte [gamestate], 1          ; 1 = In-game
    jne skip_timer_update            ; Skip timer updates if not in-game
	;je updation
    
    ; timer printing
    mov ax,36
    push ax
    mov ax,11
    push ax
    mov ax,7
    push ax
    mov ax,time
    push ax
    mov ax,11
    push ax
    mov ax,0xbb00
    mov es,ax
    call printstr
    
    ; score string printing
    mov ax,36
    push ax
    mov ax,13
    push ax
    mov ax,7
    push ax
    mov ax,scoreString
    push ax
    mov ax,6
    push ax
    mov ax,0xbb00
    mov es,ax
    call printstr

    ; score printing
    cmp word [score], 0
    jge print_pos       ; if score >= 0, print normally
    
    ; Handle negative score
    mov ax, 0xbb00
    mov es, ax          ; point es to video base
    
    ; Print minus sign first
    mov di, 2164        ; Your desired position
    mov byte [es:di], '-'
    mov byte [es:di+1], 0x07  ; attribute
    add di, 2           ; move position past minus sign
    
    ; Convert score to positive
    mov ax, [score]
    neg ax              ; make positive
    push 2166          ; position after minus sign
    push ax            ; push positive number
    jmp do_print

    print_pos:
        mov ax, 0xbb00
        mov es, ax          ; point es to video base
        push 2164
        push word [score]

    do_print:
        call printnum

        ; Restore registers and end interrupt
        mov al, 0x20                   ; End of interrupt command
        out 0x20, al                   ; Send EOI to the PIC
        pop di
        pop bx
        pop ax

        iret                           ; Return from interrupt

    
    update_time_string:
        ; Update seconds in time
        mov al, byte [sec]
        call byte_to_ascii
        mov [time + 9], ah        ; Tens place of seconds
        mov [time + 10], al       ; Units place of seconds

        ; Update minutes in time
        mov al, byte [min]
        call byte_to_ascii
        mov [time + 6], ah        ; Tens place of minutes
        mov [time + 7], al        ; Units place of minutes
        ret




; GENERIC HELPING FUNCTIONS
printstr: 
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    ; mov ax, 0xb900
    ; mov es, ax ; point es to video base
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

clrscr_page3:
    push es            ; Save ES register
    push ax            ; Save AX register
    push di            ; Save DI register

    mov ax, 0xb800     ; Set segment to video memory
    mov es, ax         ; ES = 0xB800 (video memory segment)
    mov di, 0x2000     ; Start at offset 0x1000 (beginning of page 1)

nextloc_page3:
    mov word [es:di], 0x0720  ; Store space character (0x20) with attribute (0x07)
    add di, 2                 ; Move to the next character position (2 bytes per character)
    cmp di, 0x2000            ; 0x2000 = 4000 in hex, end of page 1 memory
    jne nextloc_page3         ; If not done, keep clearing

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

toPage0Subroutine:
    mov word [currentPage], 0
    mov ax,0500h
    int 10h
    ret

toPage1Subroutine:
    mov word [currentPage], 1
    mov ax,0501h
    int 10h
    ret
    
toPage2Subroutine:
    mov word [currentPage], 2
    mov ax,0502h
    int 10h
    ret

toPage3Subroutine:
    mov word [currentPage], 3
    mov ax,0503h
    int 10h
    ret

printnum: 
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, [bp+4]      ; load number in ax
    mov di, [bp+6]      ; load position from stack
    
    mov bx, 10          ; use base 10 for division
    mov cx, 0           ; initialize count of digits

nextdigit: 
    mov dx, 0           ; zero upper half of dividend
    div bx              ; divide by 10
    add dl, 0x30        ; convert digit into ascii value
    push dx             ; save ascii value on stack
    inc cx              ; increment count of values
    cmp ax, 0           ; is the quotient zero
    jnz nextdigit       ; if no divide it again

nextpos: 
    pop dx              ; remove a digit from the stack
    mov dh, 0x07        ; use normal attribute
    mov [es:di], dx     ; print char on screen
    add di, 2           ; move to next screen location
    loop nextpos        ; repeat for all digits on stack

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 4               ; return and pop both parameters

byte_to_ascii:
    ;-------------------------------------------------------------------------------
    ; Converts a byte (0-99) in AL to two ASCII digits in AL and AH
    ;-------------------------------------------------------------------------------    
    mov ah, 0                        ; Clear AH for division
    mov bl, 10                       ; Divisor for decimal conversion
    div bl                           ; Divide AL by 10
    add al, '0'                      ; Convert quotient (tens) to ASCII
    xchg al, ah                      ; Move tens to AH
    add al, '0'                      ; Convert remainder (units) to ASCII
    ret

makeSomethingUnblink:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    mov al, 80  ; load al with columns per row
    mul byte [bp+10] ; multiply with y position
    add ax, [bp+12] ; add x position
    shl ax, 1 ; turn into byte offset
    mov di,ax ; point di to required location
    mov si, [bp+6] ; point si to string
    mov cx, [bp+4] ; load length of string in cx
    mov ax,[es:di]
    and ah,01101111b
    mov [es:di], ax
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 10 

makeSomethingBlink:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    mov al, 80  ; load al with columns per row
    mul byte [bp+10] ; multiply with y position
    add ax, [bp+12] ; add x position
    shl ax, 1 ; turn into byte offset
    mov di,ax ; point di to required location
    mov si, [bp+6] ; point si to string
    mov cx, [bp+4] ; load length of string in cx
    mov ax,[es:di]
    or ah,10010000b
    mov [es:di], ax
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 10 

checkSudoku:
    ; AX => Number to check
    ; BX => COL NUMBER
    ; DX => ROW NUMBER
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    mov word [isValid], 0         ; Assume invalid until proven otherwise

    ; Check if the number already exists in the column (excluding the current row)
    columnConfig:
        mov si, row1                ; Start with the first row (row1)
        mov cx, 9                     ; Loop through 9 rows
        shl bx, 1                     ; Multiply column number by 2 (since each element is a word)
        add si, bx                    ; Move to the column in row1
        shr bx, 1                     ; Restore column index after the shift

    checkCol:
        cmp ax, [si]                  ; Compare input with the value in this cell
        je incorrect                  ; If it matches, skip this cell (we're modifying it)
        add si, 20                    ; Move to the next row in the column (each row is 20 bytes)
        loop checkCol                 ; Continue looping through rows


    cmp byte [currentPage], 0
    je setRow
    cmp byte [currentPage], 2
    je setRowRemain

    setRow:
        cmp byte [cursorRow], 0
        je setRow1
        cmp byte [cursorRow], 1
        je setRow2
        cmp byte [cursorRow], 2
        je setRow3
        cmp byte [cursorRow], 3
        je setRow4
        cmp byte [cursorRow], 4
        je setRow5
        cmp byte [cursorRow], 5
        je setRow6
        jmp rowConfig

        
    setRowRemain:
        cmp byte [cursorRowRemain], 0
        je setRow7
        cmp byte [cursorRowRemain], 1
        je setRow8
        cmp byte [cursorRowRemain], 2
        je setRow9
        jmp skipCheck

    setRow1:
        mov si, row1
        jmp rowConfig
    setRow2:
        mov si, row2
        jmp rowConfig
    setRow3:
        mov si, row3
        jmp rowConfig
    setRow4:
        mov si, row4
        jmp rowConfig
    setRow5:
        mov si, row5
        jmp rowConfig
    setRow6:
        mov si, row6
        jmp rowConfig
    setRow7:
        mov si, row7
        jmp rowConfig
    setRow8:
        mov si, row8
        jmp rowConfig
    setRow9:
        mov si, row9
        jmp rowConfig

    rowConfig:
        mov cx,9

    checkRow:      
        cmp ax,[si]
        je incorrect
        add si,2
        loop checkRow

    mov byte [isValid],1 
    call play_correct_input_sound
    call turnoffspeakers       
    call incScore
    skipCheck:
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        ret

    incorrect:
        call decScore
        inc word [mistakes]
        cmp word [mistakes],5
        je mistakesOver
        call play_incorrect_input_sound
        jmp skipCheck

    mistakesOver:
        call toPage0Subroutine
        call play_incorrect_input_sound
        call wrapper_make_loosing_screen
        ret

gameWin:
    push si
    mov si,row1
    mov cx,90
    mov ax,' '
    mov byte [isWin],0
    checkWin:
        cmp ax,[si]
        je notWin
        add si,2
        loop checkWin
    
    mov byte [isWin],1
    returnWinCondition:
        pop si
        ret

    notWin:
        mov byte [isWin],0
        jmp returnWinCondition

wrapperGameWin:
    call gameWin
    cmp byte [isWin],1
    je matchWon
    ret

    matchWon:
        call wrapper_make_winning_screen
        jmp FINAL


; PRINTING GRID AND NUMBERS
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr            ; Call the printstr subroutine
        jmp nextColumn           ; Jump to handle the next column

    pushVerticalLine:
        push ax                  ; Push the attribute
        mov ax, verticalLine     ; Address of verticalLine
        push ax                  ; Push the string address
        push word [length]       ; Push the length of verticalLine
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr            ; Call the printstr subroutine

    nextColumn:
        inc bx                   ; Increment y position
        cmp bx, 25               ; Compare for the end of screen (25 rows)
        jle loopCol              ; If not yet, loop back to print next row

        add cx, 8                ; Increment x position (next column block)
        cmp cx, 81               ; Compare for the end of screen width (80 columns)
        jle printCols            ; If not yet, loop back to print next column block
        ret

printRemainingColsSubroutine:
    mov cx, 3
    printColsRemain:
        mov bx, 0
    loopColRemain:
        mov ax, cx
        push ax                 ; push x position
        mov ax, bx
        push ax                 ; push y position

        ; Check if cx is 27 or 51 for blue attribute and empty string, else use yellow
        cmp cx, 27
        je handleBlueEmptyStringRemain    ; If cx is 27, jump to handle blue and empty string
        cmp cx, 51
        je handleBlueEmptyStringRemain    ; If cx is 51, jump to handle blue and empty string

        ; Default case: use yellow attribute and verticalLine
    yellowAttributeRemain:
        mov ax, 0x0E             ; Yellow on black attribute
        jmp pushVerticalLineRemain     ; Jump to push verticalLine

    handleBlueEmptyStringRemain:
        mov ax, 0x70             ; Blue on black attribute
        push ax                  ; Push the attribute

        ; Push emptyString
        mov ax, emptyString      ; Address of emptyString
        push ax                  ; Push the string address
        push word [length]   ; Push the length of the empty string
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr            ; Call the printstr subroutine
        jmp nextColumnRemain           ; Jump to handle the next column

    pushVerticalLineRemain:
        push ax                  ; Push the attribute
        mov ax, verticalLine     ; Address of verticalLine
        push ax                  ; Push the string address
        push word [length]       ; Push the length of verticalLine
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr            ; Call the printstr subroutine

    nextColumnRemain:
        inc bx                   ; Increment y position
        cmp bx, 12               ; Compare for the end of screen (25 rows)
        jle loopColRemain              ; If not yet, loop back to print next row

        add cx, 8                ; Increment x position (next column block)
        cmp cx, 81               ; Compare for the end of screen width (80 columns)
        jle printColsRemain            ; If not yet, loop back to print next column block
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr           ; Call the printstr subroutine
        jmp nextRow             ; Jump to handle the next row

    pushHorizontalLine1:
        push ax                 ; Push the attribute
        mov ax, horizontalLine  ; Address of horizontalLine
        push ax                 ; Push the string address
        push word [length]      ; Push the length of horizontalLine
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr           ; Call the printstr subroutine

    nextRow:
        add cx, 1               ; Increment column position (cx)
        cmp cx, 75              ; Compare for the end of row width (75 is arbitrary, set for spacing)
        jle loopRow             ; If not yet, loop back to print next column

        add bx, 4              ; Increment y position (next row)
        cmp bx, 24              ; Compare for the end of screen (25 rows)
        jle printRows           ; If not yet, loop back to print next row
        ret

printRowsSubroutineRemain:
    mov bx, 0               ; Start at row 0
    printRowsRemain:
        mov cx, 3               ; Start at column 3
    loopRowRemain:
        mov ax, cx
        push ax                 ; Push x (column) position
        mov ax, bx
        push ax                 ; Push y (row) position

        ; Check if bx is 9 or 18 for blue attribute and empty string
        cmp bx, 9
        je handleBlueEmptyString1Remain  ; If bx is 9, jump to handle blue and empty string
        cmp bx, 18
        je handleBlueEmptyString1Remain  ; If bx is 18, jump to handle blue and empty string

        ; Default case: use yellow attribute and horizontalLine
    yellowAttribute1Remain:
        mov ax, 0x0E            ; Yellow on black attribute
        jmp pushHorizontalLine1Remain ; Jump to push horizontalLine

    handleBlueEmptyString1Remain:
        mov ax, 0x7F            ; Blue on black attribute
        push ax                 ; Push the attribute

        ; Push emptyString
        mov ax, emptyString      ; Address of emptyString
        push ax                 ; Push the string address
        push word [length]  ; Push the length of the empty string
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr           ; Call the printstr subroutine
        jmp nextRowRemain             ; Jump to handle the next row

    pushHorizontalLine1Remain:
        push ax                 ; Push the attribute
        mov ax, horizontalLine  ; Address of horizontalLine
        push ax                 ; Push the string address
        push word [length]      ; Push the length of horizontalLine
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr           ; Call the printstr subroutine

    nextRowRemain:
        add cx, 1               ; Increment column position (cx)
        cmp cx, 75              ; Compare for the end of row width (75 is arbitrary, set for spacing)
        jle loopRowRemain             ; If not yet, loop back to print next column

        add bx, 4              ; Increment y position (next row)
        cmp bx, 12              ; Compare for the end of screen (25 rows)
        jle printRowsRemain           ; If not yet, loop back to print next row
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,25
        jle colRight
    ret

printBorderRemain:
    mov cx,3
    rowTopRemain:
        mov ax, cx
        push ax ; push x position
        mov ax, 0
        push ax ; push y position
        mov ax, 0x40 
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,75
        jle rowTopRemain

    mov cx,3
    rowBottomRemain:
        mov ax, cx
        push ax ; push x position
        mov ax, 12
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,75
        jle rowBottomRemain
    
    mov cx,0
    colLeftRemain:
        mov ax, 3
        push ax ; push x position
        mov ax, cx
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,12
        jle colLeftRemain

    mov cx,0
    colRightRemain:
        mov ax, 75
        push ax ; push x position
        mov ax, cx
        push ax ; push y position
        mov ax, 0x40
        push ax ; push attribute
        mov ax, emptyString
        push ax ; push address of message
        push word [length] ; push message length
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr ; call the printstr subroutine
        inc cx
        cmp cx,12
        jle colRightRemain
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
        mov ax, 0xb800
        mov es, ax ; point es to video base
        call printstr    ; call the printstr subroutine
        add bx, 2
        add dx, 8
        cmp bx, 18
        jle printInnerLoop

        add cx, 4        ; Move to next row
        add word [rowAddress], 20  ; Move to next row's data
        cmp cx, 29
        jle printOuterLoop
        ret

printNumbersRemain:
    mov ax, row7
    mov [rowAddress], ax
    mov cx, 2        ; y pos
    printOuterLoopRemain:
        mov bx, 0        ; counter
        mov dx, 7        ; x pos
    printInnerLoopRemain:
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
        mov ax, 0xbA00
        mov es, ax ; point es to video base
        call printstr    ; call the printstr subroutine
        add bx, 2
        add dx, 8
        cmp bx, 18
        jle printInnerLoopRemain

        add cx, 4        ; Move to next row
        add word [rowAddress], 20  ; Move to next row's data
        cmp cx, 10
        jle printOuterLoopRemain
        ret




; WELCOME AND ENDING SCREEN
startingscreen:
    
    mov ax, 0B800h
    mov es, ax 
    mov di, 80 * 24 * 2         
    mov cx, 160                 

    fill_bottom_line:
        mov al, 0xDB                 ; Character to display
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
    ;    call play_correct_input_sound
    ;    call play_incorrect_input_sound
    ;    call play_on_rowncol_completion
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
    mov al, 010110110b  ; Channel 2, mode 3, binary mode
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
	call tapsound
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
	call tapsound
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
    mov byte [gamestate],0
	
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
		call tapsound
        call delay2_char
        call delay2_char
        loop print_thanks_char      

    done_thanks:
        pop di
        pop ax
        pop es
        ret



; CARDS
print_numbered_square:
    pusha
    ; Inputs for the position
    mov cx, [noofboxes]    ; Column
    mov dx, 2            ; Row (below the grid)

    ; Formula to calculate x and y
    mov ax, cx             ; col * 8
    shl ax, 3
    sub ax, 1              ; x = (col * 8) - 1
    sub ax, 2
    mov bx, ax             ; Store x in BX

    mov ax, dx             ; row * 4
    shl ax, 2
    sub ax, 2              ; y = (row * 4) - 2
    add ax, 1
    mov si, ax             ; Store y in SI

    ; Calculate video memory offset: offset = y * 160 + x * 2
    mov ax, si             ; y
    imul ax, 160           ; y * 160 (each row is 160 bytes in text mode)
    add ax, bx             ; Add x (column offset)
    shl ax, 1              ; Multiply by 2 (each character uses 2 bytes)
    mov di, ax             ; Store offset in DI

    ; Set ES to video memory segment
    mov ax, 0xBA00
    mov es, ax

    ; Draw the square (4x8 block)
    mov cx, 4              ; 4 rows for the square
    draw_row:
        push cx                ; Save loop counter

        mov cx, 5              ; 8 columns for the square
    draw_col:
        mov al, 0xDB           ; Character to draw
        mov ah, 0x0F           ; Attribute (color)
        stosw                  ; Write character and attribute to video memory
        loop draw_col          ; Repeat for 8 columns

        add di, 160 - 10       ; Move to the next row (subtract 16 for 8 columns already written)
        pop cx                 ; Restore loop counter
        loop draw_row          ; Repeat for 4 rows

        ; Draw the number in the center of the box
        ; Calculate center position (middle row and column of the box)
        mov ax, [noofboxes]    ; Get the current box number
        add ax, '0'            ; Convert to ASCII
        sub di, 480           ; Move back to the start of the box
        add di,  160+4    ; Move to the center of the box
        ;mov al, [ax]           ; Store the ASCII number
        mov bx,ax
        mov al,bl
        mov ah, 0x0C           ; Attribute (color)
        stosw                  ; Write the number to the center

        ; Update box number
        add word [noofboxes], 1

        popa
        ret
    print_numbers_bottom:
        pusha
        mov cx, 9              ; Loop for 9 boxes
        mov si, countofnumbers ; Address of the counts array
        mov bx, 1              ; Start with the first box

    draw_bottom_loop:
        ; Set ES to video memory segment
        mov ax, 0xBA00
        mov es, ax

        ; Calculate position below the box
        mov dx, bx             ; Current box number (1-based)
        shl dx, 3              ; dx *= 8 (column width)
        sub dx, 2
        shl dx, 1              ; dx *= 2 (character offset)
        mov di, dx             ; Move DI to the start of the box
        add di, 160*19 + 2    ; Move DI below the box (adjust for row below boxes)

        ; Get the count for the current box
        lodsw                  ; Load the next count into AX

        ; Convert count to ASCII
        add al, '0'
        mov ah, 0x04           ; Attribute (color)
        stosw                  ; Print the count

        ; Increment the box index
        inc bx
        loop draw_bottom_loop  ; Repeat for all 9 boxes

        popa
        ret
    wrapper_make_number_boxes:
        mov word [noofboxes], 1 ; Reset to the first box before starting
        mov cx, 9              ; Loop for 9 boxes
    loop_boxes:
        call print_numbered_square
        loop loop_boxes
        call print_numbers_bottom
        ret

countNumbersInGrid:
    ; DX => Base address of the grid (9x9, each element is a word)
    ; The count array is at `countofnumbers`

    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; First clear all counts
    mov di, countofnumbers
    mov cx, 9
    clearCounts:
        mov word [di], 9
        add di, 2
        loop clearCounts

    ; Now count numbers
    mov di,countofnumbers
    mov si, dx                    ; SI points to start of grid
    mov cx, 90                    ; Total cells to check

    countLoop:
        mov ax, word [si]            ; Get current cell value
        cmp ax, ' '                  ; Skip if space
        je nextCell
        
        ; Convert number to array index and increment
        sub ax, '1'                  ; Convert '1'-'9' to 0-8
        shl ax, 1                    ; Multiply by 2 for word offset
        push di
        mov di, countofnumbers
        add di, ax                   ; Point to correct counter
        dec word [di]                ; Increment that number's count
        pop di

    nextCell:
        add si, 2                    ; Move to next cell
        loop countLoop

        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        ret

wrapper_count_numbers_in_grid:
    mov dx,row1
    call countNumbersInGrid
    ret



; SCORE AND TIMER PRINTING
timerPrintSubroutine:
    ; call clrscr_page3
    mov ax,36
    push ax
    mov ax,11
    push ax
    mov ax,7
    push ax
    mov ax,time
    push ax
    mov ax,11
    push ax
    mov ax,0xbb00
    mov es,ax
    call printstr

scorePrintSubroutine:
    mov ax,38
    push ax
    mov ax,12
    push ax
    mov ax,7
    push ax
    mov ax,scoreString
    push ax
    mov ax,8
    push ax
    mov ax,0xbb00
    mov es,ax
    call printstr
    ret

incScore:
    add word [score],200
    ret

decScore:
    sub word [score],200
    ret


; UNDO
saveCurrentState:
    push ax
    push si
    push di

    mov ax,[cursorCol]
    mov [prevCursorCol],ax
    mov ax,[cursorColRemain]
    mov [prevCursorColRemain],ax
    mov ax,[cursorRow]
    mov [prevCursorRow],ax
    mov ax,[cursorRowRemain]
    mov [prevCursorRowRemain],ax
    mov ax,[currentPage]
    mov [prevPage],ax  
    
    mov si, countofnumbers    ; Use SI as source
    mov di, prevCardsRemaining ; Use DI as destination
    mov cx,9
    saveArray:
        mov ax,[si]        ; Load from source
        mov [di],ax       ; Store to destination
        add si,2          ; Advance source pointer
        add di,2          ; Advance destination pointer
        loop saveArray

    pop di
    pop si
    pop ax
    ret

undoMove:
    push ax
    push bx
    push cx
    push si
    push di

    cmp byte [prevPage],0
    je undoOnMainGrid
    cmp byte [prevPage],2
    je undoOnRemainGrid
    jmp endUndo

    undoOnMainGrid:
        mov ax,[prevCursorRow]
        cmp ax,0
        je setRow1ForUndo
        cmp ax,1
        je setRow2ForUndo
        cmp ax,2
        je setRow3ForUndo
        cmp ax,3
        je setRow4ForUndo
        cmp ax,4
        je setRow5ForUndo
        cmp ax,5
        je setRow6ForUndo
    undoOnRemainGrid:
        mov ax,[prevCursorRowRemain]
        cmp ax,0
        je setRow7ForUndo
        cmp ax,1
        je setRow8ForUndo
        cmp ax,2
        je setRow9ForUndo
    jmp endUndo

    setRow1ForUndo:
        mov si,row1
        jmp doFurtherForPage0
    setRow2ForUndo:
        mov si,row2
        jmp doFurtherForPage0
    setRow3ForUndo:
        mov si,row3
        jmp doFurtherForPage0
    setRow4ForUndo:
        mov si,row4
        jmp doFurtherForPage0
    setRow5ForUndo:
        mov si,row5
        jmp doFurtherForPage0
    setRow6ForUndo:
        mov si,row6
        jmp doFurtherForPage0
    setRow7ForUndo:
        mov si,row7
        jmp doFurtherForPage2
    setRow8ForUndo:
        mov si,row8
        jmp doFurtherForPage2
    setRow9ForUndo:
        mov si,row9
        jmp doFurtherForPage2

    doFurtherForPage0:
            mov ax,[prevCursorCol]
            shl ax,1
            add si,ax
            mov word [si],' '
            jmp reset
    doFurtherForPage2:
            mov ax,[prevCursorColRemain]
            shl ax,1
            add si,ax
            mov word [si],' '

    reset:
        mov ax,[prevCursorCol]
        mov [cursorCol],ax
        mov ax,[prevCursorRow]
        mov [cursorRow],ax
        mov ax,[prevCursorRowRemain]
        mov [cursorRowRemain],ax
        mov ax,[prevCursorColRemain]
        mov [cursorColRemain],ax
        mov ax,[prevPage]
        mov [currentPage],ax
        call decScore


        mov di, countofnumbers    ; Use SI as source
        mov si, prevCardsRemaining ; Use DI as destination
        mov cx,9
        saveArray1:
            mov ax,[si]        ; Load from source
            mov [di],ax       ; Store to destination
            add si,2          ; Advance source pointer
            add di,2          ; Advance destination pointer
            loop saveArray1

        

    endUndo:
        pop di
        pop si
        pop cx
        pop bx
        pop ax
        ret


; INPUT HANDELING
askForInput:
    xor ax,ax
    int 16h

    ; CHANGE PAGE KEYS
    cmp ah, 0x4D    ; Right arrow key
    je toRemainGrid
    cmp ah, 0x4B    ; Left arrow key
    je toMainGrid
    cmp ah, 0x0F    ; Tab key
    je toScorePage
    cmp ah, 0x01    ; Escape key
    je toEndGame
    cmp ah, 0x16    ; U key scan code
    je undoLastMove



    ; SETTING si TO THE SPECIFIED ROW

    tempNext1:
        cmp byte [currentPage], 0
        je saveSIPage0
        cmp byte [currentPage], 2
        je saveSIPage2

    ; SAVING si FOR MAIN GRID
    saveSIPage0:
        cmp byte [cursorRow],0
        je saveRow1
        cmp byte [cursorRow],1
        je saveRow2
        cmp byte [cursorRow],2
        je saveRow3
        cmp byte [cursorRow],3
        je saveRow4
        cmp byte [cursorRow],4
        je saveRow5
        cmp byte [cursorRow],5
        je saveRow6

    ; SAVING si FOR REMAINING GRID
    saveSIPage2:
        cmp byte [cursorRowRemain],0
        je saveRow7
        cmp byte [cursorRowRemain],1
        je saveRow8
        cmp byte [cursorRowRemain],2
        je saveRow9

    tempNext:
    cmp byte [currentPage], 0
    je gotoSimpleInput
    cmp byte [currentPage], 2
    je gotoRemainInput
    jmp inputCheckDone    ; Skip input checks if not on valid page

    gotoSimpleInput:
        cmp ah,0x02     ; '1' Key
        je toInput1
        cmp ah,0x03     ; '2' Key
        je toInput2
        cmp ah,0x04     ; '3' Key
        je toInput3
        cmp ah,0x05     ; '4' Key
        je toInput4
        cmp ah,0x06     ; '5' Key
        je toInput5
        cmp ah,0x07     ; '6' Key
        je toInput6
        cmp ah,0x08     ; '7' Key    
        je toInput7
        cmp ah,0x09     ; '8' Key
        je toInput8
        cmp ah,0x0A     ; '9' Key
        je toInput9
        jmp inputCheckDone    ; Continue to movement checks if no number pressed

    gotoRemainInput:
        cmp ah,0x02     ; '1' Key
        je toInput1Remain
        cmp ah,0x03     ; '2' Key
        je toInput2Remain
        cmp ah,0x04     ; '3' Key
        je toInput3Remain
        cmp ah,0x05     ; '4' Key
        je toInput4Remain
        cmp ah,0x06     ; '5' Key
        je toInput5Remain
        cmp ah,0x07     ; '6' Key
        je toInput6Remain
        cmp ah,0x08     ; '7' Key    
        je toInput7Remain
        cmp ah,0x09     ; '8' Key
        je toInput8Remain
        cmp ah,0x0A     ; '9' Key
        je toInput9Remain
        jmp inputCheckDone

    inputCheckDone:     ; New label to continue with movement checks
        cmp byte [currentPage], 0
        je mainGridMovement
        cmp byte [currentPage], 2
        je remainGridMovement

    ; CURSOR KEYS
    mainGridMovement:
        cmp ah, 0x11    ; W key
        je moveCursorUp
        cmp ah, 0x1F    ; S key
        je moveCursorDown
        cmp ah, 0x1E    ; A key
        je moveCursorLeft
        cmp ah, 0x20    ; D key
        je moveCursorRight
        jmp askForInputReturn

    remainGridMovement:
        cmp ah, 0x11    ; W key
        je moveCursorUpRemain
        cmp ah, 0x1F    ; S key
        je moveCursorDownRemain
        cmp ah, 0x1E    ; A key
        je moveCursorLeftRemain
        cmp ah, 0x20    ; D key
        je moveCursorRightRemain
        jmp askForInputReturn

    toMainGrid:
        call toPage0Subroutine
        jmp askForInputReturn

    toRemainGrid:
        call toPage2Subroutine
        jmp askForInputReturn

    toScorePage:
        call toPage3Subroutine
        jmp askForInputReturn

    moveCursorUp:
        call moveCursorUpSubroutine
        jmp askForInputReturn

    moveCursorDown:
        call moveCursorDownSubroutine
        jmp askForInputReturn

    moveCursorLeft:
        call moveCursorLeftSubroutine
        jmp askForInputReturn

    moveCursorRight:
        call moveCursorRightSubroutine
        jmp askForInputReturn
        
    moveCursorUpRemain:
        call moveCursorUpSubroutineRemain
        jmp askForInputReturn

    moveCursorDownRemain:
        call moveCursorDownSubroutineRemain
        jmp askForInputReturn

    moveCursorLeftRemain:
        call moveCursorLeftSubroutineRemain
        jmp askForInputReturn

    moveCursorRightRemain:
        call moveCursorRightSubroutineRemain
        jmp askForInputReturn

    undoLastMove:
        call undoMove
        ret
    
    ; toInput1:
    ;     mov bx,[cursorCol]
    ;     shl bx,1
    ;     cmp word [si+bx],' '  ; Check if current position contains a space
    ;     jne short_ret         ; If not a space, return without inputting
    ;     mov ax,'1'
    ;     mov bx,[cursorCol]
    ;     mov dx,[cursorRow]
    ;     call checkSudoku
    ;     cmp word [isValid],0
    ;     je short_ret
    ;     mov word [si+bx],'1'
    ;     ret

    toInput1:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '1'                  ; Prepare input '1'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput1                ; If valid, place the input
        ret

        placeInput1:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '1'
            ret
    




    toInput2:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '2'                  ; Prepare input '1'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput2               ; If valid, place the input
        ret

        placeInput2:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '2'
            ret

    toInput3:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '3'                  ; Prepare input '3'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput3              ; If valid, place the input
        ret

        placeInput3:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '3'
            ret

    toInput4:
        ; mov bx,[cursorCol]
        ; shl bx,1
        ; cmp word [si+bx],' '  ; Check if current position contains a space
        ; jne short_ret         ; If not a space, return without inputting
        ; mov word [si+bx],'4'
        ; ret
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '4'                  ; Prepare input '4'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput4               ; If valid, place the input
        ret

        placeInput4:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '4'
            ret

    toInput5:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '5'                  ; Prepare input '5'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput5              ; If valid, place the input
        ret

        placeInput5:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '5'
            ret

    toInput6:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '6'                  ; Prepare input '6'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput6               ; If valid, place the input
        ret

        placeInput6:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '6'
            ret

    toInput7:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '7'                  ; Prepare input '7'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput7               ; If valid, place the input
        ret

        placeInput7:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '7'
            ret

    toInput8:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '8'                  ; Prepare input '8'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput8               ; If valid, place the input
        ret

        placeInput8:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '8'
            ret

    toInput9:
        mov bx, [cursorCol]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '9'                  ; Prepare input '9'
        mov bx, [cursorCol]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRow]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move

        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput9              ; If valid, place the input
        ret

        placeInput9:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '9'
            ret
        
        
    toInput1Remain:
        ; mov bx,[cursorColRemain]
        ; shl bx,1
        ; cmp word [si+bx],' '
        ; jne short_ret
        ; mov word [si+bx],'1'
        ; ret
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '1'                  ; Prepare input '1'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput1Remain                ; If valid, place the input
        ret

        placeInput1Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '1'
            ret

    toInput2Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '2'                  ; Prepare input '2'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput2Remain                ; If valid, place the input
        ret

        placeInput2Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '2'
            ret

    toInput3Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '3'                  ; Prepare input '3'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput3Remain                ; If valid, place the input
        ret

        placeInput3Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '3'
            ret

    toInput4Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '4'                  ; Prepare input '4'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput4Remain                ; If valid, place the input
        ret

        placeInput4Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '4'
            ret

    toInput5Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '5'                  ; Prepare input '5'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput5Remain                ; If valid, place the input
        ret

        placeInput5Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '5'
            ret

    toInput6Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '6'                  ; Prepare input '6'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput6Remain                ; If valid, place the input
        ret

        placeInput6Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '6'
            ret

    toInput7Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '7'                  ; Prepare input '7'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput7Remain                ; If valid, place the input
        ret

        placeInput7Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '7'
            ret

    toInput8Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '8'                  ; Prepare input '8'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput8Remain                ; If valid, place the input
        ret

        placeInput8Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '8'
            ret

    toInput9Remain:
        mov bx, [cursorColRemain]         ; Get the column index
        shl bx,1
        cmp word [si + bx], ' '      ; Check if the current position contains a space
        jne short_ret                ; If not a space, return without inputting

        mov ax, '9'                  ; Prepare input '9'
        mov bx, [cursorColRemain]          ; Get column index again (no need for shl bx, 1 here)
        mov dx, [cursorRowRemain]          ; Get the row index
        call checkSudoku             ; Check if it's a valid move
        cmp word [isValid], 1        ; Check if the move is valid (1 means valid)
        je placeInput9Remain                ; If valid, place the input
        ret

        placeInput9Remain:
            call saveCurrentState
            shl bx,1
            mov word [si + bx], '9'
            ret

    short_ret:
        ret
        

    saveRow1:
        mov si,row1
        jmp tempNext
    
    saveRow2:
        mov si,row2
        jmp tempNext
    
    saveRow3:
        mov si,row3
        jmp tempNext
    
    saveRow4:
        mov si,row4
        jmp tempNext

    saveRow5:
        mov si,row5
        jmp tempNext
    
    saveRow6:
        mov si,row6
        jmp tempNext

    saveRow7:
        mov si,row7
        jmp tempNext

    saveRow8:
        mov si,row8
        jmp tempNext

    saveRow9:
        mov si,row9
        jmp tempNext
        


    toEndGame:
        call toPage0Subroutine
        jmp FINAL

    askForInputReturn:
        jmp askForInput

moveCursorUpSubroutine:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorRow]
    cmp ax,0
    je moveCursorUpSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    mov [cursorRow],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorRow]
    cmp ax,7
    jge moveCursorUpSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    dec ax                              ; Increment instead of decrement for moving down
    mov [cursorRow],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingBlink

    moveCursorUpSubroutineReturn:
        ret
    
moveCursorDownSubroutine:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorRow]
    cmp ax,5
    jge moveCursorDownSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    mov [cursorRow],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorRow]
    cmp ax,7
    jge moveCursorDownSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    inc ax                              ; Increment instead of decrement for moving down
    mov [cursorRow],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingBlink

    moveCursorDownSubroutineReturn:
        ret

moveCursorLeftSubroutine:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorCol]
    cmp ax,0
    je moveCursorLeftSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    mov [cursorCol],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorCol]
    cmp ax,0
    je moveCursorLeftSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    dec ax                      ; Increment instead of decrement for moving down
    mov [cursorCol],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingBlink

    moveCursorLeftSubroutineReturn:
        ret

moveCursorRightSubroutine:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorRow]
    cmp ax,8
    je moveCursorRightSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    mov [cursorRow],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorCol]
    cmp ax,8
    jge moveCursorRightSubroutineReturn  ; Change jmp to jge (jump if greater or equal)
    inc ax                              ; Increment instead of decrement for moving down
    mov [cursorCol],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRow]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorCol]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xb800
    mov es,ax
    call makeSomethingBlink

    moveCursorRightSubroutineReturn:
        ret

moveCursorUpSubroutineRemain:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorRowRemain]
    cmp ax,0
    je moveCursorUpSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    mov [cursorRowRemain],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorRowRemain]
    cmp ax,7
    jge moveCursorUpSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    dec ax                              ; Increment instead of decrement for moving down
    mov [cursorRowRemain],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingBlink

    moveCursorUpSubroutineReturnRemain:
        ret
    
moveCursorDownSubroutineRemain:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorRowRemain]
    cmp ax,2
    jge moveCursorDownSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    mov [cursorRowRemain],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorRowRemain]
    cmp ax,2
    jge moveCursorDownSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    inc ax                              ; Increment instead of decrement for moving down
    mov [cursorRowRemain],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingBlink

    moveCursorDownSubroutineReturnRemain:
        ret

moveCursorLeftSubroutineRemain:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorColRemain]
    cmp ax,0
    je moveCursorLeftSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    mov [cursorColRemain],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorColRemain]
    cmp ax,0
    je moveCursorLeftSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    dec ax                      ; Increment instead of decrement for moving down
    mov [cursorColRemain],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingBlink

    moveCursorLeftSubroutineReturnRemain:
        ret

moveCursorRightSubroutineRemain:
    ; CALCULATING OLD ROW AND COL
    mov ax,[cursorColRemain]
    cmp ax,8
    je moveCursorRightSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    mov [cursorColRemain],ax
    
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    mov ax,[cursorColRemain]
    cmp ax,8
    jge moveCursorRightSubroutineReturnRemain  ; Change jmp to jge (jump if greater or equal)
    inc ax                              ; Increment instead of decrement for moving down
    mov [cursorColRemain],ax

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingUnblink

    ; CALCULATING NEW ROW AND COL
    xor ax,ax
    mov al,4
    mov bl,[cursorRowRemain]
    mul bl
    add ax,2                ; Changed from sub to add to position correctly
    mov cx,ax               ; row

    xor ax,ax
    mov al,8
    mov bl,[cursorColRemain]
    mul bl
    add ax,7                ; Changed from sub to add, adjusted position
    mov dx,ax               ; col

    push dx                 ; x position
    push cx                 ; y position
    mov ax,0x0F             ; attribute (white on black)
    push ax
    mov ax,emptyString
    push ax
    push word [length]
    mov ax,0xbA00
    mov es,ax
    call makeSomethingBlink

    moveCursorRightSubroutineReturnRemain:
        ret

waitForKey:
    mov ah,0
    int 0x16
    ret



; MAIN GAME FUNCTION
gameSubroutine:
    ; WELCOME SCREEN
    WELCOME:
        ; call clrscr             
        ; call startingscreen    
        ; call clrscrwithdelay
        ; call turnoffspeakers
        ; call display_message_effect  
        ; call display_continue_msg 
        ; call waitForKey

    ; GAME CONFIGURATIONS
    SETTINGS:
        call clrscr 
        call settingVideoModeSubroutine
        call blackBackgroundSubroutine
        call clrscr_page3
        call timerHook


    ; GRID PRINTING
    GAMEBOARD:
        call clrscr_page3
        call printRowsSubroutine
        call printRowsSubroutineRemain
        call printColsSubroutine
        call printRemainingColsSubroutine
        call printBorder
        call printBorderRemain
        

    ; NUMBERS
    NUMBERS:
        call wrapperGameWin
        call printNumbers
        call printNumbersRemain
        call wrapper_count_numbers_in_grid
        call wrapper_make_number_boxes

    ; MECHANICS
    MECHANICS:
        call askForInput
        jmp NUMBERS

    ; END SCREEN
    FINAL:
        call clrscr
        call display_thanks_message_effect
        call end

start:
    call gameSubroutine
    jmp end

end:
    mov ax, 0x4C00
    int 21h
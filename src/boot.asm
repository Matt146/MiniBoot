; NASM directives:
; org  - tells where the program is supposed to be loaded in memory
;	 in this case, 0x7c00 is where the BIOS leaves us
; bits - encodes instructions for 16 bits
[org 0x7c00]
[bits 16]

; A20 LINE EXPLAINED:
; So, I'm gonna try to wrap my 4 IQ head around this: if you have the a20 line disabled, the 21st bit in the physical address is always gonna be set to 0 basically, in order for the wraparound feature to work?
; The reason why they needed this to work like this was bc the intel 8086 had the wraparound feature at 1 mb, but the 286 could address 16 mb now in protected mode, and to keep compatibility with the wraparound feature, they disabled the a20 line.

; Global vars
KERNEL_OFFSET equ 0x8c00

; Set up segment registers by setting all of them, 
; except for cs, to 0
mov ax, 0x0000	; Set ax to 0x0000 because that's what the value of our segment regs will be set to
mov ds, ax	; Setup data segment
mov es, ax	; Setup extra segment
mov ss, ax	; Setup stack segment
mov sp, 0xAAAA	; Setup stack pointer
mov bp, sp	; Setup base pointer to keep track of stack. If you're reading this, I did
		; it to do nested function calls and not have to worry about the stack.
		; Much safer to have a baser pointer here.

call load_kernel	     ; Load the kernel using BIOS functions
call enable_a20_main	     ; Enable the A20 line
jmp switch_to_protected_mode ; Now, we go to protected mode

; ===========================================
;	    KERNEL LOADING FUNCTIONS
; ===========================================
load_kernel:
    call disk_load
    ret

disk_load:
    mov ah, 0x42
    mov si, DAP
    int 0x13
    jc disk_error
    ret

DAP:
    db 0x10
    db 0x0
    dw 15
    dd KERNEL_OFFSET
    dq 1

disk_error:
    mov ah, 0x0e
    mov al,'F'
    int 0x10
    jmp $

; ===========================================
;	FUNCTIONS TO ENABLE A20 GATE
; ===========================================
enable_a20_main:
    push bp
    mov bp, sp

    ; Try the keyboard controller method
    call enable_a20_keyboard
    call check_a20
    cmp ax, 1
    je enable_a20_main_end

    ; Try the bios method
    call enable_a20_bios
    call check_a20
    cmp ax, 1
    je enable_a20_main_end

    ; Try the fast a20 method
    call enable_a20_fast
    call check_a20
    cmp ax, 1
    je enable_a20_main_end

    ; If none of these, work just halt and print "B"
    jmp enable_a20_main_fail

enable_a20_main_end:
    mov sp, bp
    pop bp
    ret

enable_a20_main_fail:
   mov ah, 0x0e 
   mov al, 'B'
   int 0x10
   cli
   hlt
   jmp $

; The keybaord control method to enable 
; the a20 gate
enable_a20_keyboard:
    push bp
    mov bp, sp

    mov al, 0xdd
    out 0x64, al

    mov sp, bp
    pop bp
    ret

; The BIOS method to enable the a20 gate
enable_a20_bios:
    push bp
    mov bp, sp

    mov ax, 0x2401
    int 0x15

    mov sp, bp
    pop bp
    ret

; Fast a20 method - fast, but dangerous ;)
enable_a20_fast:
    push bp
    mov bp, sp
    
    mov al, 2
    out 0x92, al

    mov sp, bp
    pop bp
    ret


; Ez way of checking a20. Really helpful
check_a20:
    pushf
    push ds
    push es
    push di
    push si
 
    cli
 
    xor ax, ax ; ax = 0
    mov es, ax
 
    not ax ; ax = 0xFFFF
    mov ds, ax
 
    mov di, 0x0500
    mov si, 0x0510
 
    mov al, byte [es:di]
    push ax
 
    mov al, byte [ds:si]
    push ax
 
    mov byte [es:di], 0x00
    mov byte [ds:si], 0xFF
 
    cmp byte [es:di], 0xFF
 
    pop ax
    mov byte [ds:si], al
 
    pop ax
    mov byte [es:di], al
 
    mov ax, 0
    je check_a20__exit
 
    mov ax, 1
 
check_a20__exit:
    pop si
    pop di
    pop es
    pop ds
    popf
 
    ret

; ==========================================
;		SETUP THE GDT
; ==========================================
gdt_start:

gdt_null:
    dd 0x0
    dd 0x0

gdt_code:
    ; base = 0x0, limit=0xfffff
    ; 1st  flags: (present )1 (privilege )00 (descriptor  type)1 -> 1001b
    ; type  flags: (code)1 (conforming )0 (readable )1 (accessed )0 -> 1010b
    ; 2nd  flags: (granularity )1 (32-bit  default )1 (64-bit  seg)0 (AVL)0 -> 1100b
    dw 0xffff    ; Limit (bits 0-15)
    dw 0x0       ; Base (bits 0-15)
    db 0x0       ; Base (bits 16-23)
    db 10011010b ; 1st flags , type  flags
    db 11001111b ; 2nd flags , Limit (bits  16-19)
    db 0x0       ; Base (bits  24 -31)

gdt_data: ;the  data  segment  descriptor
    ; Same as code  segment  except  for  the  type  flags
    ; type  flags: (code)0 (expand  down)0 (writable )1 (accessed )0 -> 0010b
    dw 0xffff      ; Limit (bits  0-15)
    dw 0x0         ; Base (bits  0-15)
    db 0x0         ; Base (bits  16 -23)
    db 10010010b   ; 1st flags , type  flags
    db 11001111b   ; 2nd flags , Limit (bits  16-19)
    db 0x0         ; Base (bits  24 -31)

gdt_end:           ; The  reason  for  putting a label  at the  end of the
                   ; GDT is so we can  have  the  assembler  calculate
                   ; the  size of the  GDT  for  the GDT  decriptor (below)

; GDT descriptor
gdt_descriptor:
    dw  gdt_end  - gdt_start  - 1	; 16 bits for size of GDT
    dd  gdt_start			; 32 bits for where the GDT starts

; Define  some  handy  constants  for  the  GDT  segment  descriptor  offsets , which
; are  what  segment  registers  must  contain  when in  protected  mode.  For  example ,
; when we set DS = 0x10 in PM , the  CPU  knows  that we mean it to use  the
; segment  described  at  offset 0x10 (i.e. 16  bytes) in our GDT , which in our
; case is the  DATA  segment  (0x0 -> NULL; 0x08  -> CODE; 0x10  -> DATA)
CODE_SEG  equ  gdt_code  - gdt_start
DATA_SEG  equ  gdt_data  - gdt_start

; ==========================================
;	SWITCH TO PROTECTED MODE
; ==========================================
switch_to_protected_mode:
    ; Disable interrutps for rn, so that
    ; we don't screw up switching to pmode
    cli     
    
    ; Tell the CPU about the GDT we just prepared
    lgdt [gdt_descriptor]

    ; Set cr0's first bit
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    ; Far jump to set CS
    jmp CODE_SEG:init_pm

[bits 32]
init_pm:
    ; For flat memory model
    mov ax, DATA_SEG	; put the offset of the data segment descriptor in the GDT into ax
    mov ds, ax		; now put ax into ds - can't set ds directly
    mov ss, ax		; set stack segment to ax
    mov es, ax		; set extra segment to ax
    mov fs, ax		; set fs to ax
    mov gs, ax		; set gs to ax

    ; now set the base pointer and the stack pointer
    ; to set up the stack
    mov ebp, 0x90000
    mov esp, ebp

    sti ; re-enable interrupts

    ; as of now, we have completely set up protected mode
    ; jump to kernel code
    jmp KERNEL_OFFSET

times 510-($-$$) db 0
dw 0xaa55

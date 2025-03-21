
.code

INC_REG_PC MACRO mem

ADD_REG_PC mem, 1

ENDM

ADD_REG_PC MACRO mem, value

push x_ax	
mov x_ax, reg_pc
sub x_ax, mem
add ax, value
mov reg_pc, mem
add reg_pc, x_ax
pop x_ax

ENDM

CopyToMaskedRegOrNextOpcode MACRO msk, value8

	invoke DCB_REGISTER_FROM_MASK, msk	
	test x_ax, x_ax	
	jz Z80Loop
	mov cl, value8
	mov [x_ax], cl
ENDM

ProcesarOpcodeFromRom MACRO JumpTable	
		
	push x_bp
	xor x_bp, x_bp
	xor x_ax, x_ax	
	mov al, [reg_pc]             ; Select index

	;invoke printOpcode, reg_pc

	mov x_bp, OFFSET JumpTable ; Load the base address of the jump table into RBP
	mov x_bx, [x_bp + x_ax*PTR_SIZE]    ; Load the function address from the table
	INC_REG_PC memPtr
	pop x_bp
	jmp x_bx

ENDM

IncRegR MACRO 

mov al,RegR
mov ah,al
and al,80h
inc ah
and ah,7fh
or al,ah
mov RegR,al
	
ENDM

GetBitMask MACRO dest, args:VARARG

	LOCAL a
	push Reg_Tmp
	push x_ax
	mov Reg_Tmp, 0
    FOR a, <args>
		
		mov x_ax, 1		
		shl x_ax, a
		; OF --> PVF
		and ah, 08h
		shr ah, 1
		or al, ah
		xor ah, ah
		or Reg_Tmp, x_ax
	ENDM
	pop x_ax
	mov dest, Reg_Tmp
	pop Reg_Tmp
ENDM

GetBitMaskNeg MACRO dest, args:VARARG

	GetBitMask dest, args
	not dest

ENDM

ResetFlagBits MACRO args:VARARG

push x_cx
GetBitMaskNeg x_cx, args
and RegF, cl
pop x_cx

ENDM

SetFlagBits MACRO args:VARARG

push x_cx
GetBitMask x_cx, args
or RegF, cl
pop x_cx

ENDM

X86FlagsToZ80Flags MACRO args:VARARG
	
	LOCAL OF_SLICE
	OF_SLICE = OF - PVF

	push x_bx
	push x_ax
	pushf
	ResetFlagBits args
	pop ax	
	xor bx, bx
	FOR a, <args>
		
		IF a EQ OF
			mov bl, 1
			shl bl, PVF
			not bl
			and al, bl
			mov bx, ax
			and bx, 0800h
			shr bx, OF_SLICE
			or RegF, bl
		ENDIF
	
	ENDM
	GetBitMask x_bx, args
	and al, bl
	or RegF, al
	pop x_ax
	pop x_bx

ENDM

SetPVFromBC MACRO

ResetFlagBits PVF
mov ax, RegBC
test ax, ax
lahf
and ah, 040h
xor ah, 040h
shr ah, ZF-PVF
or RegF, ah

ENDM

SetIOFlags MACRO value

push x_ax
mov al, value
test al, al
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
pop x_ax

ENDM

LoadZ80CarryFlag MACRO

push x_ax
push x_bx
GetBitMaskNeg x_bx, CF
lahf
and ah, bl
not bl
mov bh, RegF
and bh, bl
or ah, bh
sahf
pop x_bx
pop x_ax

ENDM

TestZ80Flag MACRO flag

push x_cx
GetBitMask x_cx, flag
mov ch, RegF
and ch, cl
test ch, ch
pop x_cx

ENDM

SetTestRegValue64 MACRO reg

xor rax, rax
mov ax, WORD PTR [rcx]
mov reg, rax
add rcx, 2

ENDM

SetTestRegValue16 MACRO reg

mov ax, WORD PTR [rcx]
mov reg, ax
add rcx, 2

ENDM

SetTestRegValue8 MACRO reg

mov ax, WORD PTR [rcx]
mov reg, al
add rcx, 2

ENDM

GetTestRegValue64 MACRO reg

mov rax, reg
mov WORD PTR [rcx], ax
add rcx, 2

ENDM

GetTestRegValue16 MACRO reg

xor rax, rax
mov ax, reg
mov WORD PTR [rcx], ax
add rcx, 2

ENDM

GetTestRegValue8 MACRO reg

xor rax, rax
mov al, reg
mov WORD PTR [rcx], ax
add rcx, 2

ENDM

Z80HFCarryFromBit11 MACRO memptr16_a, memptr16_b, usecf

; (((a & 0xFFF) + (b & 0xFFF)) & 0x1000) == 0x1000

ResetFlagBits HF
mov ax, WORD PTR [memptr16_a]
mov bx, WORD PTR [memptr16_b]
and ax, 0FFFh
and bx, 0FFFh
IF usecf EQ 1
push dx
xor dx, dx
mov dl, RegF
and dl, 1
add ax, dx
pop dx
ENDIF
add bx, ax
and bx, 01000h
shr bx, 8
or RegF, bl

ENDM

Z80HFBorrowFromBit12 MACRO memptr16_a, memptr16_b, usecf

; ((a & 0xFFF) - (b & 0xFFF)) & 0x1000) == 0x1000
ResetFlagBits HF
mov ax, WORD PTR [memptr16_a]
mov bx, WORD PTR [memptr16_b]
and ax, 0FFFh
and bx, 0FFFh
IF usecf EQ 1
push dx
xor dx, dx
mov dl, RegF
and dl, 1
add ax, dx
pop dx
ENDIF
sub bx, ax
and bx, 01000h
shr bx, 8
or RegF, bl

ENDM


SetIOBlockFlagsOUT MACRO reg8ptr

push x_ax
push x_bx

ResetFlagBits HF, PVF, NF, CF

; SF, ZF flags Affected by decreasing register B
mov al, RegB
test al, al
X86FlagsToZ80Flags SF, ZF

;The N flag is a copy of bit 7 of the last value read from/written too the I/O port
mov al, [reg8ptr]
and al, 080h
shr al, SF-NF
or RegF, al

; HF and CF Both set if ((HL) + L > 255)
xor x_ax, x_ax
xor x_bx, x_bx
mov al, [reg8ptr]
mov bl, RegL
add ax, bx
and ax, 0FF00h
test ah, ah
lahf
and ah, 040h
xor ah, 040h
shr ah, ZF-CF
or RegF, ah
shl ah, HF-CF
or RegF, ah

;PF The parity of ((((HL) + L) & 7) xor B)
xor x_ax, x_ax
xor x_bx, x_bx
mov al, [reg8ptr]
mov bl, RegL
add ax, bx
and ax, 07h
mov bl, RegB
xor ax, bx
lahf
and ah, 04h
or RegF, ah

pop x_bx
pop x_ax

ENDM

SetIOBlockFlagsIN MACRO reg8ptr, incdec

push x_ax
push x_bx

ResetFlagBits HF, PVF, NF, CF

; SF, ZF flags Affected by decreasing register B
mov al, RegB
test al, al
X86FlagsToZ80Flags SF, ZF

;The N flag is a copy of bit 7 of the last value read from/written too the I/O port
mov al, [reg8ptr]
and al, 080h
shr al, SF-NF
or RegF, al

;HF and CF Both set if ((HL) + ((C +/- 1) & 255) > 255)
xor x_ax, x_ax
xor x_bx, x_bx
mov al, [reg8ptr]
mov bl, RegC
add bl, incdec
add ax, bx
and ax, 0FFh
and ax, 0FF00h
test ah, ah
lahf
and ah, 040h
xor ah, 040h
shr ah, ZF-CF
or RegF, ah
shl ah, HF-CF
or RegF, ah

;PF The parity of (((HL) + ((C +/- 1) & 255)) & 7) xor B)
xor x_ax, x_ax
mov al, [reg8ptr]
add al, RegC
add al, incdec
and al, 0FFh
and al, 07h
xor al, RegB
lahf
and ah, 04h
or RegF, ah

pop x_bx
pop x_ax

ENDM
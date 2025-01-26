.code

EmulateOpcodeTime MACRO Estados, Ciclos, uSecs

ENDM

ResetCPUCicles MACRO

ENDM

CopyToMaskedRegOrNextOpcode MACRO msk, Reg

	push x_cx
	invoke DCB_REGISTER_FROM_MASK, msk	
	pop x_cx
	cmp reg_di, 0	
	jz Z80Loop
	push x_cx
	push x_dx
	invoke Inst_LD8, Reg, reg_di
	pop x_dx
	pop x_cx
ENDM

ProcesarOpcodeFromRom MACRO JumpTable	
		
	xor x_bp, x_bp
	xor x_ax, x_ax	
	mov al, [reg_pc]             ; Select index

	;invoke printOpcode, reg_pc

	mov x_bp, OFFSET JumpTable ; Load the base address of the jump table into RBP
	mov x_cx, [x_bp + x_ax*PTR_SIZE]    ; Load the function address from the table
	inc reg_pc
	jmp x_cx

ENDM

CargarFlags MACRO
	
	lahf
	mov RegF,ah

ENDM

ModifOverflowFlag MACRO

	jo setoverflow		
	push x_cx
	push x_dx
	invoke UnSetBit,OFFSET RegF, PVF
	pop x_dx
	pop x_cx
	jmp exitoverflow

setoverflow:
	push x_cx
	push x_dx
	invoke SetBit,OFFSET RegF,PVF	
	pop x_dx
	pop x_cx
	
exitoverflow:
	
ENDM	

ModifParityFlag MACRO

	jp setparity		
	push x_cx
	push x_dx
	invoke UnSetBit,OFFSET RegF, PVF
	pop x_dx
	pop x_cx
	jmp exitparity

setparity:
	push x_cx
	push x_dx
	invoke SetBit,OFFSET RegF,PVF	
	pop x_dx
	pop x_cx
	
exitparity:
	
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

IncRegRByBC MACRO

mov al,RegR
mov ah,al
and al,80h
add ah,2
and ah,7fh
or al,ah
mov RegR,al
	
ENDM


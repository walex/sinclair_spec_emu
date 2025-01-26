.data

EXTERNDEF Inst_OUT_Impl:PROC
EXTERNDEF Inst_IN_Impl:PROC
EXTERNDEF onDeviceIOEnd:PROC

.code

Inst_RST PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; dir:BYTE -> dl

push x_dx
mov x_dx, reg_pc
xor x_bx,x_bx
xor x_ax,x_ax
mov x_ax,x_cx
sub reg_pc,x_cx
mov bx,RegSP
add x_cx,x_bx
dec x_cx
mov x_dx, reg_pc
mov [x_cx],dh
dec x_cx
mov [x_cx],dl
sub bx,2
mov RegSP,bx
xor x_bx,x_bx
pop x_dx
mov bl,dl
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_RST ENDP

Inst_LD16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov ax, WORD PTR [x_cx]
mov WORD PTR [x_dx], ax
ret

Inst_LD16 ENDP

Inst_LD8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx
; Reg_Dest:PTR BYTE -> x_dx

mov al,[x_cx]
mov [x_dx],al 
ret

Inst_LD8 ENDP

Inst_INC16 PROC

; parameters
; Reg_Dest:PTR WORD -> x_cx

inc WORD PTR [x_cx]
ret

Inst_INC16 ENDP

Inst_INC8 PROC

; parameters
; Reg_Dest:PTR BYTE -> x_cx

inc BYTE PTR [x_cx]
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_INC8 ENDP

Inst_DEC16 PROC

; parameters
; Reg_Dest:PTR WORD -> x_cx

dec WORD PTR [x_cx]
ret

Inst_DEC16 ENDP

Inst_DEC8 PROC

; parameters
; Reg_Dest:PTR BYTE -> x_cx

dec BYTE PTR [x_cx]
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_DEC8 ENDP

Inst_EX PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov reg_di,x_dx
mov bx,WORD PTR [reg_di]
mov reg_si,x_cx
mov ax, WORD PTR[reg_si]
xchg ax,bx
mov WORD PTR [reg_si],ax
mov WORD PTR [reg_di],bx 
ret

Inst_EX ENDP

Inst_ADD16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov x_ax,x_dx
mov x_bx,x_cx
mov bx, WORD PTR [x_bx]
mov cx, WORD PTR [x_ax]	
add cx,bx
mov WORD PTR [x_ax],cx
lahf
jc add_cf_set
invoke UnSetBit,OFFSET RegF,CF
jmp add_hf_check
add_cf_set:
invoke SetBit,OFFSET RegF,CF
add_hf_check:
shr al, 3            ; Shift AL right by 3 bits to isolate bit 3 carry
and al, 1            ; Mask all other bits, leaving only the carry from bit 3
jnz add_hf_set
invoke UnSetBit,OFFSET RegF,HF
jmp add16ext
add_hf_set:
invoke SetBit,OFFSET RegF,HF
add16ext:
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADD16 ENDP

Inst_ADD8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov x_ax,x_cx
mov al,[x_ax]	
add RegA, al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADD8 ENDP

Inst_ADC8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov al,[x_ax]	
adc RegA, al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADC8 ENDP

Inst_ADC16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov ax, WORD PTR [x_ax]	
adc RegHL,ax
CargarFlags	
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADC16 ENDP

Inst_BIT PROC

; parameters
; Reg:PTR BYTE -> x_cx
; Idx:BYTE -> dl

push x_cx
push x_dx
invoke UnSetBit,OFFSET RegF,ZF
pop x_dx
pop x_cx
xor bx,bx
mov x_ax,x_cx	
mov al,[x_ax]
xor ah,ah
mov cl,dl
mov bl,1
shl bl,cl
test al,bl
lahf		
and ah,40h	
or RegF,ah
invoke UnSetBit,OFFSET RegF,NF
invoke SetBit,OFFSET RegF,HF
ret

Inst_BIT ENDP

Inst_AND PROC

; parameters
; Valor:BYTE -> cl

mov al,cl
and RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,CF
ret

Inst_AND ENDP

Inst_OR PROC

; parameters
; Valor:BYTE -> cl

mov al,cl
or RegA,al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,HF	
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,CF
ret

Inst_OR ENDP

Inst_XOR PROC

; parameters
; Valor:BYTE -> cl

mov al,cl
xor RegA,al		
CargarFlags
ModifParityFlag
invoke UnSetBit,OFFSET RegF,HF	
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,CF
ret

Inst_XOR ENDP

Inst_CCF PROC

xor RegF,1
xor RegH,1
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_CCF ENDP

Inst_SCF PROC

invoke UnSetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF
invoke SetBit,OFFSET RegF,CF

ret

Inst_SCF ENDP

Inst_CP PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov x_ax,x_cx
mov al,[x_ax]
cmp RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_CP ENDP

Inst_CPD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
cmp RegA,bl
CargarFlags
dec RegHL
dec RegBC
jz UnSetP
invoke SetBit,OFFSET RegF,PVF
jmp salir
UnSetP:
invoke UnSetBit,OFFSET RegF,PVF		
salir:
invoke SetBit,OFFSET RegF,NF
ret

Inst_CPD ENDP

Inst_CPDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz inicio_cpdr
mov RegBC, 0FFFFh
inicio_cpdr:
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
dec RegHL
dec RegBC
invoke SetBit,OFFSET RegF,PVF
cmp RegA,bl
CargarFlags
jz end_cpdr
cmp RegBC, 0
jz end_cpdr_bc0
IncRegRByBC
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp inicio_cpdr
end_cpdr_bc0:
invoke UnSetBit,OFFSET RegF,PVF
end_cpdr:
invoke SetBit,OFFSET RegF,NF
EmulateOpcodeTime 18,4
ret

Inst_CPDR ENDP

Inst_CPI PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
cmp RegA,bl
CargarFlags
inc RegHL
dec RegBC
jz UnSetP
invoke SetBit,OFFSET RegF,PVF
jmp salir
UnSetP:
invoke UnSetBit,OFFSET RegF,PVF		
salir:
invoke SetBit,OFFSET RegF,NF
ret

Inst_CPI ENDP

Inst_CPIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz inicio_cpir
mov RegBC, 0FFFFh
inicio_cpir:
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
inc RegHL
dec RegBC
invoke SetBit,OFFSET RegF,PVF
cmp RegA,bl
CargarFlags
jz end_cpir
cmp RegBC, 0
jz end_cpir_bc0
IncRegRByBC
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp inicio_cpir
end_cpir_bc0:
invoke UnSetBit,OFFSET RegF,PVF
end_cpir:
invoke SetBit,OFFSET RegF,NF
EmulateOpcodeTime 18,4
ret

Inst_CPIR ENDP

Inst_CPL PROC

not RegA
invoke SetBit,OFFSET RegF,HF
invoke SetBit,OFFSET RegF,NF
ret

Inst_CPL ENDP

Inst_DAA PROC

mov al, RegA
xor bl, bl
invoke UnSetBit,OFFSET RegF,CF

;- If the A register is greater than 0x99, OR the Carry flag is SET, then
;
;    The upper four bits of the Correction Factor are set to 6,
;    and the Carry flag will be SET.
;  Else
;    The upper four bits of the Correction Factor are set to 0,
;    and the Carry flag will be CLEARED.
;

cmp al, 99h
jle daa_step2
invoke ChecktBit,OFFSET RegF,CF
jz daa_step2
mov bl, 60h
invoke SetBit,OFFSET RegF,CF
;
;- If the lower four bits of the A register (A AND 0x0F) is greater than 9,
;  OR the Half-Carry (H) flag is SET, then
;
;    The lower four bits of the Correction Factor are set to 6.
;  Else
;    The lower four bits of the Correction Factor are set to 0.
;
;

daa_step2:
mov ah, al
and ah, 0fh
cmp ah, 9h
jle daa_step3
invoke ChecktBit,OFFSET RegF,HF
jz daa_step3
or bl, 6h

;- This results in a Correction Factor of 0x00, 0x06, 0x60 or 0x66.
;
;

;- If the N flag is CLEAR, then
;
;    ADD the Correction Factor to the A register.
;  Else
;    SUBTRACT the Correction Factor from the A register.
;
;

daa_step3:
mov ah, al
invoke ChecktBit,OFFSET RegF,NF
jnz sub_cf
add al, bl
jmp daa_step4
sub_cf:
sub al, bl
mov RegA, al

;- The Flags are set as follows:
;
;  Carry:      Set/clear as in the first step above.
;
;  Half-Carry: Set if the correction operation caused a binary carry/borrow
;              from bit 3 to bit 4.
;              For this purpose, may be calculated as:
;              Bit 4 of: A(before) XOR A(after).
;
;  S,Z,P,5,3:  Set as for simple logic operations on the resultant A value.
;
;  N:          Leave.

daa_step4:
invoke UnSetBit,OFFSET RegF,HF
and ah, 8h
and al, 8h
xor ah, al
jz finish
invoke SetBit,OFFSET RegF,HF
finish:
ret

Inst_DAA ENDP

Inst_LDD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegDE
mov [reg_di],al
dec RegDE
dec RegHL
dec RegBC
jz UnSetP
invoke SetBit,OFFSET RegF,PVF
jmp salir
UnSetP:
invoke UnSetBit,OFFSET RegF,PVF		
salir:
invoke UnSetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_LDD ENDP

Inst_LDDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz inicio_ldir
mov RegBC, 0FFFFh
inicio_ldir:
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegDE
mov [reg_di],al
invoke execute_interrupts, x_cx
dec RegHL
dec RegDE
dec RegBC
jz salir_ldir
push x_cx
invoke UnSetBit,OFFSET RegF,PVF
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
pop x_cx
IncRegRByBC
EmulateOpcodeTime 21,5
jmp inicio_ldir
salir_ldir:
invoke UnSetBit,OFFSET RegF,PVF
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
EmulateOpcodeTime 16,4
ret 

Inst_LDDR ENDP

Inst_LDI PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegDE
mov [reg_di],al
inc RegDE
inc RegHL
dec RegBC

jz UnSetP
invoke SetBit,OFFSET RegF,PVF
jmp salir
UnSetP:
invoke UnSetBit,OFFSET RegF,PVF		
salir:
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
ret

Inst_LDI ENDP

Inst_LDIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz inicio_ldir
mov RegBC, 0FFFFh
inicio_ldir:
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,x_cx,RegDE
mov [reg_di],al
invoke execute_interrupts, x_cx
inc RegHL
inc RegDE
dec RegBC
jz salir_ldir
push x_cx
invoke SetBit,OFFSET RegF,PVF
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
pop x_cx
IncRegRByBC
EmulateOpcodeTime 21,5
jmp inicio_ldir
salir_ldir:
invoke UnSetBit,OFFSET RegF,PVF
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
EmulateOpcodeTime 16,4
ret 

Inst_LDIR ENDP

Inst_NEG PROC

neg RegA
CargarFlags
ModifOverflowFlag
ret

Inst_NEG ENDP

Inst_RL PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
rcl bl,1
mov [x_ax],bl
lahf
and ah,1
mov al,ah
test bl,0ffh
lahf
and ah,0c4h
or ah,al
mov RegF,ah
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF		
ret

Inst_RL ENDP

Inst_RLA PROC

mov ah,RegF
sahf
mov al,RegA
mov bl,RegA
rcl bl,1
mov RegA,bl
shr al,7
jnz rlc_set_c
invoke UnSetBit,OFFSET RegF,CF
jmp rlc_exit
rlc_set_c:
invoke SetBit,OFFSET RegF,CF
rlc_exit:
invoke UnSetBit,OFFSET RegF,HF	
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_RLA ENDP

Inst_RLC PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
rol bl,1
mov [x_ax],bl
lahf
and ah,1
mov al,ah
test bl,0ffh
lahf
and ah,0c4h
or ah,al
mov RegF,ah
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF		
ret

Inst_RLC ENDP

Inst_RLCA PROC

mov al,RegA
mov bl,RegA
rol bl,1
mov RegA,bl
shr al,7
jnz rlca_set_c
invoke UnSetBit,OFFSET RegF,CF
jmp rlca_exit
rlca_set_c:
invoke SetBit,OFFSET RegF,CF
rlca_exit:
invoke UnSetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF		
ret

Inst_RLCA ENDP

Inst_RLD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL		
mov ah,RegA
and ah,0fh 	;parte baja de RegA
mov al,RegA
and al,0f0h ;parte alta de RegA
shl ax,4  ; RegA con nibbles intercambiados en ah
mov al,[reg_di] ; (HL) en al
rol ax,4
mov RegA,ah
mov [reg_di],al
test ah,ah
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
ret

Inst_RLD ENDP

Inst_RR PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
rcr bl,1
mov [x_ax],bl
lahf
and ah,1
mov al,ah
test bl,0ffh
lahf
and ah,0c4h
or ah,al
mov RegF,ah
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF		
ret

Inst_RR ENDP

Inst_RRA PROC

mov ah,RegF
sahf
mov al,RegA
mov bl,RegA
rcr bl,1
mov RegA,bl
test al, 0
jnz rra_set_c
invoke UnSetBit,OFFSET RegF,CF
jmp rra_exit
rra_set_c:
invoke SetBit,OFFSET RegF,CF
rra_exit:
invoke UnSetBit,OFFSET RegF,HF	
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_RRA ENDP

Inst_RRC PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
ror bl,1
mov [x_ax],bl		
lahf
and ah,1
mov al,ah
and bl,0ffh
lahf
and ah,0c4h
or ah,al
mov RegF,ah
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF		
ret

Inst_RRC ENDP

Inst_RRCA PROC

mov al,RegA
mov bl,RegA
ror bl,1
mov RegA,bl
test al,0
jnz rrca_set_c
invoke UnSetBit,OFFSET RegF,CF
jmp rrca_exit
rrca_set_c:
invoke SetBit,OFFSET RegF,CF
rrca_exit:
invoke UnSetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF		
ret

Inst_RRCA ENDP

Inst_RRD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO, x_cx,RegHL		
mov ah,RegA 
mov al,[reg_di]
ror ax,4
mov bh,ah
and bh,0fh	
mov bl,ah
and bl,0f0h
shl bx,4
mov RegA,bh	
mov [reg_di],al
test bh,bh
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF
ret

Inst_RRD ENDP

Inst_SBC8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov al,[x_ax]	
sbb RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SBC8 ENDP

Inst_SBC16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov ax,WORD PTR [x_ax]	
sbb RegHL,ax
CargarFlags	
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SBC16 ENDP

Inst_SUB PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov x_ax,x_cx
mov al,[x_ax]	
sub RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SUB ENDP

Inst_SLA PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
sal bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SLA ENDP

Inst_SRA PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
sar bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SRA ENDP

Inst_SRL PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
shr bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SRL ENDP

Inst_SLL PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov ah,RegF
sahf
mov x_ax,x_cx
mov bl,[x_ax]
sal bl,1
push x_ax	
CargarFlags
pop x_ax
or bl,1    
mov [x_ax],bl

invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SLL ENDP

Inst_JP PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; dir:WORD -> dx

xor x_bx,x_bx
mov x_ax,x_cx
mov bx,dx
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_JP ENDP

Inst_PUSH PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; Reg:WORD -> dx

xor x_bx,x_bx
mov bx,RegSP
add x_cx,x_bx
mov ax,dx			;ah Reg HIGH , al Reg LOW
dec x_cx			; (SP-1)
mov [x_cx],ah
dec x_cx			; (SP-2)
mov [x_cx],al
sub RegSP,2
ret

Inst_PUSH ENDP

Inst_POP PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; Reg: PTR WORD -> x_dx

xor x_bx,x_bx
mov bx,RegSP
add x_cx,x_bx
mov al,[x_cx]
inc x_cx			; (SP+1)
mov ah,[x_cx]
add RegSP,2
mov x_bx,x_dx
mov [x_bx],ax
ret

Inst_POP ENDP

Inst_RET PROC

; parameters
; memPtr:PTR BYTE -> x_cx

xor x_bx,x_bx
xor x_ax,x_ax
mov reg_pc,x_cx
mov bx,RegSP
add x_cx,x_bx
mov al,[x_cx]
inc x_cx
mov ah,[x_cx]
add reg_pc,x_ax
add RegSP,2
ret

Inst_RET ENDP

Inst_CALL PROC

; parameters 
; memPtr:PTR BYTE -> x_cx
; dir: WORD -> dx

xor x_bx,x_bx
xor x_ax,x_ax
mov x_ax,x_cx
sub reg_pc,x_cx
mov bx,RegSP
add x_cx,x_bx
dec x_cx
push x_dx
mov x_dx, reg_pc
mov [x_cx],dh
dec x_cx
mov [x_cx],dl
sub bx,2
mov RegSP,bx
xor x_bx,x_bx
pop x_dx
mov bx,dx
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_CALL ENDP

Inst_OUT PROC

; parameters
; port:WORD -> cx
; value:BYTE -> dl

push reg_pc
push reg_si
push rbp

invoke Inst_OUT_Impl, cx, dl

pop rbp
pop reg_si
pop reg_pc

ret

Inst_OUT ENDP

Inst_IN PROC 

; parameters
; port:WORD -> cx
; dest:PTR BYTE -> x_dx

push reg_pc
push reg_si
push rbp

invoke Inst_IN_Impl, cx, x_dx

pop rbp
pop reg_si
pop reg_pc

ret

Inst_IN ENDP

INST_OUTD PROC 

; parameters
; port:BYTE  -> cx
; value:BYTE -> dl

invoke INST_OUT, cx, dl	 
dec RegB
dec RegHL
ret

INST_OUTD ENDP

INST_OUTI PROC 

; parameters
; port:BYTE  -> cx
; value:BYTE -> dl

invoke INST_OUT, cx, dl	 
dec RegB
inc RegHL
ret

INST_OUTI ENDP

INST_OTIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; port:WORD  -> dx
; value:BYTE -> r8b

INST_OUTIR_init:
	invoke INST_OUTI, dx, r8b
	invoke execute_interrupts, x_cx
	cmp RegB, 0
	jz INST_OUTIR_end
	jmp INST_OUTIR_init
INST_OUTIR_end:
ret

INST_OTIR ENDP

INST_OTDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; port:WORD  -> dx
; value:BYTE -> r8b

INST_OUTDR_init:
invoke INST_OUTD, dx, r8b
invoke execute_interrupts, x_cx
cmp RegB, 0
jz INST_OUTDR_end
jmp INST_OUTDR_init
INST_OUTDR_end:
ret

INST_OTDR ENDP

INST_IND PROC 

; parameters
; port:BYTE  -> cx
; value:BYTE -> x_dx

invoke INST_IN, cx, x_dx	 
dec RegB
dec RegHL
ret

INST_IND ENDP

INST_INDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; port:WORD  -> dx
; dest:PTR BYTE -> r8

INST_INDR_init:
invoke INST_IND, dx, r8
invoke execute_interrupts, x_cx
cmp RegB, 0
jz INST_INDR_end
jmp INST_INDR_init
INST_INDR_end:
ret

INST_INDR ENDP

INST_INI PROC

; parameters
; memPtr:PTR BYTE -> cl
; dest:PTR BYTE  -> x_dx

invoke INST_IN, cl, x_dx	 
dec RegB
inc RegHL
ret

INST_INI ENDP

INST_INIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx
; port:PTR BYTE  -> dx
; dest:PTR BYTE  -> r8

INST_INIR_init:
invoke INST_INI, dx, r8
invoke execute_interrupts, x_cx
cmp RegB, 0
jz INST_INIR_end
jmp INST_INIR_init
INST_INIR_end:
ret

INST_INIR ENDP

Noemu PROC

; parameters 
; p: WORD -> cx
ret

Noemu ENDP

Undoc PROC

; parameters 
; p: WORD -> cx
ret

Undoc ENDP
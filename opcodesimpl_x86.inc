.data

Inst_OUT_Impl PROTO STDCALL port:WORD, v:BYTE
Inst_IN_Impl PROTO STDCALL port:WORD, v:PTR BYTE
onDeviceIOEnd PROTO STDCALL

.code

Inst_RST PROC memPtr:PTR BYTE, dir:BYTE

xor x_bx,x_bx
xor x_ax,x_ax
mov x_cx,memPtr
mov x_ax,x_cx
sub reg_pc,x_cx
mov bx,RegSP
add x_cx,x_bx
dec x_cx
mov [x_cx],dh
dec x_cx
mov [x_cx],dl
sub bx,2
mov RegSP,bx
xor x_bx,x_bx
mov bl,dir
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_RST ENDP

Inst_LD16 PROC Reg_Ori:PTR WORD, Reg_Dest:PTR WORD

mov x_bx,Reg_Dest
mov x_ax,Reg_Ori
mov ax, WORD PTR [x_ax]
mov WORD PTR [x_bx], ax
ret

Inst_LD16 ENDP

Inst_LD8 PROC Reg_Ori:PTR BYTE, Reg_Dest:PTR BYTE

mov x_bx,Reg_Dest
mov x_ax,Reg_Ori
mov al,[x_ax]
mov [x_bx],al 
ret

Inst_LD8 ENDP

Inst_INC16 PROC Reg_Dest:PTR WORD

mov x_ax, Reg_Dest
inc WORD PTR [x_ax]
ret

Inst_INC16 ENDP

Inst_INC8 PROC Reg_Dest:PTR BYTE

mov x_ax, Reg_Dest
inc BYTE PTR [x_ax]

CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_INC8 ENDP

Inst_DEC16 PROC Reg_Dest:PTR WORD

mov x_ax, Reg_Dest
dec WORD PTR [x_ax]
ret

Inst_DEC16 ENDP

Inst_DEC8 PROC Reg_Dest:PTR BYTE

mov x_ax, Reg_Dest
dec BYTE PTR [x_ax]

CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_DEC8 ENDP

Inst_EX PROC Reg_Ori:PTR WORD, Reg_Dest:PTR WORD

mov reg_di,Reg_Dest
mov bx, WORD PTR [reg_di] 

mov reg_si,Reg_Ori
mov ax, WORD PTR [reg_si] 

xchg ax,bx
mov WORD PTR [reg_si],ax
mov WORD PTR [reg_di],bx 
ret

Inst_EX ENDP

Inst_ADD16 PROC Reg_Ori:PTR WORD, Reg_Dest:PTR WORD

mov x_ax,Reg_Dest
mov x_bx,Reg_Ori
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

Inst_ADD8 PROC Reg_Ori:PTR BYTE

mov x_ax,Reg_Ori
mov al,[x_ax]	
add RegA, al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADD8 ENDP

Inst_ADC8 PROC Reg_Ori:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg_Ori
mov al,[x_ax]	
adc RegA, al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADC8 ENDP

Inst_ADC16 PROC Reg_Ori:PTR WORD

mov ah,RegF
sahf

mov x_ax,Reg_Ori
mov ax, WORD PTR [x_ax]	
adc RegHL,ax
CargarFlags	
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,NF
ret

Inst_ADC16 ENDP

Inst_BIT PROC Reg:PTR BYTE,Idx:BYTE

invoke UnSetBit,OFFSET RegF,ZF
xor bx,bx
mov x_ax,Reg	
mov al,[x_ax]

xor ah,ah	

mov cl,Idx	

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

Inst_AND PROC Valor:BYTE

mov al,Valor
and RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,HF
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,CF
ret

Inst_AND ENDP

Inst_OR PROC Valor:BYTE

mov al,Valor
or RegA,al
CargarFlags
ModifOverflowFlag
invoke UnSetBit,OFFSET RegF,HF	
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,CF
ret

Inst_OR ENDP

Inst_XOR PROC Valor:BYTE

mov al,Valor
xor RegA,al		
CargarFlags
ModifParityFlag
invoke UnSetBit,OFFSET RegF,CF
invoke UnSetBit,OFFSET RegF,NF	
invoke UnSetBit,OFFSET RegF,HF	
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

Inst_CP PROC Reg:PTR BYTE

mov x_ax,Reg
mov al,[x_ax]
cmp RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_CP ENDP


Inst_CPD PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
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


Inst_CPDR PROC memPtr:PTR BYTE

cmp RegBC, 0
jnz inicio_cpdr
mov RegBC, 0FFFFh
inicio_cpdr:
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
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
invoke execute_interrupts, memPtr
jmp inicio_cpdr
end_cpdr_bc0:
invoke UnSetBit,OFFSET RegF,PVF
end_cpdr:
invoke SetBit,OFFSET RegF,NF
EmulateOpcodeTime 18,4
ret

Inst_CPDR ENDP

Inst_CPI PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
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

Inst_CPIR PROC memPtr:PTR BYTE

cmp RegBC, 0
jnz inicio_cpir
mov RegBC, 0FFFFh
inicio_cpir:
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
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
invoke execute_interrupts, memPtr
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

and RegF,2h	
mov bl,RegF
mov al,RegA
jnz inst_das
daa
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
jmp salir

inst_das:
das
CargarFlags
salir:
ret

Inst_DAA ENDP

Inst_LDD PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegDE
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

Inst_LDDR PROC memPtr:PTR BYTE

cmp RegBC, 0
jnz inicio_ldir
mov RegBC, 0FFFFh
inicio_ldir:
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegDE
mov [reg_di],al
invoke execute_interrupts, memPtr
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

Inst_LDI PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegDE
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

Inst_LDIR PROC memPtr:PTR BYTE

cmp RegBC, 0
jnz inicio_ldir
mov RegBC, 0FFFFh
inicio_ldir:
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL
mov al,[reg_di]
invoke DIR_REGISTRO_INDIRECTO,memPtr,RegDE
mov [reg_di],al
invoke execute_interrupts, memPtr
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

Inst_RL PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
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

Inst_RLC PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
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

Inst_RLD PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO,memPtr,RegHL		
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

Inst_RR PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
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

Inst_RRC PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
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

Inst_RRD PROC memPtr:PTR BYTE

invoke DIR_REGISTRO_INDIRECTO, memPtr,RegHL		

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

Inst_SBC8 PROC Reg_Ori:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg_Ori
mov al,[x_ax]	
sbb RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SBC8 ENDP

Inst_SBC16 PROC Reg_Ori:PTR WORD

mov ah,RegF
sahf

mov x_ax,Reg_Ori
mov ax,WORD PTR [x_ax]	
sbb RegHL,ax
CargarFlags	
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SBC16 ENDP

Inst_SUB PROC Reg_Ori:PTR BYTE

mov x_ax,Reg_Ori
mov al,[x_ax]	
sub RegA,al
CargarFlags
ModifOverflowFlag
invoke SetBit,OFFSET RegF,NF
ret

Inst_SUB ENDP

Inst_SLA PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
mov bl,[x_ax]
sal bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SLA ENDP

Inst_SRA PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
mov bl,[x_ax]
sar bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SRA ENDP

Inst_SRL PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
mov bl,[x_ax]
shr bl,1
mov [x_ax],bl
CargarFlags
invoke UnSetBit,OFFSET RegF,NF
invoke UnSetBit,OFFSET RegF,HF	 
ret

Inst_SRL ENDP

Inst_SLL PROC Reg:PTR BYTE

mov ah,RegF
sahf

mov x_ax,Reg
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

Inst_JP PROC memPtr:PTR BYTE, dir:WORD

xor x_bx,x_bx
mov x_ax,memPtr
mov bx,dir
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_JP ENDP

Inst_PUSH PROC memPtr:PTR BYTE, Reg:WORD

xor x_bx,x_bx
mov x_cx,memPtr
mov bx,RegSP
add x_cx,x_bx
mov ax,Reg		;ah Reg HIGH , al Reg LOW
dec x_cx		; (SP-1)
mov [x_cx],ah
dec x_cx		; (SP-2)
mov [x_cx],al
sub RegSP,2
ret

Inst_PUSH ENDP

Inst_POP PROC  memPtr:PTR BYTE, Reg:PTR WORD

xor x_bx,x_bx
mov x_cx,memPtr
mov bx,RegSP
add x_cx,x_bx
mov al,[x_cx]
inc x_cx			; (SP+1)
mov ah,[x_cx]
add RegSP,2

mov x_bx,Reg
mov [x_bx],ax
ret

Inst_POP ENDP

Inst_RET PROC memPtr:PTR BYTE 

xor x_bx,x_bx
xor x_ax,x_ax
mov x_cx,memPtr
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

Inst_CALL PROC memPtr:PTR BYTE, dir:WORD

xor x_bx,x_bx
xor x_ax,x_ax

mov x_cx,memPtr
mov x_ax,x_cx
sub reg_pc,x_cx
mov bx,RegSP
add x_cx,x_bx
dec x_cx
mov [x_cx],dh
dec x_cx
mov [x_cx],dl
sub bx,2
mov RegSP,bx
xor x_bx,x_bx
mov bx,dir
add x_ax,x_bx
mov reg_pc,x_ax
ret

Inst_CALL ENDP

Inst_OUT PROC port:WORD, value:BYTE

push reg_pc
invoke Inst_OUT_Impl, port, value
pop reg_pc
ret

Inst_OUT ENDP

Inst_IN PROC port:WORD, dest:PTR BYTE

push reg_pc
invoke Inst_IN_Impl, port, dest
pop reg_pc
ret

Inst_IN ENDP

INST_OUTD PROC port:WORD, value:BYTE

invoke INST_OUT, port, value	 
dec RegB
dec RegHL
ret

INST_OUTD ENDP

INST_OUTI PROC port:WORD, value:BYTE

invoke INST_OUT, port, value	 
dec RegB
inc RegHL
ret

INST_OUTI ENDP

INST_OTIR PROC memPtr: PTR BYTE, port:WORD, value:BYTE

INST_OUTIR_init:
invoke INST_OUTI, port, value
invoke execute_interrupts, memPtr
cmp RegB, 0
jz INST_OUTIR_end
jmp INST_OUTIR_init
INST_OUTIR_end:
ret

INST_OTIR ENDP

INST_OTDR PROC memPtr: PTR BYTE, port:WORD, value:BYTE

INST_OUTDR_init:
invoke INST_OUTD, port, value
invoke execute_interrupts, memPtr
cmp RegB, 0
jz INST_OUTDR_end
jmp INST_OUTDR_init
INST_OUTDR_end:
ret

INST_OTDR ENDP

INST_IND PROC port:WORD, dest:PTR BYTE

invoke INST_IN, port, dest	 
dec RegB
dec RegHL
ret

INST_IND ENDP

INST_INDR PROC memPtr:PTR BYTE, port:WORD, dest:PTR BYTE

INST_INDR_init:
invoke INST_IND, port, dest
invoke execute_interrupts, memPtr
cmp RegB, 0
jz INST_INDR_end
jmp INST_INDR_init
INST_INDR_end:
ret

INST_INDR ENDP

INST_INI PROC port:WORD, dest:PTR BYTE

invoke INST_IN, port, dest	 
dec RegB
inc RegHL
ret

INST_INI ENDP

INST_INIR PROC memPtr: PTR BYTE, port:WORD, dest:PTR BYTE

INST_INIR_init:
invoke INST_INI, port, dest
invoke execute_interrupts, memPtr
cmp RegB, 0
jz INST_INIR_end
jmp INST_INIR_init
INST_INIR_end:
ret

INST_INIR ENDP

Noemu PROC p:WORD 
	  ret
Noemu ENDP

Undoc PROC p:WORD  
	  ret
Undoc ENDP
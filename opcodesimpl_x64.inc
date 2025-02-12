.data

EXTERNDEF Inst_OUT_Impl:PROC
EXTERNDEF Inst_IN_Impl:PROC
EXTERNDEF onDeviceIOEnd:PROC

.code

Inst_LD8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx
; Reg_Dest:PTR BYTE -> x_dx

mov al,[x_cx]
mov [x_dx],al 
ret

Inst_LD8 ENDP

Inst_LD16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov ax, WORD PTR [x_cx]
mov WORD PTR [x_dx], ax
ret

Inst_LD16 ENDP

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

Inst_EX PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov bx,WORD PTR [x_cx]
mov ax, WORD PTR[x_dx]
xchg ax,bx
mov WORD PTR [x_cx],bx
mov WORD PTR [x_dx],ax 
ret

Inst_EX ENDP

Inst_EXX PROC

	invoke Inst_EX,OFFSET RegBC_ESP,OFFSET RegBC
	invoke Inst_EX,OFFSET RegDE_ESP,OFFSET RegDE
	invoke Inst_EX,OFFSET RegHL_ESP,OFFSET RegHL
	ret

Inst_EXX ENDP

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
SetPVFromBC
ResetFlagBits HF, NF
ret

Inst_LDI ENDP

Inst_LDIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz LDIR_start
mov RegBC, 0FFFFh
LDIR_start:
invoke Inst_LDI
cmp RegBC, 0
jz LDIR_exit
IncRegRByBC
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp LDIR_start
LDIR_exit:
ret

Inst_LDIR ENDP

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
SetPVFromBC
ResetFlagBits HF, NF
ret

Inst_LDD ENDP

Inst_LDDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz LDDR_start
mov RegBC, 0FFFFh
LDDR_start:
invoke Inst_LDD
cmp RegBC, 0
jz LDDR_exit
IncRegRByBC
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp LDDR_start
LDDR_exit:
ret

Inst_LDDR ENDP

Inst_CPI PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
cmp RegA,bl
X86FlagsToZ80Flags SF, ZF, HF
inc RegHL
dec RegBC
SetPVFromBC
SetFlagBits NF
ret

Inst_CPI ENDP

Inst_CPIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz CPIR_start
mov RegBC, 0FFFFh
CPIR_start:
invoke Inst_CPI
TestZ80Flag ZF
jz CPIR_exit
TestZ80Flag PVF
jz CPIR_exit
invoke execute_interrupts, x_cx
jmp CPIR_start
CPIR_exit:
ret

Inst_CPIR ENDP

Inst_CPD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL
mov bl, BYTE PTR [reg_di]
dec RegHL
dec RegBC
cmp RegA,bl
X86FlagsToZ80Flags SF, ZF, HF
SetPVFromBC
SetFlagBits NF
ret

Inst_CPD ENDP

Inst_CPDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

cmp RegBC, 0
jnz CPDR_start
mov RegBC, 0FFFFh
CPDR_start:
invoke Inst_CPD
TestZ80Flag ZF
jz CPDR_exit
TestZ80Flag PVF
jz CPDR_exit
invoke execute_interrupts, x_cx
jmp CPDR_start
CPDR_exit:
ret

Inst_CPDR ENDP

Inst_ADD8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov al,[x_cx]	
add RegA, al
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
ResetFlagBits NF
ret

Inst_ADD8 ENDP

Inst_ADC8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

LoadZ80CarryFlag
mov al,[x_cx]	
adc RegA, al
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
ResetFlagBits NF
ret

Inst_ADC8 ENDP

Inst_SUB PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

mov al,[x_cx]	
sub RegA,al
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
SetFlagBits NF
ret

Inst_SUB ENDP

Inst_SBC8 PROC

; parameters
; Reg_Ori:PTR BYTE -> x_cx

LoadZ80CarryFlag
mov al,[x_cx]	
sbb RegA, al
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
SetFlagBits NF
ret

Inst_SBC8 ENDP

Inst_AND PROC

; parameters
; Valor:BYTE -> cl

and RegA,cl
X86FlagsToZ80Flags SF, ZF, OF
SetFlagBits HF
ResetFlagBits NF, CF
ret

Inst_AND ENDP

Inst_OR PROC

; parameters
; Valor:BYTE -> cl

or RegA,cl
X86FlagsToZ80Flags SF, ZF, OF
ResetFlagBits HF, NF, CF
ret

Inst_OR ENDP

Inst_XOR PROC

; parameters
; Valor:BYTE -> cl

xor RegA,cl		
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF, CF
ret

Inst_XOR ENDP

Inst_CP PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov al,[x_cx]
cmp RegA,al
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
SetFlagBits NF
ret

Inst_CP ENDP

Inst_INC8 PROC

; parameters
; Reg_Dest:PTR BYTE -> x_cx

inc BYTE PTR [x_cx]
X86FlagsToZ80Flags SF, ZF, HF, OF
ResetFlagBits NF
ret

Inst_INC8 ENDP

Inst_DEC8 PROC

; parameters
; Reg_Dest:PTR BYTE -> x_cx

dec BYTE PTR [x_cx]
X86FlagsToZ80Flags SF, ZF, HF, OF
SetFlagBits NF
ret

Inst_DEC8 ENDP

Inst_DAA PROC

mov al, RegA
xor bl, bl
ResetFlagBits CF

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
SetFlagBits CF

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
ResetFlagBits HF
and ah, 8h
and al, 8h
xor ah, al
jz finish
SetFlagBits HF
finish:
ret

Inst_DAA ENDP

Inst_CPL PROC

not RegA
SetFlagBits HF, NF
ret

Inst_CPL ENDP

Inst_NEG PROC

neg RegA
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
SetFlagBits NF
ret

Inst_NEG ENDP

Inst_CCF PROC

mov ah, RegF
GetBitMask x_cx, CF
sahf
setc ah
shl ah, HF
xor RegF, cl
or RegF, ah
ResetFlagBits NF
ret

Inst_CCF ENDP

Inst_SCF PROC

ResetFlagBits HF, NF
SetFlagBits CF
ret

Inst_SCF ENDP

Inst_ADD16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx
; Reg_Dest:PTR WORD -> x_dx

mov bx, WORD PTR [x_cx]
add WORD PTR [x_dx],bx
X86FlagsToZ80Flags HF, CF
ResetFlagBits NF
ret

Inst_ADD16 ENDP

Inst_ADC16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx

LoadZ80CarryFlag
mov ax, WORD PTR [x_cx]	
adc RegHL,ax
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
ResetFlagBits NF
ret

Inst_ADC16 ENDP

Inst_SBC16 PROC

; parameters
; Reg_Ori:PTR WORD -> x_cx

LoadZ80CarryFlag
mov ax,WORD PTR [x_cx]	
sbb RegHL,ax
X86FlagsToZ80Flags SF, ZF, HF, OF, CF
SetFlagBits NF
ret

Inst_SBC16 ENDP

Inst_INC16 PROC

; parameters
; Reg_Dest:PTR WORD -> x_cx

inc WORD PTR [x_cx]
ret

Inst_INC16 ENDP

Inst_DEC16 PROC

; parameters
; Reg_Dest:PTR WORD -> x_cx

dec WORD PTR [x_cx]
ret

Inst_DEC16 ENDP

Inst_RLCA PROC

rol RegA,1
X86FlagsToZ80Flags CF
ResetFlagBits HF, NF
ret

Inst_RLCA ENDP

Inst_RLA PROC

LoadZ80CarryFlag
rcl RegA,1
X86FlagsToZ80Flags CF
ResetFlagBits HF, NF
ret

Inst_RLA ENDP

Inst_RRCA PROC

ror RegA,1
X86FlagsToZ80Flags CF
ResetFlagBits HF, NF
ret

Inst_RRCA ENDP

Inst_RRA PROC

LoadZ80CarryFlag
rcr RegA,1
X86FlagsToZ80Flags CF
ResetFlagBits HF, NF
ret

Inst_RRA ENDP

Inst_RLC PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
rol bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_RLC ENDP

Inst_RL PROC

; parameters
; Reg:PTR BYTE -> x_cx

LoadZ80CarryFlag
mov bl,[x_cx]
rcl bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_RL ENDP

Inst_RRC PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
ror bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_RRC ENDP

Inst_RR PROC

; parameters
; Reg:PTR BYTE -> x_cx

LoadZ80CarryFlag
mov bl,[x_cx]
rcr bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_RR ENDP

Inst_SLA PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
sal bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_SLA ENDP

Inst_SLL PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
sal bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_SLL ENDP

Inst_SRA PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
sar bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_SRA ENDP

Inst_SRL PROC

; parameters
; Reg:PTR BYTE -> x_cx

mov bl,[x_cx]
shr bl,1
mov [x_cx],bl
X86FlagsToZ80Flags CF
test bl, bl
X86FlagsToZ80Flags ZF, PVF
ResetFlagBits SF, HF, NF
ret

Inst_SRL ENDP

Inst_RLD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL		

xor cx, cx
mov cl, [reg_di]
mov ch, cl
and cl, 0fh	 ; low (HL)
and ch, 0f0h  ; high (HL)		 

xor ax, ax
mov al, RegA
mov ah, al
and al, 0fh ; low A
and ah, 0f0h ; high A

; new (HL)
shl cl, 4
or cl, al
mov [reg_di], cl

; NEW A
shr ch, 4
or ah, ch
mov RegA, ah

test ah, ah
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF

Inst_RLD ENDP

Inst_RRD PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO,x_cx,RegHL		

xor cx, cx
mov cl, [reg_di]
mov ch, cl
and cl, 0fh	 ; low (HL)
and ch, 0f0h  ; high (HL)		 

xor ax, ax
mov al, RegA
mov ah, al
and al, 0fh ; low A
and ah, 0f0h ; high A

; NEW A
or cl, ah
mov RegA, cl

; new (HL)
shl al, 4
shr ch, 4
or al, ch
mov [reg_di], al

test cl, cl
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
ret

Inst_RRD ENDP

Inst_BIT PROC

; parameters
; Reg:PTR BYTE -> x_cx
; Idx:BYTE -> dl

xor  ax, ax
mov al, [x_cx]
ResetFlagBits ZF, NF
SetFlagBits HF
xor cx, cx
mov cl, dl
bt ax, cx
lahf
GetBitMask x_cx, CF
and ah, cl
xor ah, cl
shl ah, ZF-CF
or RegF, ah
ret

Inst_BIT ENDP

Inst_SET PROC

; parameters
; Reg:PTR BYTE -> x_cx
; Idx:BYTE -> dl

push x_cx
mov cl, dl
GetBitMask x_cx, cl
mov dl, cl
pop x_cx
or [x_cx], dl
ret

Inst_SET ENDP

Inst_RES PROC

; parameters
; Reg:PTR BYTE -> x_cx
; Idx:BYTE -> dl

push x_cx
mov cl, dl
GetBitMaskNeg x_cx, cl
mov dl, cl
pop x_cx
and [x_cx], dl
ret

Inst_RES ENDP

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

Inst_IN PROC 

; parameters
; port:WORD -> cx
; dest:PTR BYTE -> x_dx

push reg_pc
push reg_si
push rbp
push x_dx
invoke Inst_IN_Impl, cx, x_dx
mov al, [x_dx]
test al, al
X86FlagsToZ80Flags SF, ZF, PVF
ResetFlagBits HF, NF
pop x_dx
pop rbp
pop reg_si
pop reg_pc
ret

Inst_IN ENDP

INST_INI PROC

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO, x_cx, RegHL 
invoke INST_IN, RegBC, reg_di
inc RegHL
dec RegB
X86FlagsToZ80Flags ZF
SetFlagBits NF
ret

INST_INI ENDP

INST_INIR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

INST_INIR_init:
invoke INST_INI, x_cx
cmp RegB, 0
jz INST_INIR_end
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp INST_INIR_init
INST_INIR_end:
SetFlagBits ZF
ret

INST_INIR ENDP

INST_IND PROC 

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO, x_cx, RegHL 
invoke INST_IN, RegBC, reg_di
dec RegHL
dec RegB
X86FlagsToZ80Flags ZF
SetFlagBits NF
ret

INST_IND ENDP

INST_INDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

INST_INDR_init:
invoke INST_IND, x_cx
cmp RegB, 0
jz INST_INDR_end
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp INST_INDR_init
INST_INDR_end:
SetFlagBits ZF
ret

INST_INDR ENDP

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

INST_OUTI PROC 

; parameters
; memPtr:PTR BYTE  -> x_cx

invoke DIR_REGISTRO_INDIRECTO, x_cx, RegHL
dec RegB
invoke INST_OUT, RegBC, dl	 
inc RegHL
mov al, RegB
test al, al
X86FlagsToZ80Flags ZF
SetFlagBits NF
ret

INST_OUTI ENDP

INST_OTIR PROC

; parameters
; memPtr:PTR BYTE  -> x_cx

INST_OTIR_init:
invoke INST_OUTI, x_cx
cmp RegB, 0
jz INST_OTIR_end
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp INST_OTIR_init
INST_OTIR_end:
SetFlagBits ZF
ret

INST_OTIR ENDP

INST_OUTD PROC 

; parameters
; memPtr:PTR BYTE -> x_cx

invoke DIR_REGISTRO_INDIRECTO, x_cx, RegHL 
dec RegB
mov dl, [reg_di]
invoke INST_OUT, RegBC, dl
dec RegHL
mov al, RegB
test al, al
X86FlagsToZ80Flags ZF
SetFlagBits NF
ret

INST_OUTD ENDP

INST_OTDR PROC

; parameters
; memPtr:PTR BYTE -> x_cx

INST_OTDR_init:
invoke INST_OUTD, x_cx
cmp RegB, 0
jz INST_OTDR_end
EmulateOpcodeTime 21,5
invoke execute_interrupts, x_cx
jmp INST_OTDR_init
INST_OTDR_end:
SetFlagBits ZF
ret

INST_OTDR ENDP

;-----------------------------------------


















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
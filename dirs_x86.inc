.code

DIR_INMEDIATO PROC 

xor x_ax,x_ax
mov al,[reg_pc]                
mov Reg_Tmp,x_ax
mov reg_di, OFFSET Reg_Tmp
inc reg_pc
ret
    
DIR_INMEDIATO ENDP

DIR_INMEDIATO_EXT PROC

xor x_ax,x_ax
mov ax, WORD PTR [reg_pc]
mov Reg_Tmp, x_ax
mov reg_di, OFFSET Reg_Tmp
add reg_pc, 2
ret

DIR_INMEDIATO_EXT ENDP

DIR_RELATIVO PROC 

xor x_bx,x_bx
movsx x_bx, BYTE PTR [reg_pc] ;e
inc reg_pc ; PC+1 
add reg_pc, x_bx ;PC + e
ret

DIR_RELATIVO ENDP

DIR_EXT PROC 

xor x_ax,x_ax
mov ax, WORD PTR [reg_pc]
add reg_pc, 2
mov reg_di, x_ax
ret

DIR_EXT ENDP

DIR_INDEXADO PROC memPtr:PTR BYTE, Reg:WORD

xor reg_si,reg_si
mov si,Reg
movsx x_ax,BYTE PTR [reg_pc]
add reg_si,x_ax
mov reg_di,memPtr
add reg_di,reg_si
ADD_REG_PC memPtr, 1
ret

DIR_INDEXADO ENDP

DIR_REGISTRO_INDIRECTO PROC memPtr:PTR BYTE, Reg:WORD

xor reg_si,reg_si
mov si,Reg
mov reg_di,memPtr
add reg_di,reg_si 
ret 

DIR_REGISTRO_INDIRECTO ENDP

DCB_REGISTER_FROM_MASK PROC value: BYTE

xor x_ax, x_ax
mov al, value
and al, 07h
cmp al , 00h
jz mask_RegB
cmp al , 01h
jz mask_RegC
cmp al , 02h
jz mask_RegD
cmp al , 03h
jz mask_RegE
cmp al , 04h
jz mask_RegH
cmp al , 05h
jz mask_RegL
cmp al , 06h
jz mask_exit	; 06h     (none: documented opcode)
cmp al , 07h
jz mask_RegA

mask_RegB:
mov x_ax, OFFSET RegB
jmp mask_exit

mask_RegC:
mov x_ax, OFFSET RegC
jmp mask_exit

mask_RegD:
mov x_ax, OFFSET RegD
jmp mask_exit

mask_RegE:
mov x_ax, OFFSET RegE
jmp mask_exit

mask_RegH:
mov x_ax, OFFSET RegH
jmp mask_exit

mask_RegL:
mov x_ax, OFFSET RegL
jmp mask_exit

mask_RegA:
mov x_ax, OFFSET RegA

mask_exit:
mov reg_di, x_ax
ret

DCB_REGISTER_FROM_MASK ENDP
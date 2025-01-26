.code

SetBit PROC Reg:PTR BYTE, pos:BYTE
	
	mov x_ax,Reg	
	mov cl,pos	
	mov bl,1
	shl bl,cl	
	or [x_ax],bl	
	ret

SetBit ENDP

UnSetBit PROC Reg:PTR BYTE,pos:BYTE
	
	mov x_ax,Reg	
	mov cl,pos
	mov bl,1
	shl bl,cl			
	not bl
	and [x_ax],bl		
	ret

UnSetBit ENDP

SetFlagIR PROC

test RegI,0ffh
CargarFlags
invoke UnSetBit,OFFSET RegF,1
invoke UnSetBit,OFFSET RegF,4

cmp IFF2,1
jz PVFlag
invoke UnSetBit,OFFSET RegF,PVF
jmp salirPVFlag

PVFlag:
invoke SetBit,OFFSET RegF,PVF

salirPVFlag:
 ret

SetFlagIR ENDP
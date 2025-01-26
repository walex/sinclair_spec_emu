.code

SetBit PROC
	
; parameters
; Reg:PTR BYTE -> x_cx
; pos:BYTE -> dl

	mov x_ax,x_cx	
	mov cl,dl	
	mov bl,1
	shl bl,cl	
	or [x_ax],bl	
	ret

SetBit ENDP

UnSetBit PROC
	
; parameters
; Reg:PTR BYTE -> x_cx
; pos:BYTE -> dl

	mov x_ax,x_cx	
	mov cl,dl
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

ChecktBit PROC
	
; parameters
; Reg:PTR BYTE -> x_cx
; pos:BYTE -> dl

mov al,[x_cx]	
xor x_bx, x_bx
mov cl,dl	
mov bl,1
shl bl,cl
and al,bl
ret

ChecktBit ENDP
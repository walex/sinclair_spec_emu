.code

LD_SetFlagIR PROC

; parameters
; Reg -> x_cx

mov cl, [x_cx]
test cl,cl
X86FlagsToZ80Flags SF, ZF
ResetFlagBits HF, NF, PVF
mov al, IFF2
shl al, PVF
or RegF, al
ret

LD_SetFlagIR ENDP

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
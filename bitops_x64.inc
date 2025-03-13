.data

kHostCPUCycleTimeNano			dq	0
kZ80CycleTimeNano				dq	0
instructionStartTime			dd	0
zero dq 0.0
TEST_OPCODES EQU 1
TEST_OPCODES_TIME EQU 0

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

InitCPUTimer PROC

; parameters
; kZ80CycleTimeNano:DWORD -> xmm1
; kHostCPUCycleTimeNano:DWORD  -> xmm2

IF TEST_OPCODES_TIME eq 1

movsd kZ80CycleTimeNano, xmm1
movsd kHostCPUCycleTimeNano, xmm2

ENDIF

ret

InitCPUTimer ENDP

RunOpcodeTimer PROC

; parameters
; Estados:BYTE -> cl
; Ciclos:BYTE -> dl

IF TEST_OPCODES_TIME eq 1

; calc time of z80 instruction in nanosecs
; opcode time = cycles * Z80CpuCycleTimeNanoSecs;

movq xmm0, x_dx
movq xmm1, kZ80CycleTimeNano
mulsd xmm0, xmm1
; xmm0 stores the real duration in nanosecs of the opcode

RunOpcodeTimer_wait_loop:

; calc time of the emulated opcode in nanosecs
rdtsc
sub eax, instructionStartTime
movq xmm1, x_ax
movq xmm2, kHostCPUCycleTimeNano
mulsd xmm1, xmm2
; xmm1 stores the emulated duration in nanosecs of the opcode

; get remaining time of opcode to complete
subsd xmm0, xmm1

; chekc value
comisd xmm0, zero
ja RunOpcodeTimer_wait_loop

ENDIF

ret

RunOpcodeTimer ENDP

ResetOpcodeTimer PROC

IF TEST_OPCODES_TIME eq 1

rdtsc
mov instructionStartTime, eax

ENDIF

ret

ResetOpcodeTimer ENDP
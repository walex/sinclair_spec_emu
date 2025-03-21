.data
      _CPU STRUCT	
	   UNION
			Reg_AF dw 0
			STRUCT
				Reg_F db 0 	
           		Reg_A db 0                         
			ENDS
       ENDS
	 
	   UNION
			Reg_AF_ESP dw 0
			STRUCT
				Reg_F_ESP db 0	
           		Reg_A_ESP db 0                         
			ENDS
       ENDS

	   UNION
		    Reg_BC dw 0
	 	    STRUCT
				Reg_C db 0	
	       		Reg_B db 0	             
		    ENDS
       ENDS
	
	   UNION
		    Reg_BC_ESP dw 0
			STRUCT
				Reg_C_ESP db 0  
	       		Reg_B_ESP db 0	             
			ENDS
       ENDS


       UNION
		    Reg_DE dw 0
		    STRUCT
				Reg_E db 0 	
		        Reg_D db 0	               
			ENDS
       ENDS
         
	   UNION
		    Reg_DE_ESP dw 0
			STRUCT
				Reg_E_ESP db 0	
		        Reg_D_ESP db 0	               
			ENDS
       ENDS

       UNION
	  	    Reg_HL dw 0
			STRUCT
				Reg_L db 0		       	               
			    Reg_H db 0 
			ENDS
       ENDS             
	
       UNION
		    Reg_HL_ESP dw 0
			STRUCT
				Reg_L_ESP db 0 	
		        Reg_H_ESP db 0	               
			ENDS
       ENDS             
	             
	         
	   UNION
		    Reg_IR dw 0
			STRUCT
				Reg_R db 0 	
		        Reg_I db 0	               
			ENDS
       ENDS 
         
	 UNION
	  	Reg_IX dw 0
		STRUCT
			Reg_IXL db 0		       	               
			Reg_IXH db 0 
		ENDS
     ENDS 

	 UNION
	  	Reg_IY dw 0
		STRUCT
			Reg_IYL db 0		       	               
			Reg_IYH db 0 
		ENDS
     ENDS 

	 Reg_SP dw	0 
	
	 IMF		db	0 
	 IFF1		db  0
	 IFF2		db  0	
	 NMI_LINE	db	0
	 INT_LINE	db	0
	 D_LINE		db	0
	 A_LINE		dw  0
	 HALT		db  0

	 Reg_Tmp PTR_DATA_TYPE 0
	 Reg_Tmp16	dw	0
	 Reg_Tmp8	db	0

   _CPU ENDS		

   reg_pc_tmp dw 0

    OF EQU 11	; x86 oferflow flag

	SF EQU 7	;Bit 7 - Sign flag
	ZF EQU 6	;Bit 6 - Zero flag
	NOUSO5 EQU 5	;Bit 5 - documented as unused (see note)
	HF EQU 4	;Bit 4 - Half-carry flag
	NOUSO3 EQU 3    ;Bit 3 - documented as unused (see note)
	PVF EQU 2       ;Bit 2 - Parity/Overflow flag
	NF EQU 1	;Bit 1 - Subtract flag
	CF EQU 0	;Bit 0 - Carry flag


   Z80_CPU _CPU <>
	
RegAF EQU [Z80_CPU.Reg_AF]
RegA EQU [Z80_CPU.Reg_A]
RegF EQU [Z80_CPU.Reg_F]

RegBC EQU [Z80_CPU.Reg_BC]
RegB EQU [Z80_CPU.Reg_B]
RegC EQU [Z80_CPU.Reg_C]

RegDE EQU [Z80_CPU.Reg_DE]
RegD EQU [Z80_CPU.Reg_D]
RegE EQU [Z80_CPU.Reg_E]

RegHL EQU [Z80_CPU.Reg_HL]
RegH EQU [Z80_CPU.Reg_H]
RegL EQU [Z80_CPU.Reg_L]

RegAF_ESP EQU [Z80_CPU.Reg_AF_ESP]
RegA_ESP EQU [Z80_CPU.Reg_A_ESP]
RegF_ESP EQU [Z80_CPU.Reg_F_ESP]

RegBC_ESP EQU [Z80_CPU.Reg_BC_ESP]
RegB_ESP EQU [Z80_CPU.Reg_B_ESP]
RegC_ESP EQU [Z80_CPU.Reg_C_ESP]

RegDE_ESP EQU [Z80_CPU.Reg_DE_ESP]
RegD_ESP EQU [Z80_CPU.Reg_D_ESP]
RegE_ESP EQU [Z80_CPU.Reg_E_ESP]

RegHL_ESP EQU [Z80_CPU.Reg_HL_ESP]
RegH_ESP EQU [Z80_CPU.Reg_H_ESP]
RegL_ESP EQU [Z80_CPU.Reg_L_ESP]

RegSP EQU [Z80_CPU.Reg_SP]

RegIY EQU [Z80_CPU.Reg_IY]
RegIX EQU [Z80_CPU.Reg_IX]
RegIXH EQU [Z80_CPU.Reg_IXH]
RegIXL EQU [Z80_CPU.Reg_IXL]
RegIYH EQU [Z80_CPU.Reg_IYH]
RegIYL EQU [Z80_CPU.Reg_IYL]

;RegIY EQU [Z80_CPU.Reg_IY]
;RegIX EQU [Z80_CPU.Reg_IX]

RegIR EQU [Z80_CPU.Reg_IR]
RegI EQU [Z80_CPU.Reg_I]
RegR EQU [Z80_CPU.Reg_R]

IMF EQU [Z80_CPU.IMF]
IFF1 EQU [Z80_CPU.IFF1]
IFF2 EQU [Z80_CPU.IFF2]
NMI_LINE EQU [Z80_CPU.NMI_LINE]
INT_LINE EQU [Z80_CPU.INT_LINE]
D_LINE EQU [Z80_CPU.D_LINE]
A_LINE EQU [Z80_CPU.A_LINE]
HALT EQU [Z80_CPU.HALT]

Reg_Tmp EQU [Z80_CPU.Reg_Tmp]
Reg_Tmp16 EQU [Z80_CPU.Reg_Tmp16]
Reg_Tmp8 EQU [Z80_CPU.Reg_Tmp8]
#include "ULA.h"

extern "C" void __stdcall Inst_IN_Impl(unsigned short int addr, unsigned char* value) {
	
	ula_read(addr, value);
}

extern "C" void __stdcall Inst_OUT_Impl(unsigned short int addr, unsigned char value) {

	addr = addr & 0x00FF;
	switch (addr) {
	case 0xFE:
		ula_write(value);
		break;
	default:
		break;
	}
}
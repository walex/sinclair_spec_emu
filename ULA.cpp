#include "ULA.h"
#include "display.h"
#include "keyboard.h"

const int PAL_FPS = 1000 / 25;
/*

Bit   7   6   5   4   3   2   1   0
    +-------------------------------+
    |   |   |   | E | M |   Border  |
    +-------------------------------+
*/

ULA_PARAMS sUlaParams;
void ula_init(unsigned char* rom) {

	sUlaParams.videoRefreshRate = PAL_FPS;
	sUlaParams.rom = rom;
	keyboard_init(sUlaParams);
	display_init(sUlaParams);
}

void ula_read(unsigned short int addr, unsigned char* value) {

	unsigned char key;
	switch (addr) {
	case 0xFEFE:
	case 0xFDFE:
	case 0xFBFE:
	case 0xF7FE:
	case 0xEFFE:
	case 0xDFFE:
	case 0xBFFE:		
	case 0x7FFE:
		key = ((addr & 0xFF00) >> 8);
		*value = keyboard_getKeyMap(key);
		break;
	default:
		*value = 0;
		break;
	}
}

void ula_write(unsigned char value) {

   // border = value & 0x7;
   // mic = value & 0x8;
   // ear = value & 0x10;

    display_setBorder(value & 0x7);
}


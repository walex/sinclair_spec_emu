#include "io.h"
#include "display.h"
#include "WindowsTools.h"

static const int kScanConvert[192] = {
   0,8,16,24,32,40,48,56,
   1,9,17,25,33,41,49,57,
   2,10,18,26,34,42,50,58,
   3,11,19,27,35,43,51,59,
   4,12,20,28,36,44,52,60,
   5,13,21,29,37,45,53,61,
   6,14,22,30,38,46,54,62,
   7,15,23,31,39,47,55,63,

   64,72,80,88,96,104,112,120,
   65,73,81,89,97,105,113,121,
   66,74,82,90,98,106,114,122,
   67,75,83,91,99,107,115,123,
   68,76,84,92,100,108,116,124,
   69,77,85,93,101,109,117,125,
   70,78,86,94,102,110,118,126,
   71,79,87,95,103,111,119,127,

   128,136,144,152,160,168,176,184,
   129,137,145,153,161,169,177,185,
   130,138,146,154,162,170,178,186,
   131,139,147,155,163,171,179,187,
   132,140,148,156,164,172,180,188,
   133,141,149,157,165,173,181,189,
   134,142,150,158,166,174,182,190,
   135,143,151,159,167,175,183,191
};

static const unsigned int KVideoColorPallete[8] = { 0,0xfd0000,0x0000fd,0xfd00fd,0x00fd00,0xfdfd00,0x00fdfd,0xfdfdfd };

static const int kSpectrumResolutionX = 256;
static const int kSpectrumResolutionY = 192;


void display_draw(unsigned char* mem, OnScreenValueCB onScreenValue, DrawScreenBlitCB onScreenBlt)
{
	unsigned char* mem_atrib_video = mem + 22528; // 0x5800
	unsigned char* mem_video = mem + 0x4000;
	unsigned char mask = 0x80;
	unsigned char byte;
	unsigned long ink, paper;
	int i, xPos;

	for (int yPos = 0; yPos < kSpectrumResolutionY; yPos++) {
		xPos = 0;
		for (int jPos = 0;jPos < 32;jPos++)
		{
			mask = 0x80;
			byte = *(mem_video + ((kScanConvert[yPos]) << 5) + jPos);
			i = (yPos / 8) * jPos;
			ink = KVideoColorPallete[(((unsigned char)*(mem_atrib_video + i)) & 0x7)];
			paper = KVideoColorPallete[((((unsigned char)*(mem_atrib_video + i)) & 0x38) >> 3)];
			for (int r = 0;r < 8;r++)
			{
				onScreenValue((xPos + r), yPos, byte & mask ? ink : paper);
				mask >>= 1;
			}
			xPos += 8;
		}
	}

	onScreenBlt();
}

void display_vSync(unsigned char* mem) {

	trigger_MI();
}

void display_init(ULA_PARAMS& ulaParams) {
	
	ulaParams.onVideoSignal = display_draw;
	ulaParams.onVideoVSync = display_vSync;
	window_main_loop(&ulaParams);
}


void display_setBorder(unsigned char color) {

	window_set_border(KVideoColorPallete[color]);
}
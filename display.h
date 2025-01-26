#ifndef __display_h__

#define __display_h__

#include "ULA.h"

void display_init(ULA_PARAMS& ulaParams);
void display_draw(unsigned char* mem, OnScreenValueCB onScreenValue, DrawScreenBlitCB onScreenBlt);
void display_setBorder(unsigned char color);
void display_vSync(unsigned char* mem);

#endif
#ifndef __keyboard_h__
#define __keyboard_h__

void keyboard_init(ULA_PARAMS& ulaParams);
void keyboard_setKeyMap(unsigned short int keyValue);
unsigned char keyboard_getKeyMap(unsigned char addr);

#endif
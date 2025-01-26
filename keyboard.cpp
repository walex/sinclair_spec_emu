#include "ULA.h"
#include "keyboard.h"
#include <map>

/*
____________________________________
 PORT|            BIT              |
------------------------------------ 
-----|  0  |  1  |  2  |  3  |  4  |
-----|-----|-----|-----|-----|-----|
FEFE |shift|  Z	 |	X  |  C	 |	V  |
------------------------------------
FDFE |	A  |  S	 |	D  |  F  |	G  |
------------------------------------
FBFE |	Q  |  W	 |	E  |  R	 |	T  |
------------------------------------
F7FE |  1  |  2  |	3  |  4	 |	5  |
------------------------------------
EFFE |	0  |  9  |	8  |  7  |	6  |
------------------------------------
DFFE |	P  |  O  |	I  |  U  |	Y  |
------------------------------------
BFFE |enter|  L	 |	K  |  J  |	H  |
------------------------------------
7FFE |space| sym |	M  |  N  |	B  |
------------------------------------
*/ 

std::map<unsigned char, unsigned char> zxKeyMap = {
{0xFE, 0},
{0xFD, 0},
{0xFB, 0},
{0xF7, 0},
{0xEF, 0},
{0xDF, 0},
{0xBF, 0},
{0x7F, 0}
};

void keyboard_setKeyMap(unsigned short int keyValue) {
	
	unsigned char key = (keyValue & 0xFF00) >> 8;
	unsigned char newValue = (keyValue & 0xFF);
	unsigned char& oldValue = zxKeyMap[key];
	oldValue = oldValue | newValue;
}

unsigned char keyboard_getKeyMap(unsigned char addr) {

	return ~(zxKeyMap[addr]);
}

void keyboard_keyPressed(std::vector<unsigned char>* keyMap, int pressed) {

	if (keyMap) {
		unsigned char key = keyMap->at(0);
		unsigned char pos = keyMap->at(1);
		unsigned char data = zxKeyMap.at(key);
		if (pressed)
			data = data | (unsigned char)(1 << pos);
		else
			data = 0;
		zxKeyMap.at(key) = data;		
	}

}

void keyboard_init(ULA_PARAMS& ulaParams) {

	ulaParams.onKeyPressed = keyboard_keyPressed;
}
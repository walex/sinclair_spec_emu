#ifndef __ULA_H__
#define __ULA_H__

#include <functional>
#include <vector>

using OnScreenValueCB = std::function<void(int x, int y, unsigned long color)>;
using DrawScreenBlitCB = std::function<void()>;
using OnVideoSignal = std::function<void(unsigned char* rom, OnScreenValueCB onScreenValue, DrawScreenBlitCB onScreenBlt)>;
using OnVideoVSync = std::function<void(unsigned char* rom)>;
using OnKeyPressed = std::function<void(std::vector<unsigned char>* keyMap, int pressed)>;

struct ULA_PARAMS {

    unsigned char* rom;
    OnVideoSignal onVideoSignal;
    OnVideoVSync onVideoVSync;    
    int videoRefreshRate;
    OnKeyPressed onKeyPressed;
};

void ula_init(unsigned char* rom);
void ula_read(unsigned short int addr, unsigned char* value);
void ula_write(unsigned char value);

#endif
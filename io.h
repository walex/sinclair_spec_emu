#ifndef __io_h__

#define __io_h__

#define INT_NONE	0
#define INT_MI		1
#define INT_NMI		2

extern "C" void __stdcall trigger_NMI();
extern "C" void __stdcall trigger_MI();

#ifdef TEST_CPU

unsigned char get_io_next_test_data(unsigned short portId);
void put_io_next_test_data(unsigned short portId, unsigned char data);
void clear_io_test_data();

#endif

#endif
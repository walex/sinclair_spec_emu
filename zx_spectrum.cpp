#ifndef TEST_CPU

#include <Windows.h>
#include <stdio.h>
#include <thread>
#include <chrono>
#include <time.h>
#include "ULA.h"

extern "C" int __stdcall Z80CPU(unsigned char*);

static double kZ80CpuCycleUsecs = 0;
static LARGE_INTEGER kHostCpuFreq;
static LARGE_INTEGER kStartTimer;
static  HANDLE kTimer;

const char* SPECY_ROM_PATH = "..\\..\\spec_48.rom";
const size_t RAM_48K_SIZE = 48 * 1024;
const size_t ROM_16K_SIZE = 16 * 1024;
const double SPEC_CPU_FREQ = 3.5;

double calculate_cycle_time_usecs(double clock_frequency_mhz) {
	// Convert clock frequency from MHz to Hz
	double clock_frequency_hz = clock_frequency_mhz * 1e6;

	// Calculate time per cycle in seconds
	double time_per_cycle_seconds = 1.0 / clock_frequency_hz;

	// Convert to microseconds (1 second = 1,000,000 microseconds)
	double time_per_cycle_microseconds = time_per_cycle_seconds * 1e6;

	return time_per_cycle_microseconds;
}

extern "C" void __stdcall CreateTimer(double clock_frequency_mhz) {

	kZ80CpuCycleUsecs = calculate_cycle_time_usecs(clock_frequency_mhz);
	QueryPerformanceFrequency(&kHostCpuFreq); // Get the high-resolution timer frequency	 
	kTimer = CreateWaitableTimer(NULL, TRUE, NULL);
	QueryPerformanceCounter(&kStartTimer);      // Get the starting counter value
}

extern "C" void __stdcall DestroyTimer() {

	CloseHandle(kTimer);
}

extern "C" void __stdcall EmulateOpcodeTimeCB(unsigned short cycles, unsigned short t_states, unsigned char op) {

	printf("opcode called %d %d %0x\n", t_states, cycles, (int)op);

	double microseconds = ((double)t_states) * ((double)cycles) * kZ80CpuCycleUsecs;

	LARGE_INTEGER end;

	double target_ticks = (microseconds * kHostCpuFreq.QuadPart) / 1e6; // Convert microseconds to ticks

	do {
		QueryPerformanceCounter(&end);    // Get the current counter value
	} while ((end.QuadPart - kStartTimer.QuadPart) < target_ticks);

	QueryPerformanceCounter(&kStartTimer);
}

extern "C" void __stdcall PrintType(const char* str) {

	printf("%s\n", str);

}

unsigned char* loadRom(const char* path, size_t ramSize) {

	FILE* rom = nullptr;
	fopen_s(&rom, path, "rb");
	if (rom == nullptr) {
		perror("Error opening file");
		return nullptr;
	}

	fseek(rom, 0, SEEK_END); // Move the file pointer to the end
	size_t romSize = ftell(rom);
	fseek(rom, 0, SEEK_SET);

	size_t totalMem = romSize + ramSize;
	unsigned char *mem = (unsigned char*)malloc(totalMem);
	if (mem == nullptr) {

		perror("RAM memory error");
		fclose(rom);
		return nullptr;
	}
	memset(mem, 0, totalMem);
	fread(mem, romSize, 1, rom);
	fclose(rom);

	return mem;
}

std::thread* _T; // put in ula.cpp

int main(int argc, char* argv[]) {
	
	unsigned char* rom = loadRom(SPECY_ROM_PATH, RAM_48K_SIZE);
	if (!rom) {
		perror("cannot load rom file");
		return -1;
	}
	printf("ROM %p\n", (void*)rom);
	CreateTimer(SPEC_CPU_FREQ);	

	_T = new std::thread([&]() {

		ula_init(rom);
		
	});
	Z80CPU(rom);
	free(rom);
	DestroyTimer();
	return 0;
}

#endif

/*

void daa()
{
   int t;

   t=0;

   // 4 T states
   T(4);

   if(flags.H || ((A & 0xF) > 9) )
		 t++;

   if(flags.C || (A > 0x99) )
   {
		 t += 2;
		 flags.C = 1;
   }

   // builds final H flag

   if (flags.N) {
	
	if (flags.H) {
		
		flags.H = (((A & 0x0F)) < 6);
	}
   } else {
	flags.H = ((A & 0x0F) >= 0x0A);
   }

   switch(t)
   {
		case 1:
			A += (flags.N)?0xFA:0x06; // -6:6
			break;
		case 2:
			A += (flags.N)?0xA0:0x60; // -0x60:0x60
			break;
		case 3:
			A += (flags.N)?0x9A:0x66; // -0x66:0x66
			break;
   }

   flags.S = (A & BIT_7);
   flags.Z = !A;
   flags.P = parity(A);
   flags.X = A & BIT_5;
   flags.Y = A & BIT_3;
}

*/
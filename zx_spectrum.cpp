#ifndef TEST_CPU

// http://www.breakintoprogram.co.uk/hardware/computers/zx-spectrum/memory-map
// https://zx.remysharp.com/tools/
#define NOMINMAX 
#include <Windows.h>
#include <stdio.h>
#include <thread>
#include <chrono>
#include <time.h>
#include "ULA.h"
#include <fstream>
#include "tape.h"

extern "C" int __stdcall Z80CPU(unsigned char*, double z80CycleTimeNano, double hostCPUCycleTimeNano, unsigned char* assemble);

static double kZ80CpuCycleUsecs = 0;
static LARGE_INTEGER kHostCpuFreq;
static LARGE_INTEGER kStartTimer;
static  HANDLE kTimer;

const char* SPECY_ROM_PATH = "..\\..\\spec_48.rom";
const char* TK95_ROM_PATH = "..\\..\\tk95.rom";
const size_t RAM_48K_SIZE = 48 * 1024;
const size_t ROM_16K_SIZE = 16 * 1024;
const double SPEC_CPU_FREQ = 3.5;

double calculate_cycle_time_nanosecs(double clock_frequency_mhz) {
	// Convert clock frequency from MHz to Hz
	double clock_frequency_hz = clock_frequency_mhz * 1e6;

	// Calculate time per cycle in seconds
	double time_per_cycle_seconds = 1.0 / clock_frequency_hz;

	// Convert to microseconds (1 second = 1,000,000 microseconds)
	double time_per_cycle_nanoeconds = time_per_cycle_seconds * 1e9;

	return time_per_cycle_nanoeconds;
}

extern "C" void __stdcall CreateTimer(double clock_frequency_mhz) {

	kZ80CpuCycleUsecs = calculate_cycle_time_nanosecs(clock_frequency_mhz);
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

const char* SAMPLE_COLORS_PATH = "..\\..\\sample.tap";
const char* MANIC_MINER_TAP_PATH = "..\\..\\manic_miner.tap";
const char* BATMAN_TAP_PATH = "..\\..\\batman.tap";
const char* Z80_ZEXALL_TAP_PATH = "..\\..\\zexall.tap";
const char* Z80_ZEXALL_BIN_PATH = "..\\..\\zexall.bin";
const char* Z80_ZEXDOC_BIN_PATH = "..\\..\\zexdoc.bin";
// https://github.com/raxoft/z80test
const char* Z80_TEST_SUIT_DOC__BIN_PATH = "C:\\Users\\wadrw\\Documents\\develop\\projects\\personal\\z80test\\src\\z80doc.out";

const char* CURRENT_BIN_TEST_PATH = Z80_TEST_SUIT_DOC__BIN_PATH;

void runBinTest(unsigned char* mem) {

	FILE* rom = nullptr;
	if (!fopen_s(&rom, CURRENT_BIN_TEST_PATH, "rb") && rom) {
		fseek(rom, 0, SEEK_END); // Move the file pointer to the end
		size_t romSize = ftell(rom);
		fseek(rom, 0, SEEK_SET);
		fread(mem + 0x8000, romSize, 1, rom);
		fclose(rom);
	}
}

unsigned char* createMemory(const char* romPath, size_t ramSize, size_t* romSize) {

	*romSize = 0;

	FILE* rom = nullptr;
	fopen_s(&rom, romPath, "rb");
	if (rom == nullptr) {
		perror("Error opening file");
		return nullptr;
	}

	fseek(rom, 0, SEEK_END); // Move the file pointer to the end
	size_t l = ftell(rom);
	fseek(rom, 0, SEEK_SET);

	size_t totalMem = l + ramSize;
	unsigned char *mem = (unsigned char*)malloc(totalMem);
	if (mem == nullptr) {

		perror("RAM memory error");
		fclose(rom);
		return nullptr;
	}
	memset(mem, 0, totalMem);
	fread(mem, l, 1, rom);
	fclose(rom);

	*romSize = l;
	return mem;
}

std::thread* _T; // put in ula.cpp
std::thread* _T2; // put in ula.cpp

int main(int argc, char* argv[]) {
	
	size_t romSize;
	unsigned char* mem = createMemory(SPECY_ROM_PATH, RAM_48K_SIZE, &romSize);
	if (!mem) {
		perror("cannot load rom file");
		return -1;
	}
	
	printf("ROM %p\n", (void*)mem);
	
	
	//CreateTimer(SPEC_CPU_FREQ);	

	_T = new std::thread([&]() {

		
		ula_init(mem);
		
		
	});

	
	_T2 = new std::thread([&]() {

		std::this_thread::sleep_for(std::chrono::seconds(5));
		//loadTap(SAMPLE_COLORS_PATH, mem);
		runBinTest(mem);
		//tape_load_from_file(MANIC_MINER_TAP_PATH);
		});
		

		// Get the initial time stamp counter value
	unsigned long long startCycles = __rdtsc();  // rdtsc provides the number of cycles

	// Sleep for a known amount of time (e.g., 1 second)
	Sleep(1000);  // Sleep for 1 second

	// Get the final time stamp counter value
	unsigned long long endCycles = __rdtsc();

	// Calculate the difference in cycles
	unsigned long long cycleDifference = endCycles - startCycles;

	// The number of nanoseconds in one second is 1e9
	double cyclesPerSecond = cycleDifference;  // This is the total cycles over 1 second
	double hostCpuCycleTimeInNanoseconds = 1.0 / (cyclesPerSecond / 1e9);
	double z80CpuCycleTimeInNanoseconds = calculate_cycle_time_nanosecs(SPEC_CPU_FREQ);
	Z80CPU(mem, z80CpuCycleTimeInNanoseconds, hostCpuCycleTimeInNanoseconds, 0);
	free(mem);
	//DestroyTimer();
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
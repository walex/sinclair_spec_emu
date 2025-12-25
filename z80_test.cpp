#ifdef TEST_CPU
#define _CRT_SECURE_NO_WARNINGS
// https://worldofspectrum.org/ZXBasicManual/
// https://zx.remysharp.com/tools/
// https://github.com/SingleStepTests/z80/tree/main/v1
#define NOMINMAX 
#include <Windows.h>
#include <stdio.h>
#include <condition_variable>
#include <thread>
#include <chrono>
#include <time.h>
#include "ULA.h"
#include "io.h"
#include <fstream>
#include "kaitai/kaitaistream.h"
#include "zx_spectrum_tap.h"
#include "optable.h"

extern "C" int __stdcall Z80CPU(unsigned char*, double z80CycleTimeNano, double hostCPUCycleTimeNano, unsigned char* assemble);
extern "C" void __stdcall InitRegisters(unsigned short*);
extern "C" void __stdcall GetRegisters(unsigned short*);

static double kZ80CpuCycleUsecs = 0;
static LARGE_INTEGER kHostCpuFreq;
static LARGE_INTEGER kStartTimer;
static  HANDLE kTimer;
static double kZ80CpuCycleTimeInNanoseconds;
static double kHostCpuCycleTimeInNanoseconds;

const char* SPECY_ROM_PATH = "..\\..\\spec_48.rom";
const char* Z80_ZEXALL_TAP_PATH = "..\\..\\zexall.tap";
const char* Z80_ZEXALL_BIN_PATH = "..\\..\\zexall.bin";
const char* Z80_ZEXDOC_BIN_PATH = "..\\..\\zexdoc.bin";
const char* MANIC_MINER_PATH = "..\\..\\manic_miner.tap";
const char* TEST_CASES_PATH = "C:\\Users\\wadrw\\Documents\\develop\\projects\\personal\\sinclair_spec_emu\\test\\json";
const size_t ROM_128K_SIZE = 128 * 1024;
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

extern "C" void __stdcall PrintType(const char* str) {

	printf("%s\n", str);

}

void loadZexallBin(unsigned char* mem) {

   FILE* rom = nullptr;
   fopen_s(&rom, Z80_ZEXDOC_BIN_PATH, "rb");
   if (rom){
	   fseek(rom, 0, SEEK_END); // Move the file pointer to the end
	   size_t romSize = ftell(rom);
	   fseek(rom, 0, SEEK_SET);
	   fread(mem + 0x8000, romSize, 1, rom);
	   fclose(rom);
   }
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
	unsigned char* mem = (unsigned char*)malloc(totalMem);
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

/*

Spectrum RAM Map
  4000h  VRAM Bitmap     (256x192 pixels)
  5800h  VRAM Attributes (32x24 characters)
  5B00h  System Area
  5CB6h  Memory (starting with CHANS)
Memory
  CHANS    channels      (usually at 5CB6h)
  PROG     basic program (usually at 5CCBh)
  VARS     basic variables
  E_LINE   input buffer
  WORKSP   temporary work space
  STKBOT   bottom of calculator stack (same as WORKSP when empty)
  STKEND   start of spare space (same as STKBOT when empty)
  RAMTOP   CPU stacktop+1 (usually FF57h)
  UDG      User-defined graphics (charset) (usually FF58h)
  P_RAMT   physical RAM top (FFFFh for 48K RAM, or 7FFFh for 16K RAM)
Variants
Expansion hardware like Interface 1 and Beta Disk may allocate
memory between 5CB6h and CHANS. Other hardware like Currah uSpeech
 may allocate memory between RAMTOP and UDG.

*/


#define MAX_LINE_LENGTH 512
#define REGISTERS_COUNT 25
enum reg_indices {
	pc = 0,
	sp,
	a,
	b,
	c,
	d,
	e,
	f,
	h,
	l,
	i,
	r,
	ei,
	wz,
	ix,
	iy,
	af_,
	bc_,
	de_,
	hl_,
	im,
	p,
	q,
	iff1,
	iff2
};

const char* reg_names[] = { "pc",
"sp",
"a",
"b",
"c",
"d",
"e",
"f",
"h",
"l",
"i",
"r",
"ei",
"wz",
"ix",
"iy",
"af_",
"bc_",
"de_",
"hl_",
"im",
"p",
"q",
"iff1",
"iff2"
};

void getZ80AndHostCPUFreq(double& z80CpuCycleTimeInNanoseconds, double& hostCpuCycleTimeInNanoseconds) {
	// Get the initial time stamp counter value
	unsigned long long startCycles = __rdtsc();  // rdtsc provides the number of cycles

	// Sleep for a known amount of time (e.g., 1 second)
	Sleep(1000);  // Sleep for 1 second

	// Get the final time stamp counter value
	unsigned long long endCycles = __rdtsc();

	// Calculate the difference in cycles
	unsigned long long cycleDifference = endCycles - startCycles;

	// The number of nanoseconds in one second is 1e9
	double cyclesPerSecond = (double)cycleDifference;  // This is the total cycles over 1 second
	hostCpuCycleTimeInNanoseconds = 1.0 / (cyclesPerSecond / 1e9);
	z80CpuCycleTimeInNanoseconds = calculate_cycle_time_nanosecs(SPEC_CPU_FREQ);
}

void print_registers(const std::vector<unsigned short int>& regs) {

	for (int i = 0; i < regs.size(); i++) {

		printf("%s -> %X\n", reg_names[i], regs[i]);
	}
}

void test_opcode(const char* test_name, unsigned char* mem, std::vector<unsigned short int>& parameters, bool print_regs = false) {

	// Create a new vector with the first REGISTERS_COUNT elements
	std::vector<unsigned short int> init_data(parameters.begin(), parameters.begin() + REGISTERS_COUNT);
	std::vector<unsigned short int> result_data(REGISTERS_COUNT);

	InitRegisters(init_data.data());

	// Erase the first REGISTERS_COUNT elements from the original vector
	parameters.erase(parameters.begin(), parameters.begin() + REGISTERS_COUNT);
	
	// Copy initial ram data
	unsigned short int ram_positions_count = (parameters[0] * 2);
	// Erase ram_positions_count from parameters
	parameters.erase(parameters.begin(), parameters.begin() + 1);
	for (unsigned short int i = 0; i < ram_positions_count; i+=2) {

		mem[parameters[i]] = (unsigned char)parameters[i + 1];
	}
	
	// Erase initial ram data so parametes will have only the last REGISTERS_COUNT elements + result ram data + port data
	parameters.erase(parameters.begin(), parameters.begin() + ram_positions_count);

	// read port data
	clear_io_test_data();
	// Copy port data
	unsigned short int port_positions_count = (parameters[0] * 3);
	// Erase port_positions_count from parameters
	parameters.erase(parameters.begin(), parameters.begin() + 1);
	// init port mem
	for (unsigned short int i = 0; i < port_positions_count; i += 3) {

		put_io_next_test_data(parameters[i], (unsigned char)parameters[i + 1]);
	}

	// Erase port data so parametes will have only the last REGISTERS_COUNT elements + result ram data
	parameters.erase(parameters.begin(), parameters.begin() + port_positions_count);

	// get cpus cycle time
	int r = Z80CPU(mem, kZ80CpuCycleTimeInNanoseconds, kHostCpuCycleTimeInNanoseconds, (unsigned char *)0x8000);
	
	// evaluate if is a invalid o not emulated opcode
	if (r == -1 && (test_name[0] != '0' || test_name[1] != '0')) {
		return;
	}

	GetRegisters(result_data.data());
	int errcnt = 0;
	for (int i = 0; i < init_data.size(); i++) {
		
		if ( i == (int)wz || i == (int)ei || i == (int)p || i == (int)q)
			continue;
		if (i == (int)f) {

			// ignore undoc flags bits for the moment...
			result_data[i] = result_data[i] & 0xD7;
			parameters[i] = parameters[i] & 0xD7;
		}
		if (result_data[i] != parameters[i]) {
			printf("Error opcode %s register  %s is not equal result: %d test case: %d\n", test_name, reg_names[i], result_data[i], parameters[i]);
			errcnt++;
		}
	}

	// Erase the next REGISTERS_COUNT elements from the original vector
	parameters.erase(parameters.begin(), parameters.begin() + REGISTERS_COUNT);

	// Get initial ram data
	ram_positions_count = (parameters[0] * 2);
	// Erase ram_positions_count from parameters
	parameters.erase(parameters.begin(), parameters.begin() + 1);
	for (unsigned short int i = 0; i < ram_positions_count; i += 2) {
		
		if (mem[parameters[i]] != parameters[i + 1]) {

			printf("Error opcode %s memory %d is not equal result: %d test case: %d\n", test_name, i, mem[parameters[i]], parameters[i + 1]);
			errcnt++;
		}
	}

	if (errcnt == 0) {

		//printf("Test passed ok!\n");
	}
	else {

		if (print_regs)
		{
			printf("--------------- Init data --------------------------\n");
			print_registers(init_data);
			printf("--------------- Result data --------------------------\n");
			print_registers(result_data);
			printf("-----------------------------------------\n");
		}
	}
}

#include <filesystem>
#include <iostream>

void run_test_case(FILE* file, double kZ80CpuCycleTimeInNanoseconds, double kHostCpuCycleTimeInNanoseconds) {

	unsigned char mem[ROM_128K_SIZE];
	
	char line[MAX_LINE_LENGTH+1];
	
	// Read each line from the file
	while (fgets(line, sizeof(line), file)) {

		static int op_cnt = 0;
		++op_cnt;		

		/*if (op_cnt == 0x00002329) {
			op_cnt = op_cnt;
		}*/

		std::vector<unsigned short int> parameters;
		parameters.reserve((REGISTERS_COUNT * 2) + 100);
		memset(mem, 0, ROM_128K_SIZE);
		char* token = strtok(line, ";");  // Split by space, tab, or newline
		char test_name[64];
		int param_cnt = 0;
		bool show_info = false;
		const char* show_test_name = "ED 4A 0000";
		while (token != NULL) {
			if (param_cnt++ == 0) {
				
				strcpy(test_name, token);
				token = strtok(NULL, ";");

				show_info = !strcmp(test_name, show_test_name);

				continue;
			}
			char* endptr;
			parameters.push_back((unsigned int)strtoul(token, &endptr, 10));
			token = strtok(NULL, ";");
		}
		test_opcode(test_name, mem, parameters, show_info);
	}
	
}

void process_tests() {

	getZ80AndHostCPUFreq(kZ80CpuCycleTimeInNanoseconds, kHostCpuCycleTimeInNanoseconds);

	const std::string path = TEST_CASES_PATH;
	for (const auto& entry : std::filesystem::directory_iterator(path)) {
		if (entry.path().extension() == ".txt") {
			std::string test_path = entry.path().string().c_str();
			FILE* file = fopen(test_path.c_str(), "r");
			if (file == NULL) {
				printf("Error opening file %s.\n", test_path.c_str());
				return;
			}
			run_test_case(file, kZ80CpuCycleTimeInNanoseconds, kHostCpuCycleTimeInNanoseconds);

			fclose(file);
		}
	}

}

int run_zexall_test() {
	
	getZ80AndHostCPUFreq(kZ80CpuCycleTimeInNanoseconds, kHostCpuCycleTimeInNanoseconds);
	unsigned char* mem = loadRom(SPECY_ROM_PATH, ROM_128K_SIZE);
	loadZexallBin(mem);
	int r = Z80CPU(mem, kZ80CpuCycleTimeInNanoseconds, kHostCpuCycleTimeInNanoseconds, (unsigned char*)0x8000);
	free(mem);
	return 0;
}

int main(int argc, char* argv[]) {
	
	
	//return run_zexall_test();
	process_tests();
	//return load_tap();
	return 0;
}

extern "C" void __stdcall print_text(const char* str) {
		printf("%s\n", str);
}
#endif

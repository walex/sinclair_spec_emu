#ifdef TEST_CPU
#define _CRT_SECURE_NO_WARNINGS
// https://worldofspectrum.org/ZXBasicManual/
// https://zx.remysharp.com/tools/
// https://github.com/SingleStepTests/z80/tree/main/v1
#define NOMINMAX 
#include <Windows.h>
#include <stdio.h>
#include <thread>
#include <chrono>
#include <time.h>
#include "ULA.h"
#include <fstream>
#include "kaitai/kaitaistream.h"
#include "zx_spectrum_tap.h"

extern "C" void __stdcall Z80CPU(unsigned char*);
extern "C" void __stdcall InitRegisters(unsigned short*);
extern "C" void __stdcall GetRegisters(unsigned short*);

static double kZ80CpuCycleUsecs = 0;
static LARGE_INTEGER kHostCpuFreq;
static LARGE_INTEGER kStartTimer;
static  HANDLE kTimer;

const char* SPECY_ROM_PATH = "..\\..\\spec_48.rom";
const char* Z80_TEST_PATH = "..\\..\\zexall.tap";
const char* MANIC_MINER_PATH = "..\\..\\manic_miner.tap";
const char* TEST_CASES_PATH = "..\\..\\..\\tests\\test_cases.txt";
const size_t ROM_128K_SIZE = 128 * 1024;
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

void loadTap(unsigned char* mem) {

	std::ifstream is(MANIC_MINER_PATH, std::ifstream::binary);
	kaitai::kstream ks(&is);
	zx_spectrum_tap_t data(&ks);
	auto& blocks = *data.blocks();
	//zx_spectrum_tap_t::header_type_enum_t t = blocks[0]->header()->header_type();
	//t = blocks[1]->header()->header_type();
	const std::string& code_basic = blocks[0]->data();
	const std::string& code_bin = blocks[1]->data();
	const std::string& code_bin2 = blocks[2]->data();
	zx_spectrum_tap_t::program_params_t* p_params = (zx_spectrum_tap_t::program_params_t*)blocks[0]->header()->params();
	zx_spectrum_tap_t::bytes_params_t* c_params = (zx_spectrum_tap_t::bytes_params_t*)blocks[1]->header()->params();
	zx_spectrum_tap_t::bytes_params_t* c_params2 = (zx_spectrum_tap_t::bytes_params_t*)blocks[2]->header()->params();
//	memcpy(mem + 0x5CCB, code_basic.data(), p_params->len_program());
	//memcpy(mem + c_params->start_address(), code_bin.data(), code_bin.size());
	memcpy(mem + c_params2->start_address(), code_bin2.data(), code_bin2.size());
}

std::thread* _T; // put in ula.cpp

int load_tap() {

	unsigned char* mem = loadRom(SPECY_ROM_PATH, RAM_48K_SIZE);
	if (!mem) {
		perror("cannot load rom file");
		return -1;
	}
	printf("ROM %p\n", (void*)mem);

	loadTap(mem);

	CreateTimer(SPEC_CPU_FREQ);

	_T = new std::thread([&]() {

		ula_init(mem);

		});
	Z80CPU(mem);
	free(mem);
	DestroyTimer();
	return 0;
}


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

void test_opcode(const char* test_name, unsigned char* mem, std::vector<unsigned short int>& parameters) {
	
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
	
	// Erase initial ram data so parametes will have only the last REGISTERS_COUNT elements + result ram data
	parameters.erase(parameters.begin(), parameters.begin() + ram_positions_count);

	
	Z80CPU(mem);
	
	GetRegisters(result_data.data());
	int errcnt = 0;
	for (int i = 0; i < init_data.size(); i++) {
		
		if ( i == (int)wz || i == (int)ei || i == (int)q)
			continue;
		if (i == (int)f) {

			// ignore undoc flags for the moment...
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
}

int run_test_cases() {

	unsigned char mem[ROM_128K_SIZE];
	

	FILE* file = fopen(TEST_CASES_PATH, "r");
	if (file == NULL) {
		printf("Error opening file.\n");
		return 1;
	}
		
	
	char line[MAX_LINE_LENGTH+1];
	
	int cnt = 0;
	// Read each line from the file
	while (fgets(line, sizeof(line), file)) {

		static int op_cnt = 0;
		++op_cnt;
		if (op_cnt == 51107)
			op_cnt = op_cnt;

		std::vector<unsigned short int> parameters;
		parameters.reserve((REGISTERS_COUNT * 2) + 10);
		memset(mem, 0, ROM_128K_SIZE);
		char* token = strtok(line, ";");  // Split by space, tab, or newline
		char test_name[64];
		int param_cnt = 0;
		while (token != NULL) {
			if (param_cnt++ == 0) {
				
				strcpy(test_name, token);
				token = strtok(NULL, ";");
				continue;
			}
			char* endptr;
			parameters.push_back((unsigned int)strtoul(token, &endptr, 10));
			token = strtok(NULL, ";");
		}
		
		test_opcode(test_name, mem, parameters);
		cnt++;
	}
	fclose(file);
	return 0;
}

int main(int argc, char* argv[]) {
	
	
	return run_test_cases();
}
#endif

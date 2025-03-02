#include "io.h"
#include <list>
#include <map>

#ifdef TEST_CPU

std::map<unsigned short, std::list<unsigned char>> io_test_data;

unsigned char get_io_next_test_data(unsigned short portId) {

	unsigned char result = 0;
	if (io_test_data.find(portId) != io_test_data.end()) {
		std::list<unsigned char>& dataPtr = io_test_data[portId];
		if (dataPtr.size() > 0) {
			result = dataPtr.front();
			dataPtr.pop_front();
		}
	}
	return result;
}

void put_io_next_test_data(unsigned short portId, unsigned char data) {

	unsigned char result = 0;
	if (io_test_data.find(portId) == io_test_data.end())
		io_test_data[portId] = {};
	std::list<unsigned char>& dataPtr = io_test_data[portId];
	dataPtr.push_back(data);
}

void clear_io_test_data() {
	
	io_test_data.clear();
}

#endif

extern "C" void __stdcall onDeviceIOEnd() {
}

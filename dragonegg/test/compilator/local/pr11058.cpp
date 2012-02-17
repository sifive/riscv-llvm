#include <inttypes.h>
typedef uint8_t           Uint8;
typedef uint16_t           Uint16;
typedef uint32_t           Uint32;
typedef int16_t           Int16;
struct SelectiveAck
{
	Uint8 extension;
	Uint8 length;
	Uint8* bitmask;
}; 
bool Acked(const SelectiveAck* sack, Uint16 bit)
{
	// check bounds
	if (bit < 2 || bit > 8*sack->length + 1)
		return false;

	const Uint8* bitset = sack->bitmask;
	int byte = (bit - 2) / 8;
	int bit_off = (bit - 2) % 8;
	return bitset[byte] & (0x01 << bit_off);
}
Uint16 test(const SelectiveAck* sack)
{
	// A packet is lost if 3 packets have been acked after it
	Uint32 acked = 0;
	Int16 i = sack->length * 8 - 1;
	while (i >= 0 && acked < 3)
	{
		if (Acked(sack, i))
		{
			acked++;
			if (acked == 3)
				return i;
		}

		i--;
	}

	return 0;
}
int main() {
}

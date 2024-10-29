package nes

// 2KB of memory
type Mem struct {
	ram []uint8
}

type Memory interface {
	Zero()
	Read(addr uint16, readonly bool) (data uint8)
	Write(addr uint16, data uint8)
}

func NewMemory() Mem {
	return Mem{ram: make([]uint8, 2048)}
}

func (Mem *Mem) Zero() {
	Mem.ram[0] = 0x00
	for i := 1; i < 2048; i *= 2 {
		copy(Mem.ram[i:], Mem.ram[:i])
	}
}

func (Mem *Mem) Read16(addr uint16, readonly bool) uint8 {
	if 0x0000 <= addr && addr <= 0x1FFF {
		return Mem.ram[addr&0x07FF]
	}
	return 0x00
}

func (Mem *Mem) Write16(addr uint16, data uint8) {
	if 0x0000 <= addr && addr <= 0x1FFF {
		Mem.ram[addr&0x07FF] = data
	}
}

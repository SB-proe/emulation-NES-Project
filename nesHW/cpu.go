package nes

//Flags
const (
	C = 1 << iota // Carry
	Z             // Zero
	I             // Interrupts
	D             // Decimal
	B             // Break
	U             // Unused
	V             // Overflow
	N             // Negative
)

//CPU Registers
type Registers struct {
	ac uint8  // Accumulator
	x  uint8  // X Register
	y  uint8  // Y Register
	sr uint8  // Stack Register
	sp uint8  // Stack Pointer
	pc uint16 // Program Counter
}

func NewRegisters() (reg Registers) {
	reg = Registers{}
	reg.Zero()
	return
}

func (reg *Registers) Zero() {
	reg.ac = 0x00
	reg.x = 0x00
	reg.y = 0x00
	reg.sr = 0x04
	reg.sp = 0x00
	reg.pc = 0x0000
}

func (cpu *CPU) getFlag(f uint8) bool {
	return (cpu.reg.sr & f) > 0
}

func (cpu *CPU) setFlag(f uint8, b bool) {
	if b {
		cpu.reg.sr |= f
	} else {
		cpu.reg.sr &= ^f
	}
}

type CPU struct {
	reg     Registers //Registers
	M       uint8     //Current memory (for emulation)
	addr    uint16    //Address (for emulation)
	op      uint8     //Current OP code
	cycles  uint8     //Cycles in operation
	ram     Mem       //Memory
	opTable [256]OPS  //Table holding all opcodes
}

func newCPU() *CPU {
	cpu := CPU{}
	cpu.zeroCPU()
	cpu.ram = NewMemory()
	cpu.opFunctions()
	return &cpu
}

func (cpu *CPU) zeroCPU() {
	cpu.reg = NewRegisters()
	cpu.M = 0x00
	cpu.addr = 0x0000
	cpu.op = 0x00
	cpu.cycles = 0x00
}

type OPS struct {
	InstName        string
	addressingMode  func() uint8
	addrName        string
	instructionMode func() uint8
	ticks           uint8
}

func (c *CPU) opFunctions() {
	c.opTable = [256]OPS{
		//0								1								2								3								4								5								6								7								8								9								A								B								C								D								E								F
		{"BRK", c.brk, "imp", c.imp, 7}, {"ORA", c.ora, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ORA", c.ora, "zpg", c.zpg, 3}, {"ASL", c.asl, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"PHP", c.php, "imp", c.imp, 3}, {"ORA", c.ora, "imm", c.imm, 2}, {"ASL", c.asl, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ORA", c.ora, "abs", c.abs, 4}, {"ASL", c.asl, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BPL", c.bpl, "rel", c.rel, 2}, {"ORA", c.ora, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ORA", c.ora, "zpx", c.zpx, 4}, {"ASL", c.asl, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CLC", c.clc, "imp", c.imp, 2}, {"ORA", c.ora, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ORA", c.ora, "abx", c.abx, 4}, {"ASL", c.asl, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"JSR", c.jsr, "abs", c.abs, 6}, {"AND", c.and, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"BIT", c.bit, "zpg", c.zpg, 3}, {"AND", c.and, "zpg", c.zpg, 3}, {"ROL", c.rol, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"PLP", c.plp, "imp", c.imp, 4}, {"AND", c.and, "imm", c.imm, 2}, {"ROL", c.rol, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"BIT", c.bit, "abs", c.abs, 4}, {"AND", c.and, "abs", c.abs, 4}, {"ROL", c.rol, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BMI", c.bmi, "rel", c.rel, 2}, {"AND", c.and, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"AND", c.and, "zpx", c.zpx, 4}, {"ROL", c.rol, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"SEC", c.sec, "imp", c.imp, 2}, {"AND", c.and, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"AND", c.and, "abx", c.abx, 4}, {"ROL", c.rol, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"RTI", c.rti, "imp", c.imp, 6}, {"EOR", c.eor, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"EOR", c.eor, "zpg", c.zpg, 3}, {"LSR", c.lsr, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"PHA", c.pha, "imp", c.imp, 3}, {"EOR", c.eor, "imm", c.imm, 2}, {"LSR", c.lsr, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"JMP", c.jmp, "abs", c.abs, 3}, {"EOR", c.eor, "abs", c.abs, 4}, {"LSR", c.lsr, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BVC", c.bvc, "rel", c.rel, 2}, {"EOR", c.eor, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"EOR", c.eor, "zpx", c.zpx, 4}, {"LSR", c.lsr, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CLI", c.cli, "imp", c.imp, 2}, {"EOR", c.eor, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"EOR", c.eor, "abx", c.abx, 4}, {"LSR", c.lsr, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"RTS", c.rts, "imp", c.imp, 6}, {"ADC", c.adc, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ADC", c.adc, "zpg", c.zpg, 3}, {"ROR", c.ror, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"PLA", c.pla, "imp", c.imp, 4}, {"ADC", c.adc, "imm", c.imm, 2}, {"ROR", c.ror, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"JMP", c.jmp, "ind", c.ind, 5}, {"ADC", c.adc, "abs", c.abs, 4}, {"ROR", c.ror, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BVS", c.bvs, "rel", c.rel, 2}, {"ADC", c.adc, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ADC", c.adc, "zpx", c.zpx, 4}, {"ROR", c.ror, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"SEI", c.sei, "imp", c.imp, 2}, {"ADC", c.adc, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"ADC", c.adc, "abx", c.abx, 4}, {"ROR", c.ror, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"XXX", c.xxi, "xxa", c.xxa, 2}, {"STA", c.sta, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"STY", c.sty, "zpg", c.zpg, 3}, {"STA", c.sta, "zpg", c.zpg, 3}, {"STX", c.stx, "zpg", c.zpg, 3}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"DEY", c.dey, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"TXA", c.txa, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"STY", c.sty, "abs", c.abs, 4}, {"STA", c.sta, "abs", c.abs, 4}, {"STX", c.stx, "abs", c.abs, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BCC", c.bcc, "rel", c.rel, 2}, {"STA", c.sta, "idy", c.idy, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"STY", c.sty, "zpx", c.zpx, 4}, {"STA", c.sta, "zpx", c.zpx, 4}, {"STX", c.stx, "zpy", c.zpy, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"TYA", c.tya, "imp", c.imp, 2}, {"STA", c.sta, "aby", c.aby, 5}, {"TXS", c.txs, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"STA", c.sta, "abx", c.abx, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"LDY", c.ldy, "imm", c.imm, 2}, {"LDA", c.lda, "xid", c.xid, 6}, {"LDX", c.ldx, "imm", c.imm, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"LDY", c.ldy, "zpg", c.zpg, 3}, {"LDA", c.lda, "zpg", c.zpg, 3}, {"LDX", c.ldx, "zpg", c.zpg, 3}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"TAY", c.tay, "imp", c.imp, 2}, {"LDA", c.lda, "imm", c.imm, 2}, {"TAX", c.tax, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"LDY", c.ldy, "abs", c.abs, 4}, {"LDA", c.lda, "abs", c.abs, 4}, {"LDX", c.ldx, "abs", c.abs, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BCS", c.bcs, "rel", c.rel, 2}, {"LDA", c.lda, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"LDY", c.ldy, "zpx", c.zpx, 4}, {"LDA", c.lda, "zpx", c.zpx, 4}, {"LDX", c.ldx, "zpy", c.zpy, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CLV", c.clv, "imp", c.imp, 2}, {"LDA", c.lda, "aby", c.aby, 4}, {"TSX", c.txs, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"LDY", c.ldy, "abx", c.abx, 4}, {"LDA", c.lda, "abx", c.abx, 4}, {"LDX", c.ldx, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"CPY", c.cpy, "imm", c.imm, 2}, {"CMP", c.cmp, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CPY", c.cpy, "zpg", c.zpg, 3}, {"CMP", c.cmp, "zpg", c.zpg, 3}, {"DEC", c.dec, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"INY", c.iny, "imp", c.imp, 2}, {"CMP", c.cmp, "imm", c.imm, 2}, {"DEX", c.dex, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CPY", c.cpy, "abs", c.abs, 4}, {"CMP", c.cmp, "abs", c.abs, 4}, {"DEC", c.dec, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BNE", c.bne, "rel", c.rel, 2}, {"CMP", c.cmp, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CPM", c.cmp, "zpx", c.zpx, 4}, {"DEC", c.dec, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CLD", c.cld, "imp", c.imp, 2}, {"CMP", c.cmp, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CMP", c.cmp, "abx", c.abx, 4}, {"DEC", c.dec, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"CPX", c.cpx, "imm", c.imm, 2}, {"SBC", c.sbc, "xid", c.xid, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CPX", c.cpx, "zpg", c.zpg, 3}, {"SBC", c.sbc, "zpg", c.zpg, 3}, {"INC", c.inc, "zpg", c.zpg, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"INX", c.inx, "imp", c.imp, 2}, {"SBC", c.sbc, "imm", c.imm, 2}, {"NOP", c.nop, "imp", c.imp, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"CPX", c.cpx, "abs", c.abs, 4}, {"SBC", c.sbc, "abs", c.abs, 4}, {"INC", c.inc, "abs", c.abs, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2},
		{"BEQ", c.beq, "rel", c.rel, 2}, {"SBC", c.sbc, "idy", c.idy, 5}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"SBC", c.sbc, "zpx", c.zpx, 4}, {"INC", c.inc, "zpx", c.zpx, 6}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"SED", c.sed, "imp", c.imp, 2}, {"SBC", c.sbc, "aby", c.aby, 4}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"XXX", c.xxi, "xxa", c.xxa, 2}, {"SBC", c.sbc, "abx", c.abx, 4}, {"INC", c.inc, "abx", c.abx, 7}, {"XXX", c.xxi, "xxa", c.xxa, 2}}
}

func (cpu *CPU) run(uint8) {

	if cpu.cycles == 0 {
		cpu.op = cpu.ram.Read16(cpu.reg.pc, true)
		cpu.reg.pc += 1

		cpu.cycles += cpu.opTable[cpu.op].ticks

		addr := cpu.opTable[cpu.op].addressingMode()
		inst := cpu.opTable[cpu.op].instructionMode()

		cpu.cycles += addr & inst

	}
	cpu.cycles -= 1
}

func (cpu *CPU) fetch() {
	if !(cpu.opTable[cpu.op].addrName == "acc" || cpu.opTable[cpu.op].addrName == "imp") {
		cpu.M = cpu.ram.Read16(cpu.addr, true)
	}
}

/*
// ------Addressing Modes------
*/

// Absolute
func (cpu *CPU) abs() uint8 {
	lo := cpu.ram.Read16(cpu.reg.pc, true)
	hi := cpu.ram.Read16(cpu.reg.pc+1, true)
	cpu.reg.pc += 2

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	return 0
}

// Absolute, X-indexed
func (cpu *CPU) abx() uint8 {
	lo := cpu.ram.Read16(cpu.reg.pc, true)
	hi := cpu.ram.Read16(cpu.reg.pc+1, true)
	cpu.reg.pc += 2

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	cpu.addr += uint16(cpu.reg.x)

	if (cpu.addr & 0xFF00) != (uint16(hi) << 8) {
		return 1
	} else {
		return 0
	}
}

// Absolute, Y-indexed
func (cpu *CPU) aby() uint8 {
	lo := cpu.ram.Read16(cpu.reg.pc, true)
	hi := cpu.ram.Read16(cpu.reg.pc+1, true)
	cpu.reg.pc += 2

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	cpu.addr += uint16(cpu.reg.y)

	if (cpu.addr & 0xFF00) != (uint16(hi) << 8) {
		return 1
	} else {
		return 0
	}
}

// Accumulator
func (cpu *CPU) acc() uint8 {
	cpu.M = cpu.reg.ac
	return 0
}

// Immediate
func (cpu *CPU) imm() uint8 {
	cpu.addr = cpu.reg.pc + 1
	return 0
}

// Implied
func (cpu *CPU) imp() uint8 {
	cpu.M = cpu.reg.ac
	return 0
}

// Indirect
func (cpu *CPU) ind() uint8 {
	lo := cpu.ram.Read16(cpu.reg.pc, true)
	hi := cpu.ram.Read16(cpu.reg.pc+1, true)
	cpu.reg.pc += 2

	temp := (uint16(hi)<<8 | uint16(lo))
	if lo == 0xFF {
		cpu.addr = (uint16(cpu.ram.Read16(temp&0xFF00, true))<<8 | uint16(cpu.ram.Read16(temp, true)))
	} else {
		cpu.addr = (uint16(cpu.ram.Read16(temp+1, true))<<8 | uint16(cpu.ram.Read16(temp, true)))
	}
	return 0
}

// X-indexed, Indriect
func (cpu *CPU) xid() uint8 {
	temp := cpu.ram.Read16(cpu.reg.pc, true)
	cpu.reg.pc += 1

	lo := cpu.ram.Read16(uint16(temp+cpu.reg.x), true)
	hi := cpu.ram.Read16(uint16(temp+cpu.reg.x+1), true)

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	return 0
}

// Indriect, Y-indexed
func (cpu *CPU) idy() uint8 {
	temp := cpu.ram.Read16(cpu.reg.pc, true)
	cpu.reg.pc += 1

	lo := cpu.ram.Read16(uint16(temp), true)
	hi := cpu.ram.Read16(uint16(temp+1), true)

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	cpu.addr += uint16(cpu.reg.y)

	if (cpu.addr & 0xFF00) != (uint16(hi) << 8) {
		return 1
	} else {
		return 0
	}
}

// Relative
func (cpu *CPU) rel() uint8 {
	rel := uint16(cpu.ram.Read16(cpu.reg.pc, true))
	cpu.reg.pc += 1

	if rel > 0x007f {
		rel = -(0x0100 - rel)
	}
	cpu.addr += rel

	return 0
}

// Zeropage
func (cpu *CPU) zpg() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc, true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

// Zeropage, X-indexed
func (cpu *CPU) zpx() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc+uint16(cpu.reg.x), true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

// Zeropage, Y-indexed
func (cpu *CPU) zpy() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc+uint16(cpu.reg.y), true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

/*
// ------Instruction Modes------
*/

// Add with Carry
func (cpu *CPU) adc() uint8 {
	cpu.fetch()
	temp := uint16(cpu.reg.ac) + uint16(cpu.M)
	if cpu.getFlag(C) {
		temp += 1
	}
	cpu.setFlag(N, (temp&0x80) == 0x80)
	cpu.setFlag(Z, (temp&0x00FF) == 0x00)
	cpu.setFlag(C, temp > 0x00FF)
	cpu.setFlag(V, ((^(uint16(cpu.reg.ac) ^ uint16(cpu.M)))&(uint16(cpu.reg.ac)^temp)&0x80) == 0x80)
	cpu.reg.ac = uint8(temp & 0x00FF)
	return 1
}

// AND
func (cpu *CPU) and() uint8 {
	cpu.fetch()
	cpu.reg.ac = cpu.reg.ac & cpu.M
	cpu.setFlag(N, (cpu.reg.ac&0x80) == 0x80)
	cpu.setFlag(Z, cpu.reg.ac == 0x00)

	return 0
}

// Arithmetic Shift Left
func (cpu *CPU) asl() uint8 {
	cpu.fetch()

	temp := uint16(cpu.M) << 1

	cpu.setFlag(N, (temp&0x80) == 0x80)
	cpu.setFlag(Z, temp == 0x00)
	cpu.setFlag(C, temp > 0x00FF)

	if cpu.opTable[cpu.op].addrName == "acc" || cpu.opTable[cpu.op].addrName == "imp" {
		cpu.reg.ac = uint8(temp & 0x00FF)
	} else {
		cpu.ram.Write16(cpu.addr, uint8(temp&0x00FF))
	}

	return 0
}

// Branch on Carry Clear
func (cpu *CPU) bcc() uint8 {
	if !cpu.getFlag(C) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// Branch on Carry Set
func (cpu *CPU) bcs() uint8 {
	if cpu.getFlag(C) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}

	return 0
}

// Branch on Equal (Zero Set)
func (cpu *CPU) beq() uint8 {
	if cpu.getFlag(Z) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// Bit Test
func (cpu *CPU) bit() uint8 {
	cpu.fetch()
	temp := cpu.reg.ac & cpu.M

	cpu.setFlag(N, (temp&0x80) == 0x80)
	cpu.setFlag(Z, temp == 0x00)
	cpu.setFlag(V, (temp&0x40) == 0x40)
	return 0
}

// Branch on Minus (Negative Set)
func (cpu *CPU) bmi() uint8 {
	if cpu.getFlag(N) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// Branch on Not Equal
func (cpu *CPU) bne() uint8 {
	if !cpu.getFlag(Z) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// Branch on Plus (Negative Clear)
func (cpu *CPU) bpl() uint8 {
	if !cpu.getFlag(N) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// Break/Interrupt
func (cpu *CPU) brk() uint8 {
	//TO-DO***
	return 0
}

// branch on overflow clear
func (cpu *CPU) bvc() uint8 {
	if !cpu.getFlag(V) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// branch on overflow set
func (cpu *CPU) bvs() uint8 {
	if cpu.getFlag(V) {

		cpu.cycles += 1
		cpu.addr += cpu.reg.pc

		if (cpu.addr & 0xFF00) != (cpu.reg.pc & 0xFF00) {
			cpu.cycles += 1
		}
		cpu.reg.pc = cpu.addr
	}
	return 0
}

// clear carry
func (cpu *CPU) clc() uint8 {
	cpu.setFlag(C, false)
	return 0
}

// clear decimal
func (cpu *CPU) cld() uint8 {
	cpu.setFlag(D, false)
	return 0
}

// clear interrupt disable
func (cpu *CPU) cli() uint8 {
	cpu.setFlag(I, false)
	return 0
}

// clear overflow
func (cpu *CPU) clv() uint8 {
	cpu.setFlag(V, false)
	return 0
}

// compare (with accumulator)
func (cpu *CPU) cmp() uint8 {
	cpu.fetch()
	temp := uint16(cpu.reg.ac - cpu.M)

	cpu.setFlag(N, (temp&0x0080) == 0x80)
	cpu.setFlag(Z, temp == 0x0000)
	cpu.setFlag(C, uint16(cpu.reg.ac) >= temp)

	return 1
}

// compare with X
func (cpu *CPU) cpx() uint8 {
	cpu.fetch()
	temp := uint16(cpu.reg.x - cpu.M)

	cpu.setFlag(N, (temp&0x0080) == 0x80)
	cpu.setFlag(Z, temp == 0x0000)
	cpu.setFlag(C, uint16(cpu.reg.x) >= temp)

	return 0
}

// compare with Y
func (cpu *CPU) cpy() uint8 {
	cpu.fetch()
	temp := uint16(cpu.reg.y - cpu.M)

	cpu.setFlag(N, (temp&0x0080) == 0x80)
	cpu.setFlag(Z, temp == 0x0000)
	cpu.setFlag(C, uint16(cpu.reg.y) >= temp)

	return 0
}

// decrement
func (cpu *CPU) dec() uint8 {
	cpu.fetch()
	cpu.M -= 1

	cpu.setFlag(N, (cpu.M&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.M == 0x0000)
	cpu.ram.Write16(cpu.addr, cpu.M)

	return 0
}

// decrement X
func (cpu *CPU) dex() uint8 {
	cpu.reg.x -= 1

	cpu.setFlag(N, (cpu.reg.x&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.x == 0x0000)
	return 0
}

// decrement Y
func (cpu *CPU) dey() uint8 {
	cpu.reg.y -= 1

	cpu.setFlag(N, (cpu.reg.y&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.y == 0x0000)
	return 0
}

// exclusive or (with accumulator)
func (cpu *CPU) eor() uint8 {
	cpu.fetch()
	cpu.reg.ac = cpu.reg.ac ^ cpu.M

	cpu.setFlag(N, (cpu.reg.ac&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.ac == 0x0000)

	return 1
}

// increment
func (cpu *CPU) inc() uint8 {
	cpu.fetch()
	cpu.M += 1

	cpu.setFlag(N, (cpu.M&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.M == 0x0000)
	cpu.ram.Write16(cpu.addr, cpu.M)
	return 0
}

// increment X
func (cpu *CPU) inx() uint8 {
	cpu.reg.x += 1

	cpu.setFlag(N, (cpu.reg.x&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.x == 0x0000)
	return 0
}

// increment Y
func (cpu *CPU) iny() uint8 {
	cpu.reg.y += 1

	cpu.setFlag(N, (cpu.reg.y&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.y == 0x0000)
	return 0
}

// jump
func (cpu *CPU) jmp() uint8 {
	cpu.reg.pc = cpu.addr
	return 0
}

// jump subroutine
func (cpu *CPU) jsr() uint8 {
	cpu.reg.pc -= 1

	cpu.ram.Write16(0x0100+uint16(cpu.reg.sp), uint8((cpu.reg.pc>>8)&0x00FF))
	cpu.reg.sp -= 1
	cpu.ram.Write16(0x0100+uint16(cpu.reg.sp), uint8(cpu.reg.pc&0x00FF))
	cpu.reg.sp -= 1

	cpu.reg.pc = cpu.addr
	return 0
}

// jump accumulator
func (cpu *CPU) lda() uint8 {
	return 0
}

// load X
func (cpu *CPU) ldx() uint8 {
	return 0
}

// load Y
func (cpu *CPU) ldy() uint8 {
	return 0
}

// logical right shift
func (cpu *CPU) lsr() uint8 {
	return 0
}

// no operation
func (cpu *CPU) nop() uint8 {
	// nothing happens
	return 0
}

// or with accumulator
func (cpu *CPU) ora() uint8 {
	cpu.fetch()
	cpu.reg.ac = cpu.reg.ac | cpu.M

	cpu.setFlag(N, (cpu.reg.ac&0x0080) == 0x80)
	cpu.setFlag(Z, cpu.reg.ac == 0x0000)
	return 1
}

// push accumulator
func (cpu *CPU) pha() uint8 {
	cpu.ram.Write16(0x0100+uint16(cpu.reg.sp), cpu.reg.ac)
	cpu.reg.sp -= 1
	return 0
}

// push accumulator status (SR)
func (cpu *CPU) php() uint8 {
	return 0
}

// pull accumulator
func (cpu *CPU) pla() uint8 {
	cpu.reg.sp += 1
	cpu.reg.ac = cpu.ram.Read16(0x0100+uint16(cpu.reg.sp), true)
	cpu.setFlag(N, (cpu.reg.ac&0x80) == 0x80)
	cpu.setFlag(Z, cpu.reg.ac == 0x00)
	return 0
}

// pull processor status (SR)
func (cpu *CPU) plp() uint8 {
	return 0
}

// rotate left
func (cpu *CPU) rol() uint8 {
	return 0
}

// rotate right
func (cpu *CPU) ror() uint8 {
	return 0
}

// return from interrupt
func (cpu *CPU) rti() uint8 {
	return 0
}

// return from subroutine
func (cpu *CPU) rts() uint8 {
	return 0
}

// subtract with carry
func (cpu *CPU) sbc() uint8 {
	cpu.fetch()
	temp := (uint16(cpu.M)) ^ 0x00FF
	temp += uint16(cpu.reg.ac)
	if cpu.getFlag(C) {
		temp += 1
	}
	cpu.setFlag(N, (temp&0x80) == 0x80)
	cpu.setFlag(Z, (temp&0x00FF) == 0x00)
	cpu.setFlag(C, temp > 0x00FF)
	cpu.setFlag(V, ((^(uint16(cpu.reg.ac) ^ uint16(cpu.M)))&(uint16(cpu.reg.ac)^temp)&0x80) == 0x80)
	cpu.reg.ac = uint8(temp & 0x00FF)
	return 1
}

// set carry
func (cpu *CPU) sec() uint8 {
	cpu.setFlag(C, true)
	return 0
}

// set decimal
func (cpu *CPU) sed() uint8 {
	cpu.setFlag(D, true)
	return 0
}

// set interrupt disable
func (cpu *CPU) sei() uint8 {
	cpu.setFlag(I, true)
	return 0
}

// store accumulator
func (cpu *CPU) sta() uint8 {
	return 0
}

// store X
func (cpu *CPU) stx() uint8 {
	return 0
}

// store Y
func (cpu *CPU) sty() uint8 {
	return 0
}

// transfer accumulator to X
func (cpu *CPU) tax() uint8 {
	return 0
}

// transfer accumulator to Y
func (cpu *CPU) tay() uint8 {
	return 0
}

// transfer stack pointer to X
func (cpu *CPU) tsx() uint8 {
	return 0
}

// transfer X to accumulator
func (cpu *CPU) txa() uint8 {
	return 0
}

// transfer X to stack pointer
func (cpu *CPU) txs() uint8 {
	return 0
}

// transfer Y to accumulator
func (cpu *CPU) tya() uint8 {
	return 0
}

/*
// Illegal Instructions
*/

func (cpu *CPU) xxa() uint8 {
	return 0
}

func (cpu *CPU) xxi() uint8 {
	return 0
}

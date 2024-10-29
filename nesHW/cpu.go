package nes

//Flags
const (
	C = 1 << iota
	Z
	I
	D
	B
	U
	V
	N
)

//CPU Registers
type Registers struct {
	a  uint8  //Accumulator
	x  uint8  //X Register
	y  uint8  //Y Register
	st uint8  //Stack Register
	sp uint8  //Stack Pointer
	pc uint16 //Program Counter
}

func NewRegisters() (reg Registers) {
	reg = Registers{}
	reg.Zero()
	return
}

func (reg *Registers) Zero() {
	reg.a = 0x00
	reg.x = 0x00
	reg.y = 0x00
	reg.st = 0x00
	reg.sp = 0x00
	reg.pc = 0x0000
}

func (cpu *CPU) getFlag(f uint8) bool {
	return (cpu.flags & f) > 0
}

func (cpu *CPU) setFlag(f uint8, b bool) {
	if b {
		cpu.flags |= f
	} else {
		cpu.flags &= ^f
	}
}

type CPU struct {
	reg      Registers //
	flags    uint8     //Flags
	feteched uint8     //
	addr     uint16    //Address (for emulation)
	op       uint8     //Current OP code
	cycles   uint8     //
	ram      Mem       //Memory
	opTable  [256]OPS  //Table holding all opcodes
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
	cpu.flags = 0x04
	cpu.feteched = 0x00
	cpu.addr = 0x0000
	cpu.op = 0x00
	cpu.cycles = 0x00
}

type OPS struct {
	name            string
	addressingMode  func() uint8
	instructionMode func() uint8
	ticks           uint8
}

func (c *CPU) opFunctions() {
	c.opTable = [256]OPS{
		//0							1						2							3						4							5							6						7							8						9							A						B							C						D							E						F
		{"BRK", c.brk, c.imp, 7}, {"ORA", c.ora, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ORA", c.ora, c.zpg, 3}, {"ASL", c.asl, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"PHP", c.php, c.imp, 3}, {"ORA", c.ora, c.imm, 2}, {"ASL", c.asl, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ORA", c.ora, c.abs, 4}, {"ASL", c.asl, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BPL", c.bpl, c.rel, 2}, {"ORA", c.ora, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ORA", c.ora, c.zpx, 4}, {"ASL", c.asl, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"CLC", c.clc, c.imp, 2}, {"ORA", c.ora, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ORA", c.ora, c.abx, 4}, {"ASL", c.asl, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2},
		{"JSR", c.jsr, c.abs, 6}, {"AND", c.and, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"BIT", c.bit, c.zpg, 3}, {"AND", c.and, c.zpg, 3}, {"ROL", c.rol, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"PLP", c.plp, c.imp, 4}, {"AND", c.and, c.imm, 2}, {"ROL", c.rol, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"BIT", c.bit, c.abs, 4}, {"AND", c.and, c.abs, 4}, {"ROL", c.rol, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BMI", c.bmi, c.rel, 2}, {"AND", c.and, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"AND", c.and, c.zpx, 4}, {"ROL", c.rol, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"SEC", c.sec, c.imp, 2}, {"AND", c.and, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"AND", c.and, c.abx, 4}, {"ROL", c.rol, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2},
		{"RTI", c.rti, c.imp, 6}, {"EOR", c.eor, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"EOR", c.eor, c.zpg, 3}, {"LSR", c.lsr, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"PHA", c.pha, c.imp, 3}, {"EOR", c.eor, c.imm, 2}, {"LSR", c.lsr, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"JMP", c.jmp, c.abs, 3}, {"EOR", c.eor, c.abs, 4}, {"LSR", c.lsr, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BVC", c.bvc, c.rel, 2}, {"EOR", c.eor, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"EOR", c.eor, c.zpx, 4}, {"LSR", c.lsr, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"CLI", c.cli, c.imp, 2}, {"EOR", c.eor, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"EOR", c.eor, c.abx, 4}, {"LSR", c.lsr, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2},
		{"RTS", c.rts, c.imp, 6}, {"ADC", c.adc, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ADC", c.adc, c.zpg, 3}, {"ROR", c.ror, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"PLA", c.pla, c.imp, 4}, {"ADC", c.adc, c.imm, 2}, {"ROR", c.ror, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"JMP", c.jmp, c.ind, 5}, {"ADC", c.adc, c.abs, 4}, {"ROR", c.ror, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BVS", c.bvs, c.rel, 2}, {"ADC", c.adc, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ADC", c.adc, c.zpx, 4}, {"ROR", c.ror, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"SEI", c.sei, c.imp, 2}, {"ADC", c.adc, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"ADC", c.adc, c.abx, 4}, {"ROR", c.ror, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2},
		{"XXX", c.xxi, c.xxa, 2}, {"STA", c.sta, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"STY", c.sty, c.zpg, 3}, {"STA", c.sta, c.zpg, 3}, {"STX", c.stx, c.zpg, 3}, {"XXX", c.xxi, c.xxa, 2}, {"DEY", c.dey, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"TXA", c.txa, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"STY", c.sty, c.abs, 4}, {"STA", c.sta, c.abs, 4}, {"STX", c.stx, c.abs, 4}, {"XXX", c.xxi, c.xxa, 2},
		{"BCC", c.bcc, c.rel, 2}, {"STA", c.sta, c.izy, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"STY", c.sty, c.zpx, 4}, {"STA", c.sta, c.zpx, 4}, {"STX", c.stx, c.zpy, 4}, {"XXX", c.xxi, c.xxa, 2}, {"TYA", c.tya, c.imp, 2}, {"STA", c.sta, c.aby, 5}, {"TXS", c.txs, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"STA", c.sta, c.abx, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2},
		{"LDY", c.ldy, c.imm, 2}, {"LDA", c.lda, c.izx, 6}, {"LDX", c.ldx, c.imm, 2}, {"XXX", c.xxi, c.xxa, 2}, {"LDY", c.ldy, c.zpg, 3}, {"LDA", c.lda, c.zpg, 3}, {"LDX", c.ldx, c.zpg, 3}, {"XXX", c.xxi, c.xxa, 2}, {"TAY", c.tay, c.imp, 2}, {"LDA", c.lda, c.imm, 2}, {"TAX", c.tax, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"LDY", c.ldy, c.abs, 4}, {"LDA", c.lda, c.abs, 4}, {"LDX", c.ldx, c.abs, 4}, {"XXX", c.xxi, c.xxa, 2},
		{"BCS", c.bcs, c.rel, 2}, {"LDA", c.lda, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"LDY", c.ldy, c.zpx, 4}, {"LDA", c.lda, c.zpx, 4}, {"LDX", c.ldx, c.zpy, 4}, {"XXX", c.xxi, c.xxa, 2}, {"CLV", c.clv, c.imp, 2}, {"LDA", c.lda, c.aby, 4}, {"TSX", c.txs, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"LDY", c.ldy, c.abx, 4}, {"LDA", c.lda, c.abx, 4}, {"LDX", c.ldx, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2},
		{"CPY", c.cpy, c.imm, 2}, {"CMP", c.cmp, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CPY", c.cpy, c.zpg, 3}, {"CMP", c.cmp, c.zpg, 3}, {"DEC", c.dec, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"INY", c.iny, c.imp, 2}, {"CMP", c.cmp, c.imm, 2}, {"DEX", c.dex, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CPY", c.cpy, c.abs, 4}, {"CMP", c.cmp, c.abs, 4}, {"DEC", c.dec, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BNE", c.bne, c.rel, 2}, {"CMP", c.cmp, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CPM", c.cmp, c.zpx, 4}, {"DEC", c.dec, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"CLD", c.cld, c.imp, 2}, {"CMP", c.cmp, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CMP", c.cmp, c.abx, 4}, {"DEC", c.dec, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2},
		{"CPX", c.cpx, c.imm, 2}, {"SBC", c.sbc, c.izx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CPX", c.cpx, c.zpg, 3}, {"SBC", c.sbc, c.zpg, 3}, {"INC", c.inc, c.zpg, 5}, {"XXX", c.xxi, c.xxa, 2}, {"INX", c.inx, c.imp, 2}, {"SBC", c.sbc, c.imm, 2}, {"NOP", c.nop, c.imp, 2}, {"XXX", c.xxi, c.xxa, 2}, {"CPX", c.cpx, c.abs, 4}, {"SBC", c.sbc, c.abs, 4}, {"INC", c.inc, c.abs, 6}, {"XXX", c.xxi, c.xxa, 2},
		{"BEQ", c.beq, c.rel, 2}, {"SBC", c.sbc, c.izy, 5}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"SBC", c.sbc, c.zpx, 4}, {"INC", c.inc, c.zpx, 6}, {"XXX", c.xxi, c.xxa, 2}, {"SED", c.sed, c.imp, 2}, {"SBC", c.sbc, c.aby, 4}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"XXX", c.xxi, c.xxa, 2}, {"SBC", c.sbc, c.abx, 4}, {"INC", c.inc, c.abx, 7}, {"XXX", c.xxi, c.xxa, 2}}
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

// Addressing Modes
func (cpu *CPU) abs() uint8 {
	lo := cpu.ram.Read16(cpu.reg.pc, true)
	hi := cpu.ram.Read16(cpu.reg.pc+1, true)
	cpu.reg.pc += 2

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	return 0
}

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

func (cpu *CPU) imm() uint8 {
	cpu.addr = cpu.reg.pc + 1
	return 0
}

func (cpu *CPU) imp() uint8 {
	cpu.feteched = cpu.reg.a
	return 0
}

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

func (cpu *CPU) inx() uint8 {
	temp := cpu.ram.Read16(cpu.reg.pc, true)
	cpu.reg.pc += 1

	lo := cpu.ram.Read16(uint16(temp+cpu.reg.x), true)
	hi := cpu.ram.Read16(uint16(temp+cpu.reg.x+1), true)

	cpu.addr = (uint16(hi)<<8 | uint16(lo))
	return 0
}

func (cpu *CPU) iny() uint8 {
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

func (cpu *CPU) rel() uint8 {
	rel := uint16(cpu.ram.Read16(cpu.reg.pc, true))
	cpu.reg.pc += 1

	if rel > 0x007f {
		rel = -(0x0100 - rel)
	}
	cpu.addr += rel

	return 0
}

func (cpu *CPU) zpg() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc, true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

func (cpu *CPU) zpx() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc+uint16(cpu.reg.x), true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

func (cpu *CPU) zpy() uint8 {
	cpu.addr = uint16(cpu.ram.Read16(cpu.reg.pc+uint16(cpu.reg.y), true))
	cpu.reg.pc += 1
	cpu.addr &= 0x00FF
	return 0
}

//Instruction Modes

func (cpu *CPU) adc() uint8 {
	return 0
}

func (cpu *CPU) and() uint8 {
	return 0
}

func (cpu *CPU) asl() uint8 {
	return 0
}

func (cpu *CPU) bcc() uint8 {
	return 0
}

func (cpu *CPU) bcs() uint8 {
	return 0
}

func (cpu *CPU) beq() uint8 {
	return 0
}

func (cpu *CPU) bit() uint8 {
	return 0
}

func (cpu *CPU) bmi() uint8 {
	return 0
}

func (cpu *CPU) bne() uint8 {
	return 0
}

func (cpu *CPU) bpl() uint8 {
	return 0
}

func (cpu *CPU) brk() uint8 {
	return 0
}

func (cpu *CPU) bvc() uint8 {
	return 0
}

func (cpu *CPU) bvs() uint8 {
	return 0
}

func (cpu *CPU) clc() uint8 {
	return 0
}

func (cpu *CPU) cld() uint8 {
	return 0
}

func (cpu *CPU) cli() uint8 {
	return 0
}

func (cpu *CPU) clv() uint8 {
	return 0
}

func (cpu *CPU) cmp() uint8 {
	return 0
}

func (cpu *CPU) cpx() uint8 {
	return 0
}

func (cpu *CPU) cpy() uint8 {
	return 0
}

func (cpu *CPU) dec() uint8 {
	return 0
}

func (cpu *CPU) dex() uint8 {
	return 0
}

func (cpu *CPU) dey() uint8 {
	return 0
}

func (cpu *CPU) eor() uint8 {
	return 0
}

func (cpu *CPU) inc() uint8 {
	return 0
}

func (cpu *CPU) izx() uint8 {
	return 0
}

func (cpu *CPU) izy() uint8 {
	return 0
}

func (cpu *CPU) jmp() uint8 {
	return 0
}

func (cpu *CPU) jsr() uint8 {
	return 0
}

func (cpu *CPU) lda() uint8 {
	return 0
}

func (cpu *CPU) ldx() uint8 {
	return 0
}

func (cpu *CPU) ldy() uint8 {
	return 0
}

func (cpu *CPU) lsr() uint8 {
	return 0
}

func (cpu *CPU) nop() uint8 {
	return 0
}

func (cpu *CPU) ora() uint8 {
	return 0
}

func (cpu *CPU) pha() uint8 {
	return 0
}

func (cpu *CPU) php() uint8 {
	return 0
}

func (cpu *CPU) pla() uint8 {
	return 0
}

func (cpu *CPU) plp() uint8 {
	return 0
}

func (cpu *CPU) rol() uint8 {
	return 0
}

func (cpu *CPU) ror() uint8 {
	return 0
}

func (cpu *CPU) rti() uint8 {
	return 0
}

func (cpu *CPU) rts() uint8 {
	return 0
}

func (cpu *CPU) sbc() uint8 {
	return 0
}

func (cpu *CPU) sec() uint8 {
	return 0
}

func (cpu *CPU) sed() uint8 {
	return 0
}

func (cpu *CPU) sei() uint8 {
	return 0
}

func (cpu *CPU) sta() uint8 {
	return 0
}

func (cpu *CPU) stx() uint8 {
	return 0
}

func (cpu *CPU) sty() uint8 {
	return 0
}

func (cpu *CPU) tax() uint8 {
	return 0
}

func (cpu *CPU) tay() uint8 {
	return 0
}

func (cpu *CPU) tsx() uint8 {
	return 0
}

func (cpu *CPU) txa() uint8 {
	return 0
}

func (cpu *CPU) txs() uint8 {
	return 0
}

func (cpu *CPU) tya() uint8 {
	return 0
}

// Dummy Codes

func (cpu *CPU) xxa() uint8 {
	return 0
}

func (cpu *CPU) xxi() uint8 {
	return 0
}

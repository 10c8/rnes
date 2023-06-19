use std::{fs::File, io::Write};

use log::trace;

use crate::mapper::Mapper;

pub struct CPU {
    registers: Registers,
    memory: Memory,
    mapper: Option<Box<dyn Mapper>>,
    cycles: u64,

    trace_log: File,
}

impl CPU {
    pub fn new() -> Self {
        let trace_log = File::create("trace.log").unwrap();

        Self {
            registers: Registers::new(),
            memory: Memory::new(),
            mapper: None,
            cycles: 0,

            trace_log,
        }
    }

    pub fn run(&mut self) {
        loop {
            self.cycle();
        }
    }

    pub fn reset(&mut self) {
        // let pc_lo = self.memory_read(0xFFFC);
        // let pc_hi = self.memory_read(0xFFFD);
        // self.registers.pc = ((pc_hi as u16) << 8) | pc_lo as u16;
        // trace!("Entry point: {:#06X}", self.registers.pc);

        self.registers.pc = 0xC000; // Nestest.nes automation mode

        self.registers.sp = 0xFD;
        self.registers.p = 0x24;
        self.registers.a = 0;
        self.registers.x = 0;
        self.registers.y = 0;

        self.cycles = 7;
    }

    pub fn set_mapper(&mut self, mapper: Box<dyn Mapper>) {
        self.mapper = Some(mapper);
    }

    fn cycle(&mut self) {
        let opcode = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        match (opcode & 0xF0) >> 4 {
            0x0 => match opcode {
                0x00 => self.op_brk(),
                0x01 => self.op_ora_x_ind(),
                0x05 => self.op_ora_zpg(),
                0x06 => self.op_asl_zpg(),
                0x08 => self.op_php(),
                0x09 => self.op_ora_imm(),
                0x0A => self.op_asl_acc(),
                0x0D => self.op_ora_abs(),
                0x0E => self.op_asl_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x1 => match opcode {
                0x10 => self.op_bpl(),
                0x11 => self.op_ora_ind_y(),
                0x15 => self.op_ora_zpg_x(),
                0x16 => self.op_asl_zpg_x(),
                0x18 => self.op_clc(),
                0x19 => self.op_ora_abs_y(),
                0x1D => self.op_ora_abs_x(),
                0x1E => self.op_asl_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x2 => match opcode {
                0x20 => self.op_jsr(),
                0x21 => self.op_and_x_ind(),
                0x24 => self.op_bit_zpg(),
                0x25 => self.op_and_zpg(),
                0x26 => self.op_rol_zpg(),
                0x28 => self.op_plp(),
                0x29 => self.op_and_imm(),
                0x2A => self.op_rol_acc(),
                0x2C => self.op_bit_abs(),
                0x2D => self.op_and_abs(),
                0x2E => self.op_rol_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x3 => match opcode {
                0x30 => self.op_bmi(),
                // 0x31 => self.op_and_ind_y(),
                // 0x35 => self.op_and_zpg_x(),
                // 0x36 => self.op_rol_zpg_x(),
                0x38 => self.op_sec(),
                // 0x39 => self.op_and_abs_y(),
                // 0x3D => self.op_and_abs_x(),
                // 0x3E => self.op_rol_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x4 => match opcode {
                0x40 => self.op_rti(),
                // 0x41 => self.op_eor_x_ind(),
                // 0x45 => self.op_eor_zpg(),
                // 0x46 => self.op_lsr_zpg(),
                0x48 => self.op_pha(),
                0x49 => self.op_eor_imm(),
                // 0x4A => self.op_lsr_acc(),
                0x4C => self.op_jmp_abs(),
                // 0x4D => self.op_eor_abs(),
                // 0x4E => self.op_lsr_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x5 => match opcode {
                0x50 => self.op_bvc(),
                // 0x51 => self.op_eor_ind_y(),
                // 0x55 => self.op_eor_zpg_x(),
                // 0x56 => self.op_lsr_zpg_x(),
                // 0x58 => self.op_cli(),
                // 0x59 => self.op_eor_imm(),
                // 0x5D => self.op_eor_abs_x(),
                // 0x5E => self.op_lsr_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x6 => match opcode {
                0x60 => self.op_rts(),
                // 0x61 => self.op_adc_x_ind(),
                // 0x65 => self.op_adc_zpg(),
                // 0x66 => self.op_ror_zpg(),
                0x68 => self.op_pla(),
                0x69 => self.op_adc_imm(),
                // 0x6A => self.op_ror_acc(),
                // 0x6C => self.op_jmp_abs_ind(),
                // 0x6D => self.op_adc_abs(),
                // 0x6E => self.op_ror_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x7 => match opcode {
                0x70 => self.op_bvs(),
                // 0x71 => self.op_adc_ind_y(),
                // 0x75 => self.op_adc_zpg_x(),
                // 0x76 => self.op_ror_zpg_x(),
                0x78 => self.op_sei(),
                // 0x79 => self.op_adc_imm(),
                // 0x7D => self.op_adc_abs_x(),
                // 0x7E => self.op_ror_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x8 => match opcode {
                // 0x81 => self.op_sta_x_ind(),
                // 0x84 => self.op_sty_zpg(),
                0x85 => self.op_sta_zpg(),
                0x86 => self.op_stx_zpg(),
                0x88 => self.op_dey(),
                // 0x89 => self.op_txa(),
                // 0x8A => self.op_sty_abs(),
                // 0x8C => self.op_stx_abs(),
                // 0x8D => self.op_sta_abs(),
                // 0x8E => self.op_sta_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x9 => match opcode {
                0x90 => self.op_bcc(),
                // 0x91 => self.op_sta_ind_y(),
                // 0x94 => self.op_sty_zpg_x(),
                // 0x95 => self.op_sta_zpg_x(),
                // 0x96 => self.op_stx_zpg_y(),
                0x98 => self.op_tya(),
                // 0x99 => self.op_sta_abs_y(),
                0x9A => self.op_txs(),
                // 0x9D => self.op_sta_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xA => match opcode {
                0xA0 => self.op_ldy_imm(),
                // 0xA1 => self.op_lda_x_ind(),
                0xA2 => self.op_ldx_imm(),
                // 0xA4 => self.op_ldy_zpg(),
                // 0xA5 => self.op_lda_zpg(),
                // 0xA6 => self.op_ldx_zpg(),
                0xA8 => self.op_tay(),
                0xA9 => self.op_lda_imm(),
                0xAA => self.op_tax(),
                // 0xAC => self.op_ldy_abs(),
                0xAD => self.op_lda_abs(),
                // 0xAE => self.op_ldx_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xB => match opcode {
                0xB0 => self.op_bcs(),
                // 0xB1 => self.op_lda_ind_y(),
                // 0xB4 => self.op_ldy_zpg_x(),
                // 0xB5 => self.op_lda_zpg_x(),
                // 0xB6 => self.op_ldx_zpg_y(),
                0xB8 => self.op_clv(),
                // 0xB9 => self.op_lda_abs_y(),
                // 0xBA => self.op_tsx(),
                // 0xBC => self.op_ldy_abs_x(),
                // 0xBD => self.op_lda_abs_x(),
                // 0xBE => self.op_ldx_abs_y(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xC => match opcode {
                0xC0 => self.op_cpy_imm(),
                // 0xC1 => self.op_cmp_x_ind(),
                // 0xC4 => self.op_cpy_zpg(),
                // 0xC5 => self.op_cmp_zpg(),
                // 0xC6 => self.op_dec_zpg(),
                0xC8 => self.op_iny(),
                0xC9 => self.op_cmp_imm(),
                0xCA => self.op_dex(),
                // 0xCC => self.op_cpy_abs(),
                // 0xCD => self.op_cmp_abs(),
                // 0xCE => self.op_dec_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xD => match opcode {
                0xD0 => self.op_bne(),
                // 0xD1 => self.op_cmp_ind_y(),
                // 0xD5 => self.op_cmp_zpg_x(),
                // 0xD6 => self.op_dec_zpg_x(),
                0xD8 => self.op_cld(),
                // 0xD9 => self.op_cmp_abs_y(),
                // 0xDD => self.op_cmp_abs_x(),
                // 0xDE => self.op_dec_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xE => match opcode {
                0xE0 => self.op_cpx_imm(),
                // 0xE1 => self.op_sbc_x_ind(),
                // 0xE4 => self.op_cpx_zpg(),
                // 0xE5 => self.op_sbc_zpg(),
                // 0xE6 => self.op_inc_zpg(),
                0xE8 => self.op_inx(),
                0xE9 => self.op_sbc_imm(),
                0xEA => self.op_nop(),
                // 0xEC => self.op_cpx_abs(),
                // 0xED => self.op_sbc_abs(),
                // 0xEE => self.op_inc_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xF => match opcode {
                0xF0 => self.op_beq(),
                // 0xF1 => self.op_sbc_ind_y(),
                // 0xF5 => self.op_sbc_zpg_x(),
                // 0xF6 => self.op_inc_zpg_x(),
                0xF8 => self.op_sed(),
                // 0xF9 => self.op_sbc_abs_y(),
                // 0xFD => self.op_sbc_abs_x(),
                // 0xFE => self.op_inc_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            _ => panic!("Invalid opcode: {:#04X}", opcode),
        }
    }

    // Opcodes 00-0F
    fn op_brk(&mut self) {
        // BRK - Force Break
        // interrupt,                        N Z C I D V
        // push PC+2, push P                 - - - 1 - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       BRK          00        1      7

        self.trace_opcode(1, "00", "BRK");

        self.stack_push_u16(self.registers.pc + 1);

        let p = self.registers.p | 0b0001_0000;
        self.stack_push(p);

        self.registers.set_status_flag(StatusFlag::Break, true);
        self.registers.set_status_flag(StatusFlag::Interrupt, true);

        self.cycles += 7;
    }

    fn op_ora_x_ind(&mut self) {
        // ORA X, ind - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect,X)  ORA (oper,X)  01       2      6

        let operator = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.memory_read(operator.wrapping_add(self.registers.x).into());

        let value = self.memory_read(address as u16);
        self.registers.a |= value;

        self.trace_opcode(
            2,
            format!("01 {:02X}", operator),
            format!(
                "ORA (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator,
                operator.wrapping_add(self.registers.x),
                address,
                value,
            ),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    fn op_ora_zpg(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      ORA oper      05       2      3

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("05 {:02X}", address),
            format!("ORA ${:02X}", address),
        );

        let value = self.memory_read(address as u16);
        self.registers.a |= value;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 3;
    }

    fn op_asl_zpg(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // A = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      ASL oper      06       2      5

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("06 {:02X}", address),
            format!("ASL ${:02X}", address),
        );

        let mut value = self.memory_read(address as u16);
        let c = value & 0x80 != 0;
        value <<= 1;

        self.memory_write(address as u16, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
    }

    fn op_php(&mut self) {
        // PHP - Push Processor Status On Stack
        // push SR                           N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PHP          08        1      3

        self.trace_opcode(1, "08", "PHP");

        let p = self.registers.p | 0b0001_0000;
        self.stack_push(p);

        self.cycles += 3;
    }

    fn op_ora_imm(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // immediate     ORA #oper     09       2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("09 {:02X}", value),
            format!("ORA #${:02X}", value),
        );

        self.registers.a |= value;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_asl_acc(&mut self) {
        // ASL - Arithmetic Shift Left (ACC)
        // A = A << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // accumulator   ASL A         0A       1      2

        self.trace_opcode(1, "0A", "ASL A");

        let c = self.registers.a & 0x80 != 0;

        self.registers.a <<= 1;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_ora_abs(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      ORA oper      0D       3      4

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let value = self.memory_read(address);
        self.registers.a |= value;

        self.trace_opcode(
            3,
            format!("0D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ORA ${:04X} = {:02X}", address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 4;
    }

    fn op_asl_abs(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // A = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      ASL oper      0E       3      6

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let mut value = self.memory_read(address);
        let c = value & 0x80 != 0;
        value <<= 1;

        self.trace_opcode(
            3,
            format!("0E {:02X} {:02X}", address & 0xFF, address >> 8,),
            format!("ASL ${:04X} = {:02X}", address, value),
        );

        self.registers.a = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    // Opcodes 10-1F
    fn op_bpl(&mut self) {
        // BPL - Branch If Result Plus
        // branch if N eq 0                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BPL oper     10        2    2**

        let offset = self.memory_read(self.registers.pc) as i8;
        self.registers.pc += 1;

        let address = if offset.is_negative() {
            self.registers.pc.wrapping_sub(offset.wrapping_abs() as u16)
        } else {
            self.registers.pc.wrapping_add(offset as u16)
        };

        self.trace_opcode(
            2,
            format!("10 {:02X}", offset),
            format!("BPL ${:04X}", address),
        );

        if !self.registers.get_status_flag(StatusFlag::Negative) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    fn op_ora_ind_y(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect),Y  ORA ($oper),Y 11       2     5*

        let operator = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let base = self.memory_read_u16(operator as u16);
        let address = base.wrapping_add(self.registers.y as u16);

        let value = self.memory_read(address);
        self.registers.a |= value;

        self.trace_opcode(
            2,
            format!("11 {:02X}", operator),
            format!(
                "ORA (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_ora_zpg_x(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    ORA oper,X    15       2     4

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = address.wrapping_add(self.registers.x);

        let value = self.memory_read(address as u16);
        self.registers.a |= value;

        self.trace_opcode(
            2,
            format!("15 {:02X}", address),
            format!("ORA ${:02X},X @ {:02X} = {:02X}", address, address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 4;
    }

    fn op_asl_zpg_x(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // A = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    ASL oper,X    16       2      6

        let operator = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = operator.wrapping_add(self.registers.x);

        let mut value = self.memory_read(address as u16);
        let c = value & 0x80 != 0;
        value <<= 1;

        self.trace_opcode(
            2,
            format!("16 {:02X}", operator),
            format!("ASL ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.registers.a = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6
    }

    fn op_clc(&mut self) {
        // CLC - Clear Carry Flag
        // C = 0                             N Z C I D V
        //                                   - - 0 - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       CLC          18        1      2

        self.trace_opcode(1, "18", "CLC");

        self.registers.set_status_flag(StatusFlag::Carry, false);

        self.cycles += 2;
    }

    fn op_ora_abs_y(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,Y    ORA oper,Y    19       3     4*

        let operator = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let address = operator.wrapping_add(self.registers.y as u16);

        let value = self.memory_read(address);
        self.registers.a |= value;

        self.trace_opcode(
            3,
            format!("19 {:04X}", operator),
            format!("ORA ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        if address & 0xFF00 != address & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_ora_abs_x(&mut self) {
        // ORA - OR Memory With Accumulator
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,X    ORA $oper,X   1D       3     4*

        let operator = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let address = operator.wrapping_add(self.registers.x as u16);

        let value = self.memory_read(address);
        self.registers.a |= value;

        self.trace_opcode(
            3,
            format!("1D {:04X}", operator),
            format!("ORA ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        if address & 0xFF00 != address & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_asl_abs_x(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // A = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,X    ASL $oper,X   1E       3      7

        let operator = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let address = operator.wrapping_add(self.registers.x as u16);

        let mut value = self.memory_read(address);
        let c = value & 0x80 != 0;
        value <<= 1;

        self.trace_opcode(
            3,
            format!("1E {:04X}", operator),
            format!("ASL ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
    }

    // Opcodes 20-2F
    fn op_jsr(&mut self) {
        // JSR - Jump Saving Return Address
        // push (PC+2),                      N Z C I D V
        // PC = address                      - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      JSR $oper    20        3      6

        let address = self.memory_read_u16(self.registers.pc);

        self.trace_opcode(
            1,
            &format!("20 {:02X} {:02X}", address & 0xFF, address >> 8),
            &format!("JSR ${:04X}", address),
        );

        let pc = self.registers.pc;
        self.stack_push_u16(pc.wrapping_add(1));

        self.registers.pc = address;

        self.cycles += 6;
    }

    fn op_and_x_ind(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // X,indirect    AND (oper,X)  21       2      6

        let operator = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.memory_read_u16(operator.wrapping_add(self.registers.x) as u16);

        let value = self.memory_read(address);
        self.registers.a &= value;

        self.trace_opcode(
            2,
            format!("21 {:02X}", operator),
            format!(
                "AND (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator,
                operator.wrapping_add(self.registers.x),
                address,
                value,
            ),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    fn op_bit_zpg(&mut self) {
        // BIT - Test Bits In Memory With ACC
        // A AND M, N = M7, V = M6           N Z C I D V
        //                                   $ + - - - $
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      BIT oper     24        2      3

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let value = self.memory_read(address as u16);

        self.trace_opcode(
            2,
            format!("24 {:02X}", address),
            format!("BIT ${:02X} = {:02X}", address, value),
        );

        let n = (value & 0b1000_0000) != 0;
        let v = (value & 0b0100_0000) != 0;
        let z = (value & self.registers.a) == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Overflow, v);

        self.cycles += 3;
    }

    fn op_and_zpg(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      AND oper      25       2      3

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let value = self.memory_read(address as u16);
        self.registers.a &= value;

        self.trace_opcode(
            2,
            format!("25 {:02X}", address),
            format!("AND ${:02X} = {:02X}", address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 3;
    }

    fn op_rol_zpg(&mut self) {
        // ROL - Rotate Left (Memory)
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + $ - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      ROL oper      26       2      5

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let mut value = self.memory_read(address as u16);
        let c = value & 0x80 != 0;
        value <<= 1;

        if self.registers.get_status_flag(StatusFlag::Carry) {
            value |= 0x01;
        }

        self.trace_opcode(
            2,
            format!("26 {:02X}", address),
            format!("ROL ${:02X} = {:02X}", address, value),
        );

        self.memory_write(address as u16, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
    }

    fn op_plp(&mut self) {
        // PLP - Pull Status From Stack
        // pop P                             N Z C I D V
        //                                   $ $ $ $ $ $
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PLP          28        1      4

        self.trace_opcode(1, "28", "PLP");

        let p = self.stack_pop();
        let p = p & 0b1100_1111;
        let p = p | (self.registers.p & 0b0011_0000);
        self.registers.p = p;

        self.cycles += 4;
    }

    fn op_and_imm(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // immediate     AND #oper     29       2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("29 {:02X}", value),
            format!("AND #${:02X}", value),
        );

        self.registers.a &= value;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_rol_acc(&mut self) {
        // ROL - Rotate Left (ACC)
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + $ - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // accumulator   ROL A         2A       1      2

        self.trace_opcode(1, "2A", "ROL A");

        let mut value = self.registers.a;
        let c = value & 0x80 != 0;
        value <<= 1;

        if self.registers.get_status_flag(StatusFlag::Carry) {
            value |= 0x01;
        }

        self.registers.a = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_bit_abs(&mut self) {
        // BIT - Test Bits In Memory With ACC
        // A AND M, N = M7, V = M6           N Z C I D V
        //                                   $ + - - - $
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      BIT $oper     2C       3      4

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let value = self.memory_read(address);

        self.trace_opcode(
            3,
            format!("2C {:02X}", address),
            format!("BIT ${:04X} = {:02X}", address, value),
        );

        let n = (value & 0b1000_0000) != 0;
        let v = (value & 0b0100_0000) != 0;
        let z = (value & self.registers.a) == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Overflow, v);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 4;
    }

    fn op_and_abs(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      AND $oper     2D       3      4

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let value = self.memory_read(address);
        self.registers.a &= value;

        self.trace_opcode(
            3,
            format!("2D {:02X}", address),
            format!("AND ${:04X} = {:02X}", address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 4;
    }

    fn op_rol_abs(&mut self) {
        // ROL - Rotate Left (Memory)
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + $ - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      ROL $oper     2E       3      6

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let mut value = self.memory_read(address);
        let c = value & 0x80 != 0;
        value <<= 1;

        if self.registers.get_status_flag(StatusFlag::Carry) {
            value |= 0x01;
        }

        self.trace_opcode(
            3,
            format!("2E {:02X}", address),
            format!("ROL ${:04X} = {:02X}", address, value),
        );

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    // Opcodes 30-3F
    fn op_bmi(&mut self) {
        // BMI - Branch If Minus
        // branch if N eq 1                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BMI oper     30        2    2**

        let offset = self.memory_read(self.registers.pc) as i8;
        self.registers.pc += 1;

        let address = if offset.is_negative() {
            self.registers.pc.wrapping_sub(offset.abs() as u16)
        } else {
            self.registers.pc.wrapping_add(offset as u16)
        };

        self.trace_opcode(
            2,
            format!("30 {:02X}", offset),
            format!("BMI ${:04X}", address),
        );

        if self.registers.get_status_flag(StatusFlag::Negative) {
            self.registers.pc = address;
            self.cycles += 3;
        } else {
            self.cycles += 2;
        }

        if self.registers.pc & 0xFF00 != address & 0xFF00 {
            self.cycles += 1;
        }
    }

    fn op_sec(&mut self) {
        // SEC - Set Carry Flag
        // C = 1                             N Z C I D V
        //                                   - - 1 - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       SEC          38        1      2

        self.trace_opcode(1, "38", "SEC");

        self.registers.set_status_flag(StatusFlag::Carry, true);

        self.cycles += 2;
    }

    // Opcodes 40-4F
    fn op_rti(&mut self) {
        // RTI - Return From Interrupt
        // pop SR, pop PC                    N Z C I D V
        //                                   $ $ $ $ $ $
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       RTI          40        1      6

        self.trace_opcode(1, "40", "RTI");

        let p = self.stack_pop();
        let p = p & 0b1100_1111;
        let p = p | (self.registers.p & 0b0011_0000);
        self.registers.p = p;

        let pc = self.stack_pop_u16();
        self.registers.pc = pc;

        self.cycles += 6;
    }

    fn op_pha(&mut self) {
        // PHA - Push Accumulator
        // push A                            N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PHA          48        1      3

        self.trace_opcode(1, "48", "PHA");

        self.stack_push(self.registers.a);

        self.cycles += 3;
    }

    fn op_eor_imm(&mut self) {
        // EOR - Exclusive OR
        // a = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     EOR #oper    49        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("49 {:02X}", value),
            format!("EOR #${:02X}", value),
        );

        self.registers.a ^= value;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_jmp_abs(&mut self) {
        // JMP - Jump To New Location
        // PC = address                      N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      JMP oper     4C        3      3

        let address = self.memory_read_u16(self.registers.pc);

        self.trace_opcode(
            1,
            format!("4C {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("JMP ${:04X}", address),
        );

        self.registers.pc = address;

        self.cycles += 3;
    }

    // Opcodes 50-5F
    fn op_bvc(&mut self) {
        // BVC - Branch If Overflow Clear
        // branch if V eq 0                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BVC oper     50        2    2**

        let offset = self.memory_read(self.registers.pc) as i8;
        self.registers.pc += 1;

        let address = if offset.is_negative() {
            self.registers.pc.wrapping_sub(offset.wrapping_abs() as u16)
        } else {
            self.registers.pc.wrapping_add(offset as u16)
        };

        self.trace_opcode(
            2,
            format!("50 {:02X}", offset),
            format!("BVC ${:04X}", address),
        );

        if !self.registers.get_status_flag(StatusFlag::Overflow) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    // Opcodes 60-6F
    fn op_rts(&mut self) {
        // RTS - Return from Subroutine
        // pop PC, PC = PC+1                 N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       RTS          60        1      6

        self.trace_opcode(1, "60", "RTS");

        let pc = self.stack_pop_u16();
        self.registers.pc = pc.wrapping_add(1);

        self.cycles += 6;
    }

    fn op_pla(&mut self) {
        // PLA - Pull Accumulator From Stack
        // pop A                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PLA          68        1      4

        self.trace_opcode(1, "68", "PLA");

        let value = self.stack_pop();
        self.registers.a = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 4;
    }

    fn op_adc_imm(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                      N Z C I D V
        //                                    + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     ADC #oper    69        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("69 {:02X}", value),
            format!("ADC #${:02X}", value),
        );

        let a = self.registers.a;
        let c = self.registers.get_status_flag(StatusFlag::Carry);

        let carry = if c { 1 } else { 0 };
        let result = a.wrapping_add(value).wrapping_add(carry);

        self.registers.a = result;

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = result < a || result < value || result < carry;
        let v = (a ^ result) & (value ^ result) & 0x80 != 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Overflow, v);

        self.cycles += 2;
    }

    // Opcodes 70-7F
    fn op_bvs(&mut self) {
        // BVS - Branch If Overflow Set
        // branch if V eq 1                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BVS oper     70        2    2**

        let offset = self.memory_read(self.registers.pc) as i8;
        self.registers.pc += 1;

        let address = if offset.is_negative() {
            self.registers.pc.wrapping_sub(offset.wrapping_abs() as u16)
        } else {
            self.registers.pc.wrapping_add(offset as u16)
        };

        self.trace_opcode(
            2,
            format!("70 {:02X}", offset),
            format!("BVS ${:04X}", address),
        );

        if self.registers.get_status_flag(StatusFlag::Overflow) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    fn op_sei(&mut self) {
        // SEI - Set Interrupt Disable Status
        // I = 1                             N Z C I D V
        //                                   - - - 1 - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       SEI          78        1      2

        self.trace_opcode(1, "78", "SEI");

        self.registers.set_status_flag(StatusFlag::Interrupt, true);

        self.cycles += 2;
    }

    // Opcodes 80-8F
    fn op_sta_zpg(&mut self) {
        // STA - Store Accumulator In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      STA oper     85        2      3

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let initial = self.memory_read(address as u16);

        self.trace_opcode(
            2,
            format!("85 {:02X}", address),
            format!("STA ${:02X} = {:02X}", address, initial),
        );

        self.memory_write(address as u16, self.registers.a);

        self.cycles += 3;
    }

    fn op_stx_zpg(&mut self) {
        // STX - Store Index X In Memory
        // M = X                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      STX oper     86        2      3

        let address = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let initial = self.memory_read(address as u16);

        self.trace_opcode(
            2,
            format!("86 {:02X}", address),
            format!("STX ${:02X} = {:02X}", address, initial),
        );

        self.memory_write(address as u16, self.registers.x);

        self.cycles += 3;
    }

    fn op_dey(&mut self) {
        // DEY - Decrement Index Y By One
        // Y = Y - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       DEY          88        1      2

        self.trace_opcode(1, "88", "DEY");

        self.registers.y = self.registers.y.wrapping_sub(1);

        let n = self.registers.y & 0x80 != 0;
        let z = self.registers.y == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    // Opcodes 90-9F
    fn op_bcc(&mut self) {
        // BCC - Branch If Carry Clear
        // branch if C eq 0                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BCC oper     90        2    2**

        let offset = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.registers.pc.wrapping_add(offset as u16);

        self.trace_opcode(
            2,
            format!("90 {:02X}", offset),
            format!("BCC ${:04X}", address),
        );

        if !self.registers.get_status_flag(StatusFlag::Carry) {
            self.cycles += 1;

            if self.registers.pc & 0xFF00 != address & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = address;
        }

        self.cycles += 2;
    }

    fn op_tya(&mut self) {
        // TYA - Transfer Index Y To ACC
        // A = Y                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TYA          98        1      2

        self.trace_opcode(1, "98", "TYA");

        self.registers.a = self.registers.y;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_txs(&mut self) {
        // TXS - Transfer Index X To Stack Pointer
        // SP = X                            N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TXS          9A        1      2

        self.trace_opcode(1, "9A", "TXS");

        self.registers.sp = self.registers.x;

        self.cycles += 2;
    }

    // Opcodes A0-AF
    fn op_ldy_imm(&mut self) {
        // LDY - Load Index Y With Memory
        // Y = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     LDY #oper    A0        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("A0 {:02X}", value),
            format!("LDY #${:02X}", value),
        );

        self.registers.y = value;

        let n = self.registers.y & 0x80 != 0;
        let z = self.registers.y == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_ldx_imm(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     LDX #oper    A2        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("A2 {:02X}", value),
            format!("LDX #${:02X}", value),
        );

        self.registers.x = value;

        let n = self.registers.x & 0x80 != 0;
        let z = self.registers.x == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_tay(&mut self) {
        // TAY - Transfer ACC To Index Y
        // Y = A                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TAY          A8        1      2

        self.trace_opcode(1, "A8", "TAY");

        self.registers.y = self.registers.a;

        let n = self.registers.y & 0x80 != 0;
        let z = self.registers.y == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_lda_imm(&mut self) {
        // LDA - Load Accumulator With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     LDA #oper    A9        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("A9 {:02X}", value),
            format!("LDA #${:02X}", value),
        );

        self.registers.a = value;

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_tax(&mut self) {
        // TAX - Transfer ACC To Index X
        // X = A                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TAX          AA        1      2

        self.trace_opcode(1, "AA", "TAX");

        self.registers.x = self.registers.a;

        let n = self.registers.x & 0x80 != 0;
        let z = self.registers.x == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_lda_abs(&mut self) {
        // LDA - Load Accumulator With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      LDA $imm     AD        3     4*

        let address = self.memory_read_u16(self.registers.pc);
        self.registers.pc += 2;

        let value = self.memory_read(address);
        self.registers.a = value;

        self.trace_opcode(
            3,
            format!("AD {:02X}", address),
            format!("LDA ${:04X} = {:02X}", address, value),
        );

        let n = self.registers.a & 0x80 != 0;
        let z = self.registers.a == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        if address & 0xFF00 != (address + 1) & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    // Opcodes B0-BF
    fn op_bcs(&mut self) {
        // BCS - Branch If Carry Set
        // branch if C eq 1                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BCS oper     B0        2    2**

        let offset = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.registers.pc.wrapping_add(offset as u16);

        self.trace_opcode(
            2,
            format!("B0 {:02X}", offset),
            format!("BCS ${:04X}", address),
        );

        if self.registers.get_status_flag(StatusFlag::Carry) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    fn op_clv(&mut self) {
        // CLV - Clear Overflow Flag
        // V = 0                             N Z C I D V
        //                                   - - - - - 0
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       CLV          B8        1      2

        self.trace_opcode(1, "B8", "CLV");

        self.registers.set_status_flag(StatusFlag::Overflow, false);

        self.cycles += 2;
    }

    // Opcodes C0-CF
    fn op_cpy_imm(&mut self) {
        // CPY - Compare Memory And Index Y
        // Y - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     CPY #oper    C0        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("C0 {:02X}", value),
            format!("CPY #${:02X}", value),
        );

        let y = self.registers.y;
        let result = y.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = y >= value;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Carry, c);

        self.cycles += 2;
    }

    fn op_iny(&mut self) {
        // INY - Increment Index Y By One
        // Y = Y + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       INY          C8        1      2

        self.trace_opcode(1, "C8", "INY");

        self.registers.y = self.registers.y.wrapping_add(1);

        let n = self.registers.y & 0x80 != 0;
        let z = self.registers.y == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_cmp_imm(&mut self) {
        // CMP - Compare Memory With Accumulator
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     CMP #oper    C9        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("C9 {:02X}", value),
            format!("CMP #${:02X}", value),
        );

        let a = self.registers.a;
        let result = a.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = a >= value;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Carry, c);

        self.cycles += 2;
    }

    fn op_dex(&mut self) {
        // DEX - Decrement Index X By One
        // X = X - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       DEX          CA        1      2

        self.trace_opcode(1, "CA", "DEX");

        self.registers.x = self.registers.x.wrapping_sub(1);

        let n = self.registers.x & 0x80 != 0;
        let z = self.registers.x == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    // Opcodes D0-DF
    fn op_bne(&mut self) {
        // BNE - Branch If Not Equal
        // branch if Z neq 0                 N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BNE oper     D0        2    2**

        let offset = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.registers.pc.wrapping_add(offset as u16);

        self.trace_opcode(
            2,
            format!("D0 {:02X}", offset),
            format!("BNE ${:04X}", address),
        );

        if !self.registers.get_status_flag(StatusFlag::Zero) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    fn op_cld(&mut self) {
        // CLD - Clear Decimal Mode
        // D = 0                             N Z C I D V
        //                                   - - - - 0 -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       CLD          D8        1      2

        self.trace_opcode(1, "D8", "CLD");

        self.registers.set_status_flag(StatusFlag::Decimal, false);

        self.cycles += 2;
    }

    // Opcodes E0-EF
    fn op_cpx_imm(&mut self) {
        // CPX - Compare Memory And Index X
        // X - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     CPX #oper    E0        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("E0 {:02X}", value),
            format!("CPX #${:02X}", value),
        );

        let x = self.registers.x;
        let result = x.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = x >= value;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Carry, c);

        self.cycles += 2;
    }

    fn op_inx(&mut self) {
        // INX - Increment Index X By One
        // X = X + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       INX          E8        1      2

        self.trace_opcode(1, "E8", "INX");

        self.registers.x = self.registers.x.wrapping_add(1);

        let n = self.registers.x & 0x80 != 0;
        let z = self.registers.x == 0;

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_sbc_imm(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     SBC #oper    E9        2      2

        let value = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        self.trace_opcode(
            2,
            format!("E9 {:02X}", value),
            format!("SBC #${:02X}", value),
        );

        let a = self.registers.a;
        let c = self.registers.get_status_flag(StatusFlag::Carry) as u8;
        let result = a.wrapping_sub(value).wrapping_sub(1 - c);
        self.registers.a = result;

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = a >= value;
        let v = ((a ^ result) & 0x80 != 0) && ((a ^ value) & 0x80 != 0);

        self.registers.set_status_flag(StatusFlag::Negative, n);
        self.registers.set_status_flag(StatusFlag::Zero, z);
        self.registers.set_status_flag(StatusFlag::Carry, c);
        self.registers.set_status_flag(StatusFlag::Overflow, v);

        self.cycles += 2;
    }

    fn op_nop(&mut self) {
        // NOP - No Operation
        //                                   N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       NOP          EA        1      2

        self.trace_opcode(1, "EA", "NOP");

        self.cycles += 2;
    }

    // Opcodes F0-FF
    fn op_beq(&mut self) {
        // BEQ - Branch If Equal
        // branch if Z eq 1                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BEQ oper     F0        2    2**

        let offset = self.memory_read(self.registers.pc);
        self.registers.pc += 1;

        let address = self.registers.pc.wrapping_add(offset as u16);

        self.trace_opcode(
            2,
            format!("F0 {:02X}", offset),
            format!("BEQ ${:04X}", address),
        );

        if self.registers.get_status_flag(StatusFlag::Zero) {
            self.registers.pc = address;

            self.cycles += 1;

            if self.registers.pc & 0xFF00 != (self.registers.pc + 1) & 0xFF00 {
                self.cycles += 1;
            }
        }

        self.cycles += 2;
    }

    fn op_sed(&mut self) {
        // SED - Set Decimal Flag
        // D = 1                             N Z C I D V
        //                                   - - - - 1 -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       SED          F8        1      2

        self.trace_opcode(1, "F8", "SED");

        self.registers.set_status_flag(StatusFlag::Decimal, true);

        self.cycles += 2;
    }

    // Memory
    fn memory_read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x401F => self.memory.read(address),
            0x4020..=0xFFFF => {
                if let Some(mapper) = self.mapper.as_ref() {
                    mapper.read(address)
                } else {
                    0x00
                }
            }
        }
    }

    fn memory_read_u16(&self, address: u16) -> u16 {
        let lo = self.memory_read(address);
        let hi = self.memory_read(address + 1);
        ((hi as u16) << 8) | lo as u16
    }

    fn memory_write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x401F => self.memory.write(address, value),
            0x4020..=0xFFFF => {
                if let Some(mapper) = self.mapper.as_mut() {
                    mapper.write(address, value);
                }
            }
        }
    }

    fn stack_pop(&mut self) -> u8 {
        self.registers.sp = self.registers.sp.wrapping_add(1);

        let value = self.memory_read(0x0100 + self.registers.sp as u16);
        self.memory_write(0x0100 + self.registers.sp as u16, 0x00);

        value
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop();
        let hi = self.stack_pop();
        ((hi as u16) << 8) | lo as u16
    }

    fn stack_push(&mut self, value: u8) {
        self.memory_write(0x0100 + self.registers.sp as u16, value);
        self.registers.sp = self.registers.sp.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, value: u16) {
        let lo = value as u8;
        let hi = (value >> 8) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn trace_opcode<S: Into<String>>(&mut self, pc_offset: u16, opcode: S, disasm: S) {
        let opcode = opcode.into();
        let disasm = disasm.into();

        let line = format!(
            "{:04X}  {}{}{}{}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{}  <-[{:02X}, {:02X}]",
            self.registers.pc - pc_offset,
            opcode,
            " ".repeat(10 - opcode.len()),
            disasm,
            " ".repeat(32 - disasm.len()),
            self.registers.a,
            self.registers.x,
            self.registers.y,
            self.registers.p,
            self.registers.sp,
            self.cycles,
            self.memory_read(0x0100 + self.registers.sp as u16 + 1),
            self.memory_read(0x0100 + self.registers.sp as u16 + 2),
        );

        trace!("{}", line);
        self.trace_log.write_all(&line.as_bytes()[..73]).unwrap();
        self.trace_log.write_all(b"\n").unwrap();
    }
}

enum StatusFlag {
    Negative,
    Overflow,
    Break,
    Decimal,
    Interrupt,
    Zero,
    Carry,
}

struct Registers {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x0000,
            sp: 0x0,
            p: 0x00,
        }
    }

    pub fn get_status_flag(&self, flag: StatusFlag) -> bool {
        match flag {
            StatusFlag::Negative => self.p & 0b1000_0000 != 0,
            StatusFlag::Overflow => self.p & 0b0100_0000 != 0,
            StatusFlag::Break => self.p & 0b0001_0000 != 0,
            StatusFlag::Decimal => self.p & 0b0000_1000 != 0,
            StatusFlag::Interrupt => self.p & 0b0000_0100 != 0,
            StatusFlag::Zero => self.p & 0b0000_0010 != 0,
            StatusFlag::Carry => self.p & 0b0000_0001 != 0,
        }
    }

    pub fn set_status_flag(&mut self, flag: StatusFlag, value: bool) {
        match flag {
            StatusFlag::Negative => self.p = (self.p & 0b0111_1111) | ((value as u8) << 7),
            StatusFlag::Overflow => self.p = (self.p & 0b1011_1111) | ((value as u8) << 6),
            StatusFlag::Break => self.p = (self.p & 0b1110_1111) | ((value as u8) << 4),
            StatusFlag::Decimal => self.p = (self.p & 0b1111_0111) | ((value as u8) << 3),
            StatusFlag::Interrupt => self.p = (self.p & 0b1111_1011) | ((value as u8) << 2),
            StatusFlag::Zero => self.p = (self.p & 0b1111_1101) | ((value as u8) << 1),
            StatusFlag::Carry => self.p = (self.p & 0b1111_1110) | (value as u8),
        }
    }
}

struct Memory {
    ram: [u8; 0x4020],
}

impl Memory {
    pub fn new() -> Self {
        Self {
            ram: [0x00; 0x4020],
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1FFF => self.ram[address as usize],
            0x2000..=0x2007 => 0x00, // TODO: I/O registers
            _ => panic!("Invalid memory read address: 0x{:04X}", address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x07FF => {
                self.ram[address as usize] = value;

                for i in 0..=3 {
                    self.ram[(address + 0x0800 * i) as usize] = value;
                }
            }
            0x0800..=0x1FFF => self.ram[(address - 0x0800) as usize] = value,
            0x2000..=0x2007 => {
                self.ram[address as usize] = value;

                for i in 0..=7 {
                    self.ram[(address + 0x08 * i) as usize] = value;
                }
            }
            0x2008..=0x3FFF => self.ram[(address - 0x2008) as usize] = value,
            _ => panic!("Invalid memory write address: 0x{:04X}", address),
        }
    }
}

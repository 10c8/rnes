use std::{cell::RefCell, fs::File, rc::Rc};

#[allow(unused_imports)]
use std::io::Write;

#[cfg(feature = "tom_harte_tests")]
use std::io::Read;

use crate::{cartridge::Cartridge, ppu::PPU};

enum StatusFlag {
    Negative,
    Overflow,
    Break,
    Decimal,
    Interrupt,
    Zero,
    Carry,
}

#[cfg(feature = "tom_harte_tests")]
#[derive(serde::Deserialize)]
struct TomHarteTestState {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    ram: Vec<(u16, u8)>,
}

#[cfg(feature = "tom_harte_tests")]
#[derive(serde::Deserialize)]
struct TomHarteTest {
    name: String,
    #[serde(rename = "initial")]
    initial_state: TomHarteTestState,
    #[serde(rename = "final")]
    final_state: TomHarteTestState,
    #[serde(skip_deserializing)]
    cycles: usize,
}

#[cfg(feature = "tom_harte_tests")]
type TomHarteTestList = Vec<TomHarteTest>;

pub struct CPU {
    pc: u16,
    sp: u8,

    a: u8,
    x: u8,
    y: u8,

    n: bool,
    v: bool,
    bit_5: bool,
    b: bool,
    d: bool,
    i: bool,
    z: bool,
    c: bool,

    ram: [u8; 0x4020],

    cycles: usize,
    last_cycle: usize,

    pub ppu: PPU,
    cartridge: Option<Rc<RefCell<Cartridge>>>,

    #[allow(dead_code)]
    trace_log: File,

    #[cfg(feature = "tom_harte_tests")]
    is_tom_harte_test: bool,
    #[cfg(feature = "tom_harte_tests")]
    tom_harte_memory: [u8; 0x10000],
}

impl CPU {
    pub fn new() -> Self {
        let trace_log = File::create("trace.log").unwrap();

        let ppu = PPU::new();

        Self {
            pc: 0x0000,
            sp: 0x0,

            a: 0x00,
            x: 0x00,
            y: 0x00,

            n: false,
            v: false,
            bit_5: false,
            b: false,
            d: false,
            i: false,
            z: false,
            c: false,

            ram: [0x00; 0x4020],

            cycles: 0,
            last_cycle: 0,

            ppu,
            cartridge: None,

            trace_log,

            #[cfg(feature = "tom_harte_tests")]
            is_tom_harte_test: false,
            #[cfg(feature = "tom_harte_tests")]
            tom_harte_memory: [0x00; 0x10000],
        }
    }

    pub fn reset(&mut self) {
        let pc_lo = self.memory_read(0xFFFC);
        let pc_hi = self.memory_read(0xFFFD);
        self.pc = ((pc_hi as u16) << 8) | pc_lo as u16;
        log::trace!("Entry point: {:#06X}", self.pc);

        // self.pc = 0xC000; // Nestest.nes automation mode

        self.sp = 0xFD;

        self.a = 0;
        self.x = 0;
        self.y = 0;

        self.set_status_register(0x24);

        self.cycles = 7;

        for _ in 0..self.cycles * 3 {
            self.run_ppu_cycle();
        }
    }

    pub fn load_cartridge(&mut self, cartridge: Cartridge) {
        self.ppu.load_cartridge(&cartridge);
        self.cartridge = Some(Rc::new(RefCell::new(cartridge)));
    }

    pub fn step(&mut self) {
        let opcode = self.memory_read(self.pc);
        self.pc += 1;

        self.last_cycle = self.cycles;

        if self.ppu.nmi_occurred {
            self.ppu.nmi_occurred = false;
            self.int_nmi();
            return;
        }

        match (opcode & 0xF0) >> 4 {
            0x0 => match opcode {
                0x00 => self.op_brk(),
                0x01 => self.op_ora_ind_x(),
                0x04 => self.op_nop_zpg(0),
                0x05 => self.op_ora_zpg(),
                0x06 => self.op_asl_zpg(),
                0x08 => self.op_php(),
                0x09 => self.op_ora_imm(),
                0x0A => self.op_asl_acc(),
                0x0C => self.op_nop_abs(),
                0x0D => self.op_ora_abs(),
                0x0E => self.op_asl_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x1 => match opcode {
                0x10 => self.op_bpl(),
                0x11 => self.op_ora_ind_y(),
                0x14 => self.op_nop_zpg_x(1),
                0x15 => self.op_ora_zpg_x(),
                0x16 => self.op_asl_zpg_x(),
                0x18 => self.op_clc(),
                0x19 => self.op_ora_abs_y(),
                0x1A => self.op_nop(1),
                0x1C => self.op_nop_abs_x(1),
                0x1D => self.op_ora_abs_x(),
                0x1E => self.op_asl_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x2 => match opcode {
                0x20 => self.op_jsr(),
                0x21 => self.op_and_ind_x(),
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
                0x31 => self.op_and_ind_y(),
                0x34 => self.op_nop_zpg_x(3),
                0x35 => self.op_and_zpg_x(),
                0x36 => self.op_rol_zpg_x(),
                0x38 => self.op_sec(),
                0x39 => self.op_and_abs_y(),
                0x3A => self.op_nop(3),
                0x3C => self.op_nop_abs_x(3),
                0x3D => self.op_and_abs_x(),
                0x3E => self.op_rol_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x4 => match opcode {
                0x40 => self.op_rti(),
                0x41 => self.op_eor_ind_x(),
                0x44 => self.op_nop_zpg(4),
                0x45 => self.op_eor_zpg(),
                0x46 => self.op_lsr_zpg(),
                0x48 => self.op_pha(),
                0x49 => self.op_eor_imm(),
                0x4A => self.op_lsr_acc(),
                0x4C => self.op_jmp_abs(),
                0x4D => self.op_eor_abs(),
                0x4E => self.op_lsr_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x5 => match opcode {
                0x50 => self.op_bvc(),
                0x51 => self.op_eor_ind_y(),
                0x54 => self.op_nop_zpg_x(5),
                0x55 => self.op_eor_zpg_x(),
                0x56 => self.op_lsr_zpg_x(),
                0x58 => self.op_cli(),
                0x59 => self.op_eor_abs_y(),
                0x5A => self.op_nop(5),
                0x5C => self.op_nop_abs_x(5),
                0x5D => self.op_eor_abs_x(),
                0x5E => self.op_lsr_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x6 => match opcode {
                0x60 => self.op_rts(),
                0x61 => self.op_adc_ind_x(),
                0x64 => self.op_nop_zpg(6),
                0x65 => self.op_adc_zpg(),
                0x66 => self.op_ror_zpg(),
                0x68 => self.op_pla(),
                0x69 => self.op_adc_imm(),
                0x6A => self.op_ror_acc(),
                0x6C => self.op_jmp_ind(),
                0x6D => self.op_adc_abs(),
                0x6E => self.op_ror_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x7 => match opcode {
                0x70 => self.op_bvs(),
                0x71 => self.op_adc_ind_y(),
                0x74 => self.op_nop_zpg_x(7),
                0x75 => self.op_adc_zpg_x(),
                0x76 => self.op_ror_zpg_x(),
                0x78 => self.op_sei(),
                0x79 => self.op_adc_abs_y(),
                0x7A => self.op_nop(7),
                0x7C => self.op_nop_abs_x(7),
                0x7D => self.op_adc_abs_x(),
                0x7E => self.op_ror_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x8 => match opcode {
                0x80 | 0x82 | 0x89 => self.op_nop_imm(),
                0x81 => self.op_sta_ind_x(),
                0x84 => self.op_sty_zpg(),
                0x85 => self.op_sta_zpg(),
                0x86 => self.op_stx_zpg(),
                0x88 => self.op_dey(),
                0x8A => self.op_txa(),
                0x8C => self.op_sty_abs(),
                0x8D => self.op_sta_abs(),
                0x8E => self.op_stx_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0x9 => match opcode {
                0x90 => self.op_bcc(),
                0x91 => self.op_sta_ind_y(),
                0x94 => self.op_sty_zpg_x(),
                0x95 => self.op_sta_zpg_x(),
                0x96 => self.op_stx_zpg_y(),
                0x98 => self.op_tya(),
                0x99 => self.op_sta_abs_y(),
                0x9A => self.op_txs(),
                0x9D => self.op_sta_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xA => match opcode {
                0xA0 => self.op_ldy_imm(),
                0xA1 => self.op_lda_ind_x(),
                0xA2 => self.op_ldx_imm(),
                0xA4 => self.op_ldy_zpg(),
                0xA5 => self.op_lda_zpg(),
                0xA6 => self.op_ldx_zpg(),
                0xA8 => self.op_tay(),
                0xA9 => self.op_lda_imm(),
                0xAA => self.op_tax(),
                0xAC => self.op_ldy_abs(),
                0xAD => self.op_lda_abs(),
                0xAE => self.op_ldx_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xB => match opcode {
                0xB0 => self.op_bcs(),
                0xB1 => self.op_lda_ind_y(),
                0xB4 => self.op_ldy_zpg_x(),
                0xB5 => self.op_lda_zpg_x(),
                0xB6 => self.op_ldx_zpg_y(),
                0xB8 => self.op_clv(),
                0xB9 => self.op_lda_abs_y(),
                0xBA => self.op_tsx(),
                0xBC => self.op_ldy_abs_x(),
                0xBD => self.op_lda_abs_x(),
                0xBE => self.op_ldx_abs_y(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xC => match opcode {
                0xC0 => self.op_cpy_imm(),
                0xC1 => self.op_cmp_ind_x(),
                0xC2 => self.op_nop_imm(),
                0xC4 => self.op_cpy_zpg(),
                0xC5 => self.op_cmp_zpg(),
                0xC6 => self.op_dec_zpg(),
                0xC8 => self.op_iny(),
                0xC9 => self.op_cmp_imm(),
                0xCA => self.op_dex(),
                0xCC => self.op_cpy_abs(),
                0xCD => self.op_cmp_abs(),
                0xCE => self.op_dec_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xD => match opcode {
                0xD0 => self.op_bne(),
                0xD1 => self.op_cmp_ind_y(),
                0xD4 => self.op_nop_zpg_x(0xD),
                0xD5 => self.op_cmp_zpg_x(),
                0xD6 => self.op_dec_zpg_x(),
                0xD8 => self.op_cld(),
                0xD9 => self.op_cmp_abs_y(),
                0xDA => self.op_nop(0xD),
                0xDC => self.op_nop_abs_x(0xD),
                0xDD => self.op_cmp_abs_x(),
                0xDE => self.op_dec_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xE => match opcode {
                0xE0 => self.op_cpx_imm(),
                0xE1 => self.op_sbc_ind_x(),
                0xE2 => self.op_nop_imm(),
                0xE4 => self.op_cpx_zpg(),
                0xE5 => self.op_sbc_zpg(),
                0xE6 => self.op_inc_zpg(),
                0xE8 => self.op_inx(),
                0xE9 => self.op_sbc_imm(),
                0xEA => self.op_nop_official(),
                0xEC => self.op_cpx_abs(),
                0xED => self.op_sbc_abs(),
                0xEE => self.op_inc_abs(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            0xF => match opcode {
                0xF0 => self.op_beq(),
                0xF1 => self.op_sbc_ind_y(),
                0xF4 => self.op_nop_zpg_x(0xF),
                0xF5 => self.op_sbc_zpg_x(),
                0xF6 => self.op_inc_zpg_x(),
                0xF8 => self.op_sed(),
                0xF9 => self.op_sbc_abs_y(),
                0xFA => self.op_nop(0xF),
                0xFC => self.op_nop_abs_x(0xF),
                0xFD => self.op_sbc_abs_x(),
                0xFE => self.op_inc_abs_x(),
                _ => panic!("Invalid opcode: {:#04X}", opcode),
            },
            _ => panic!("Invalid opcode: {:#04X}", opcode),
        }
    }

    #[cfg(feature = "tom_harte_tests")]
    pub fn run_tomharte_tests(&mut self) {
        self.is_tom_harte_test = true;

        let test_dir = "./resources/tests/tomharte_v1";
        let mut opcodes = (0x01..0x07).collect::<Vec<u8>>();
        opcodes.extend((0x09..0x0F).collect::<Vec<u8>>());
        opcodes.extend((0x10..0x1F).collect::<Vec<u8>>());
        opcodes.extend(vec![0x2A, 0x42]);
        opcodes.extend((0x48..0x4C).collect::<Vec<u8>>());
        opcodes.push(0x4E);
        opcodes.extend((0x50..0x5F).collect::<Vec<u8>>());
        opcodes.extend((0x60..0x6F).collect::<Vec<u8>>());
        opcodes.extend((0x80..0x8F).collect::<Vec<u8>>());
        opcodes.extend((0x90..0x97).collect::<Vec<u8>>());
        opcodes.extend((0x99..0x9F).collect::<Vec<u8>>());
        opcodes.extend(vec![0xA0, 0xA2, 0xA4, 0xA6]);
        opcodes.extend((0xA8..0xAC).collect::<Vec<u8>>());
        opcodes.extend(vec![0xAE, 0xC2, 0xC4, 0xC6, 0xC8]);
        opcodes.extend((0xCA..0xCC).collect::<Vec<u8>>());
        opcodes.push(0xCE);
        opcodes.extend((0xD0..0xDF).collect::<Vec<u8>>());
        opcodes.extend((0xE0..0xE8).collect::<Vec<u8>>());
        opcodes.extend((0xEA..0xEF).collect::<Vec<u8>>());

        for opcode in opcodes {
            let opcode = opcode.rotate_left(4);

            let file = format!("{}/{:02X}.json", test_dir, opcode);
            let mut file = File::open(file).unwrap();
            let mut contents = String::new();
            file.read_to_string(&mut contents).unwrap();

            log::info!("Testing opcode {:02X}...", opcode);

            let tests: TomHarteTestList = serde_json::from_str(&contents).unwrap();
            for test in tests {
                // self.tom_harte_memory = [0; 0x10000];

                self.pc = test.initial_state.pc.wrapping_add(1);
                self.sp = test.initial_state.s;
                self.a = test.initial_state.a;
                self.x = test.initial_state.x;
                self.y = test.initial_state.y;
                self.set_status_register(test.initial_state.p);

                for (address, value) in test.initial_state.ram {
                    self.memory_write(address, value);
                }

                match (opcode & 0xF0) >> 4 {
                    0x0 => match opcode {
                        0x00 => self.op_brk(),
                        0x01 => self.op_ora_ind_x(),
                        0x04 => self.op_nop_zpg(0),
                        0x05 => self.op_ora_zpg(),
                        0x06 => self.op_asl_zpg(),
                        0x08 => self.op_php(),
                        0x09 => self.op_ora_imm(),
                        0x0A => self.op_asl_acc(),
                        0x0C => self.op_nop_abs(),
                        0x0D => self.op_ora_abs(),
                        0x0E => self.op_asl_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x1 => match opcode {
                        0x10 => self.op_bpl(),
                        0x11 => self.op_ora_ind_y(),
                        0x14 => self.op_nop_zpg_x(1),
                        0x15 => self.op_ora_zpg_x(),
                        0x16 => self.op_asl_zpg_x(),
                        0x18 => self.op_clc(),
                        0x19 => self.op_ora_abs_y(),
                        0x1A => self.op_nop(1),
                        0x1C => self.op_nop_abs_x(1),
                        0x1D => self.op_ora_abs_x(),
                        0x1E => self.op_asl_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x2 => match opcode {
                        0x20 => self.op_jsr(),
                        0x21 => self.op_and_ind_x(),
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
                        0x31 => self.op_and_ind_y(),
                        0x34 => self.op_nop_zpg_x(3),
                        0x35 => self.op_and_zpg_x(),
                        0x36 => self.op_rol_zpg_x(),
                        0x38 => self.op_sec(),
                        0x39 => self.op_and_abs_y(),
                        0x3A => self.op_nop(3),
                        0x3C => self.op_nop_abs_x(3),
                        0x3D => self.op_and_abs_x(),
                        0x3E => self.op_rol_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x4 => match opcode {
                        0x40 => self.op_rti(),
                        0x41 => self.op_eor_ind_x(),
                        0x44 => self.op_nop_zpg(4),
                        0x45 => self.op_eor_zpg(),
                        0x46 => self.op_lsr_zpg(),
                        0x48 => self.op_pha(),
                        0x49 => self.op_eor_imm(),
                        0x4A => self.op_lsr_acc(),
                        0x4C => self.op_jmp_abs(),
                        0x4D => self.op_eor_abs(),
                        0x4E => self.op_lsr_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x5 => match opcode {
                        0x50 => self.op_bvc(),
                        0x51 => self.op_eor_ind_y(),
                        0x54 => self.op_nop_zpg_x(5),
                        0x55 => self.op_eor_zpg_x(),
                        0x56 => self.op_lsr_zpg_x(),
                        0x58 => self.op_cli(),
                        0x59 => self.op_eor_abs_y(),
                        0x5A => self.op_nop(5),
                        0x5C => self.op_nop_abs_x(5),
                        0x5D => self.op_eor_abs_x(),
                        0x5E => self.op_lsr_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x6 => match opcode {
                        0x60 => self.op_rts(),
                        0x61 => self.op_adc_ind_x(),
                        0x64 => self.op_nop_zpg(6),
                        0x65 => self.op_adc_zpg(),
                        0x66 => self.op_ror_zpg(),
                        0x68 => self.op_pla(),
                        0x69 => self.op_adc_imm(),
                        0x6A => self.op_ror_acc(),
                        0x6C => self.op_jmp_ind(),
                        0x6D => self.op_adc_abs(),
                        0x6E => self.op_ror_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x7 => match opcode {
                        0x70 => self.op_bvs(),
                        0x71 => self.op_adc_ind_y(),
                        0x74 => self.op_nop_zpg_x(7),
                        0x75 => self.op_adc_zpg_x(),
                        0x76 => self.op_ror_zpg_x(),
                        0x78 => self.op_sei(),
                        0x79 => self.op_adc_abs_y(),
                        0x7A => self.op_nop(7),
                        0x7C => self.op_nop_abs_x(7),
                        0x7D => self.op_adc_abs_x(),
                        0x7E => self.op_ror_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x8 => match opcode {
                        0x80 | 0x82 | 0x89 => self.op_nop_imm(),
                        0x81 => self.op_sta_ind_x(),
                        0x84 => self.op_sty_zpg(),
                        0x85 => self.op_sta_zpg(),
                        0x86 => self.op_stx_zpg(),
                        0x88 => self.op_dey(),
                        0x8A => self.op_txa(),
                        0x8C => self.op_sty_abs(),
                        0x8D => self.op_sta_abs(),
                        0x8E => self.op_stx_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0x9 => match opcode {
                        0x90 => self.op_bcc(),
                        0x91 => self.op_sta_ind_y(),
                        0x94 => self.op_sty_zpg_x(),
                        0x95 => self.op_sta_zpg_x(),
                        0x96 => self.op_stx_zpg_y(),
                        0x98 => self.op_tya(),
                        0x99 => self.op_sta_abs_y(),
                        0x9A => self.op_txs(),
                        0x9D => self.op_sta_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xA => match opcode {
                        0xA0 => self.op_ldy_imm(),
                        0xA1 => self.op_lda_ind_x(),
                        0xA2 => self.op_ldx_imm(),
                        0xA4 => self.op_ldy_zpg(),
                        0xA5 => self.op_lda_zpg(),
                        0xA6 => self.op_ldx_zpg(),
                        0xA8 => self.op_tay(),
                        0xA9 => self.op_lda_imm(),
                        0xAA => self.op_tax(),
                        0xAC => self.op_ldy_abs(),
                        0xAD => self.op_lda_abs(),
                        0xAE => self.op_ldx_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xB => match opcode {
                        0xB0 => self.op_bcs(),
                        0xB1 => self.op_lda_ind_y(),
                        0xB4 => self.op_ldy_zpg_x(),
                        0xB5 => self.op_lda_zpg_x(),
                        0xB6 => self.op_ldx_zpg_y(),
                        0xB8 => self.op_clv(),
                        0xB9 => self.op_lda_abs_y(),
                        0xBA => self.op_tsx(),
                        0xBC => self.op_ldy_abs_x(),
                        0xBD => self.op_lda_abs_x(),
                        0xBE => self.op_ldx_abs_y(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xC => match opcode {
                        0xC0 => self.op_cpy_imm(),
                        0xC1 => self.op_cmp_ind_x(),
                        0xC2 => self.op_nop_imm(),
                        0xC4 => self.op_cpy_zpg(),
                        0xC5 => self.op_cmp_zpg(),
                        0xC6 => self.op_dec_zpg(),
                        0xC8 => self.op_iny(),
                        0xC9 => self.op_cmp_imm(),
                        0xCA => self.op_dex(),
                        0xCC => self.op_cpy_abs(),
                        0xCD => self.op_cmp_abs(),
                        0xCE => self.op_dec_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xD => match opcode {
                        0xD0 => self.op_bne(),
                        0xD1 => self.op_cmp_ind_y(),
                        0xD4 => self.op_nop_zpg_x(0xD),
                        0xD5 => self.op_cmp_zpg_x(),
                        0xD6 => self.op_dec_zpg_x(),
                        0xD8 => self.op_cld(),
                        0xD9 => self.op_cmp_abs_y(),
                        0xDA => self.op_nop(0xD),
                        0xDC => self.op_nop_abs_x(0xD),
                        0xDD => self.op_cmp_abs_x(),
                        0xDE => self.op_dec_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xE => match opcode {
                        0xE0 => self.op_cpx_imm(),
                        0xE1 => self.op_sbc_ind_x(),
                        0xE2 => self.op_nop_imm(),
                        0xE4 => self.op_cpx_zpg(),
                        0xE5 => self.op_sbc_zpg(),
                        0xE6 => self.op_inc_zpg(),
                        0xE8 => self.op_inx(),
                        0xE9 => self.op_sbc_imm(),
                        0xEA => self.op_nop_official(),
                        0xEC => self.op_cpx_abs(),
                        0xED => self.op_sbc_abs(),
                        0xEE => self.op_inc_abs(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    0xF => match opcode {
                        0xF0 => self.op_beq(),
                        0xF1 => self.op_sbc_ind_y(),
                        0xF4 => self.op_nop_zpg_x(0xF),
                        0xF5 => self.op_sbc_zpg_x(),
                        0xF6 => self.op_inc_zpg_x(),
                        0xF8 => self.op_sed(),
                        0xF9 => self.op_sbc_abs_y(),
                        0xFA => self.op_nop(0xF),
                        0xFC => self.op_nop_abs_x(0xF),
                        0xFD => self.op_sbc_abs_x(),
                        0xFE => self.op_inc_abs_x(),
                        _ => panic!("Invalid opcode: {:#04X}", opcode),
                    },
                    _ => panic!("Invalid opcode: {:#04X}", opcode),
                }

                if self.pc != test.final_state.pc {
                    panic!(
                        "{}  Expected PC: {:04X}, actual PC: {:04X}",
                        test.name, test.final_state.pc, self.pc
                    );
                }

                if self.sp != test.final_state.s {
                    panic!(
                        "{}  Expected SP: {:02X}, actual SP: {:02X}",
                        test.name, test.final_state.s, self.sp
                    );
                }

                if self.a != test.final_state.a {
                    panic!(
                        "{}  Expected A: {:02X}, actual A: {:02X}",
                        test.name, test.final_state.a, self.a
                    );
                }

                if self.x != test.final_state.x {
                    panic!(
                        "{}  Expected X: {:02X}, actual X: {:02X}",
                        test.name, test.final_state.x, self.x
                    );
                }

                if self.y != test.final_state.y {
                    panic!(
                        "{}  Expected Y: {:02X}, actual Y: {:02X}",
                        test.name, test.final_state.y, self.y
                    );
                }

                if self.get_status_register() != test.final_state.p {
                    panic!(
                        "{}  Expected P: {:02X}, actual P: {:02X}",
                        test.name,
                        test.final_state.p,
                        self.get_status_register()
                    );
                }

                for (address, value) in test.final_state.ram {
                    let read = self.memory_read(address);
                    if read != value {
                        panic!(
                            "{}  Expected RAM[{:04X}]: {:02X}, actual RAM[{:04X}]: {:02X}",
                            test.name, address, value, address, read
                        );
                    }
                }
            }
        }
    }

    pub fn get_cycles(&self) -> usize {
        self.cycles
    }

    pub fn get_elapsed_cycles(&self) -> usize {
        self.cycles - self.last_cycle
    }

    // PPU
    pub fn run_ppu_cycle(&mut self) {
        self.ppu.cycle();
    }

    pub fn ppu_framebuffer_has_changed(&mut self) -> bool {
        self.ppu.framebuffer_has_changed()
    }

    pub fn get_ppu_framebuffer(&self) -> &[u8] {
        self.ppu.get_framebuffer()
    }

    // Interrupts
    fn int_nmi(&mut self) {
        self.stack_push_u16(self.pc - 1);

        let status = self.get_status_register();
        self.stack_push((status & 0b1110_1111) | 0b0010_0000);

        self.set_status_flag(StatusFlag::Interrupt, true);

        self.cycles += 2;

        for _ in 0..6 {
            self.ppu.cycle();
        }

        self.pc = self.memory_read_u16(0xFFFA);
    }

    // Addressing
    fn zeropage(&mut self) -> (u16, u8) {
        let address = self.memory_read(self.pc) as u16;
        self.pc = self.pc.wrapping_add(1);

        let value = self.memory_read(address);

        (address, value)
    }

    fn immediate(&mut self) -> u8 {
        let value = self.memory_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        value
    }

    fn absolute(&mut self) -> (u16, u8) {
        let address = self.memory_read_u16(self.pc);
        self.pc = self.pc.wrapping_add(2);

        let value = self.memory_read(address);

        (address, value)
    }

    fn indexed_absolute(&mut self, index: u8) -> (u16, u16, u8) {
        let operator = self.memory_read_u16(self.pc);
        self.pc = self.pc.wrapping_add(2);

        let address = operator.wrapping_add(index as u16);

        let value = self.memory_read(address);

        (operator, address, value)
    }

    fn indexed_zeropage(&mut self, index: u8) -> (u8, u8, u8) {
        let operator = self.memory_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let address = operator.wrapping_add(index);

        let value = self.memory_read(address as u16);

        (operator, address, value)
    }

    fn pre_indexed_indirect(&mut self) -> (u8, u8, u16, u8) {
        let operator = self.memory_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let indirect = operator.wrapping_add(self.x);

        let address_lo = self.memory_read(indirect as u16) as u16;
        let address_hi = self.memory_read(indirect.wrapping_add(1) as u16) as u16;
        let address = (address_hi << 8) | address_lo;

        let value = self.memory_read(address);

        (operator, indirect, address, value)
    }

    fn post_indexed_indirect(&mut self) -> (u8, u16, u16, u8) {
        let operator = self.memory_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let base_lo = self.memory_read(operator as u16) as u16;
        let base_hi = self.memory_read(operator.wrapping_add(1) as u16) as u16;
        let base = (base_hi << 8) | base_lo;

        let address = base.wrapping_add(self.y as u16);

        let value = self.memory_read(address);

        (operator, base, address, value)
    }

    fn relative(&mut self) -> (i8, u16) {
        let offset = self.memory_read(self.pc) as i8;
        self.pc = self.pc.wrapping_add(1);

        let address = self.pc.wrapping_add_signed(offset as i16);

        (offset, address)
    }

    // Operations
    fn acc_load(&mut self, value: u8) {
        self.a = value;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn acc_add(&mut self, value: u8) {
        let a = self.a;
        let carry = self.get_status_flag(StatusFlag::Carry);
        let sum = (a as u16) + (value as u16) + (carry as u16);
        let result = sum as u8;

        self.a = result;

        let n = sum & 0x80 != 0;
        let z = result == 0;
        let c = sum > 0xFF;
        let v = (a ^ result) & ((value ^ result) & 0x80) != 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Overflow, v);
    }

    fn acc_subtract(&mut self, value: u8) {
        self.acc_add(value ^ 0xFF);
    }

    fn acc_compare(&mut self, value: u8) {
        let a = self.a;
        let result = a.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = a >= value;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);
    }

    fn acc_and(&mut self, value: u8) {
        self.a &= value;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn acc_or(&mut self, value: u8) {
        self.a |= value;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn acc_xor(&mut self, value: u8) {
        self.a ^= value;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn index_compare(&mut self, index: u8, value: u8) {
        let result = index.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = index >= value;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);
    }

    fn memory_shl(&mut self, address: u16, value: u8) {
        let result = value.wrapping_shl(1);

        self.memory_write(address, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn x_load(&mut self, value: u8) {
        self.x = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    fn y_load(&mut self, value: u8) {
        self.y = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
    }

    // Branching
    fn branch_if(&mut self, flag: StatusFlag, status: bool, address: u16) {
        if self.get_status_flag(flag) == status {
            self.cycles += 1;

            if self.pc & 0xFF00 != address & 0xFF00 {
                self.cycles += 1;
            }

            self.pc = address;
        }
    }

    // Registers
    fn get_status_register(&self) -> u8 {
        let mut p = 0x00;
        p |= (self.n as u8) << 7;
        p |= (self.v as u8) << 6;
        p |= (self.bit_5 as u8) << 5;
        p |= (self.b as u8) << 4;
        p |= (self.d as u8) << 3;
        p |= (self.i as u8) << 2;
        p |= (self.z as u8) << 1;
        p |= self.c as u8;
        p
    }

    fn set_status_register(&mut self, value: u8) {
        self.n = value >> 7 == 1;
        self.v = (value >> 6) & 0x01 == 1;
        self.bit_5 = (value >> 5) & 0x01 == 1;
        self.b = (value >> 4) & 0x01 == 1;
        self.d = (value >> 3) & 0x01 == 1;
        self.i = (value >> 2) & 0x01 == 1;
        self.z = (value >> 1) & 0x01 == 1;
        self.c = (value >> 0) & 0x01 == 1;
    }

    fn get_status_flag(&self, flag: StatusFlag) -> bool {
        match flag {
            StatusFlag::Negative => self.n,
            StatusFlag::Overflow => self.v,
            StatusFlag::Break => self.b,
            StatusFlag::Decimal => self.d,
            StatusFlag::Interrupt => self.i,
            StatusFlag::Zero => self.z,
            StatusFlag::Carry => self.c,
        }
    }

    fn set_status_flag(&mut self, flag: StatusFlag, value: bool) {
        match flag {
            StatusFlag::Negative => self.n = value,
            StatusFlag::Overflow => self.v = value,
            StatusFlag::Break => self.b = value,
            StatusFlag::Decimal => self.d = value,
            StatusFlag::Interrupt => self.i = value,
            StatusFlag::Zero => self.z = value,
            StatusFlag::Carry => self.c = value,
        }
    }

    // Memory
    fn memory_read(&mut self, address: u16) -> u8 {
        #[cfg(feature = "tom_harte_test")]
        if self.is_tom_harte_test {
            return self.tom_harte_memory[address as usize];
        }

        match address {
            0x0000..=0x1FFF => self.ram[address as usize],
            0x2000..=0x2007 => match address {
                0x2000 | 0x2001 | 0x2002 => self.ppu.read_status(),
                0x2003 | 0x2004 => self.ppu.oam_read(),
                0x2005 | 0x2006 | 0x2007 => self.ppu.vram_read(),
                _ => unreachable!(),
            },
            0x2008..=0x3FFF => self.ram[(address - 0x2000) as usize],
            0x4000..=0x401F => 0x00, // TODO: I/O registers
            0x4020..=0xFFFF => {
                if let Some(cartridge) = &self.cartridge {
                    cartridge.borrow().read(address)
                } else {
                    0x00
                }
            }
        }
    }

    fn memory_read_u16(&mut self, address: u16) -> u16 {
        let lo = self.memory_read(address);
        let hi = self.memory_read(address.wrapping_add(1));
        ((hi as u16) << 8) | lo as u16
    }

    fn memory_write(&mut self, address: u16, value: u8) {
        #[cfg(feature = "tom_harte_test")]
        if self.is_tom_harte_test {
            self.tom_harte_memory[address as usize] = value;
            return;
        }

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

                match address {
                    0x2000 => {
                        self.ppu.set_control1(value);
                    }
                    0x2001 => {
                        self.ppu.set_control2(value);
                    }
                    0x2003 => {
                        self.ppu.set_oam_address(value);
                    }
                    0x2004 => {
                        self.ppu.oam_write(value);
                    }
                    0x2005 => {
                        self.ppu.set_scroll(value);
                    }
                    0x2006 => {
                        self.ppu.set_vram_address(value);
                    }
                    0x2007 => {
                        self.ppu.vram_write(value);
                    }
                    _ => unreachable!(),
                }
            }
            0x2008..=0x3FFF => self.ram[(address - 0x2008) as usize] = value,
            0x4000..=0x4013 => {} // TODO: pAPU registers
            0x4014 => {
                // OAM DMA
                self.ppu.set_oam_address(0);

                let start_address = 0x0100 * value as u16;
                let end_address = start_address + 255;

                let align = start_address & 0xFF;

                for i in start_address..=end_address {
                    let data = self.memory_read(i);
                    self.ppu.oam_write(data);
                }

                self.cycles += if align == 0 { 513 } else { 514 };
            }
            0x4015 => {}          // TODO: pAPU registers
            0x4016..=0x4017 => {} // TODO: Joypad
            0x4018..=0x401F => {} // TODO: I/O registers
            0x4020..=0xFFFF => {
                if let Some(cartridge) = &self.cartridge {
                    cartridge.borrow_mut().write(address, value);
                } else {
                    panic!("No mapper found!");
                }
            }
        }
    }

    // Stack
    fn stack_pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.memory_read(0x0100 + self.sp as u16)
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop();
        let hi = self.stack_pop();
        ((hi as u16) << 8) | lo as u16
    }

    fn stack_push(&mut self, value: u8) {
        self.memory_write(0x0100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, value: u16) {
        let hi = (value >> 8) as u8;
        self.stack_push(hi);
        let lo = value as u8;
        self.stack_push(lo);
    }

    // Debugging
    #[allow(unused_variables)]
    fn trace_opcode<S: Into<String>>(&mut self, pc_offset: u16, opcode: S, disasm: S) {
        // let opcode = opcode.into();
        // let disasm = disasm.into();
        // let ppu_scanline = self.ppu.get_scanline();
        // let ppu_cycle = self.ppu.get_current_cycle();

        // let line = format!(
        //     "{:04X}  {}{}{}{}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{}{},{}{} CYC:{}",
        //     self.pc.wrapping_sub(pc_offset),
        //     opcode,
        //     " ".repeat(10 - opcode.len()),
        //     disasm,
        //     " ".repeat(32 - disasm.len()),
        //     self.a,
        //     self.x,
        //     self.y,
        //     self.get_status_register(),
        //     self.sp,
        //     " ".repeat(3 - ppu_scanline.to_string().len()),
        //     ppu_scanline,
        //     " ".repeat(3 - ppu_cycle.to_string().len()),
        //     ppu_cycle,
        //     self.cycles,
        // );

        // log::trace!("{}", line);

        // self.trace_log.write_all(line.as_bytes()).unwrap();
        // self.trace_log.write_all(b"\n").unwrap();
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

        self.stack_push_u16(self.pc + 1);

        let p = self.get_status_register() | 0b0001_0000;
        self.stack_push(p);

        self.set_status_flag(StatusFlag::Break, true);
        self.set_status_flag(StatusFlag::Interrupt, true);

        self.cycles += 7;
    }

    fn op_ora_ind_x(&mut self) {
        // ORA X, ind - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect,X)  ORA (oper,X)  01       2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("01 {:02X}", operator),
            format!(
                "ORA (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value,
            ),
        );

        self.acc_or(value);

        self.cycles += 6;
    }

    fn op_nop_zpg(&mut self, hi_nibble: u8) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      NOP oper     04       2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("{}4 {:02X}", hi_nibble, address),
            format!("*NOP ${:02X} = {:02X}", address, value),
        );

        self.cycles += 3;
    }

    fn op_ora_zpg(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      ORA oper      05       2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("05 {:02X}", address),
            format!("ORA ${:02X} = {:02X}", address, value),
        );

        self.acc_or(value);

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

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("06 {:02X}", address),
            format!("ASL ${:02X} = {:02X}", address, value),
        );

        let result = value.wrapping_shl(1);

        self.memory_write(address as u16, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let p = self.get_status_register() | 0b0001_0000;
        self.stack_push(p);

        self.cycles += 3;
    }

    fn op_ora_imm(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // immediate     ORA #oper     09       2      2

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("09 {:02X}", value),
            format!("ORA #${:02X}", value),
        );

        self.acc_or(value);

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

        let c = self.a & 0x80 != 0;

        self.a <<= 1;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_nop_abs(&mut self) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      NOP oper     0C        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("0C {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("*NOP ${:04X} = {:02X}", address, value),
        );

        self.cycles += 4;
    }

    fn op_ora_abs(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      ORA oper      0D       3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("0D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ORA ${:04X} = {:02X}", address, value),
        );

        self.acc_or(value);

        self.cycles += 4;
    }

    fn op_asl_abs(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // M = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute      ASL oper      0E       3      6

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("0E {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ASL ${:04X} = {:02X}", address, value),
        );

        self.memory_shl(address, value);

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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("10 {:02X}", offset),
            format!("BPL ${:04X}", address),
        );

        self.branch_if(StatusFlag::Negative, false, address);

        self.cycles += 2;
    }

    fn op_ora_ind_y(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect),Y  ORA (oper),Y  11       2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("11 {:02X}", operator),
            format!(
                "ORA (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_or(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_nop_zpg_x(&mut self, hi_nibble: u8) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    NOP oper,X   14        2      4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("{:X}4 {:02X}", hi_nibble, operator),
            format!("*NOP ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.cycles += 4;
    }

    fn op_ora_zpg_x(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    ORA oper,X    15       2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("15 {:02X}", operator),
            format!("ORA ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_or(value);

        self.cycles += 4;
    }

    fn op_asl_zpg_x(&mut self) {
        // ASL - Arithmetic Shift Left (Memory)
        // M = M << 1                        N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    ASL oper,X    16       2      6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("16 {:02X}", operator),
            format!("ASL ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.memory_shl(address as u16, value);

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

        self.set_status_flag(StatusFlag::Carry, false);

        self.cycles += 2;
    }

    fn op_ora_abs_y(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,Y    ORA oper,Y    19       3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("19 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ORA ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_or(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_nop(&mut self, hi_nibble: u8) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       NOP          1A        1      2

        self.trace_opcode(1, format!("{:X}A", hi_nibble), "*NOP".to_string());

        self.cycles += 2;
    }

    fn op_nop_abs_x(&mut self, hi_nibble: u8) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    NOP oper,X   1C        3      4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!(
                "{:X}C {:02X} {:02X}",
                hi_nibble,
                operator & 0xFF,
                operator >> 8
            ),
            format!("*NOP ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_ora_abs_x(&mut self) {
        // ORA - OR Memory With ACC
        // A = A OR M                        N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,X    ORA $oper,X   1D       3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("1D {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ORA ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_or(value);

        if address & 0xFF00 != operator & 0xFF00 {
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

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("1E {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ASL ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_shl(1);

        self.memory_write(address, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let address = self.memory_read_u16(self.pc);

        self.trace_opcode(
            1,
            &format!("20 {:02X} {:02X}", address & 0xFF, address >> 8),
            &format!("JSR ${:04X}", address),
        );

        let pc = self.pc;
        self.stack_push_u16(pc.wrapping_add(1));

        self.pc = address;

        self.cycles += 6;
    }

    fn op_and_ind_x(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect,X)  AND (oper,X)  21       2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("21 {:02X}", operator),
            format!(
                "AND (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value,
            ),
        );

        self.acc_and(value);

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

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("24 {:02X}", address),
            format!("BIT ${:02X} = {:02X}", address, value),
        );

        let n = (value & 0b1000_0000) != 0;
        let v = (value & 0b0100_0000) != 0;
        let z = (value & self.a) == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Overflow, v);

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

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("25 {:02X}", address),
            format!("AND ${:02X} = {:02X}", address, value),
        );

        self.acc_and(value);

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

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("26 {:02X}", address),
            format!("ROL ${:02X} = {:02X}", address, value),
        );

        let mut result = value.wrapping_shl(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x01;
        }

        self.memory_write(address as u16, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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
        let p = p | (self.get_status_register() & 0b0011_0000);
        self.set_status_register(p);

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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("29 {:02X}", value),
            format!("AND #${:02X}", value),
        );

        self.acc_and(value);

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

        let mut result = self.a.wrapping_shl(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x01;
        }

        let c = self.a & 0x80 != 0;

        self.a = result;

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("2C {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("BIT ${:04X} = {:02X}", address, value),
        );

        let n = (value & 0b1000_0000) != 0;
        let v = (value & 0b0100_0000) != 0;
        let z = (value & self.a) == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Overflow, v);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("2D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("AND ${:04X} = {:02X}", address, value),
        );

        self.acc_and(value);

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

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("2E {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ROL ${:04X} = {:02X}", address, value),
        );

        let mut result = value.wrapping_shl(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x01;
        }

        self.memory_write(address, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("30 {:02X}", offset),
            format!("BMI ${:04X}", address),
        );

        self.branch_if(StatusFlag::Negative, true, address);

        self.cycles += 2;
    }

    fn op_and_ind_y(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect),Y  AND (oper),Y  31       2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("31 {:02X}", operator),
            format!(
                "AND (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_and(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_and_zpg_x(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    AND oper,X    35       2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("35 {:02X}", operator),
            format!("AND ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_and(value);

        self.cycles += 4;
    }

    fn op_rol_zpg_x(&mut self) {
        // ROL - Rotate Left (Memory)
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + $ - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage,X    ROL oper,X    36       2     6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("36 {:02X}", operator),
            format!("ROL ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        let mut result = value.wrapping_shl(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x01;
        }

        self.memory_write(address as u16, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        self.set_status_flag(StatusFlag::Carry, true);

        self.cycles += 2;
    }

    fn op_and_abs_y(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,Y    AND $oper,Y   39       3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("39 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("AND ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_and(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_and_abs_x(&mut self) {
        // AND - AND Memory With ACC
        // A = A AND M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,X    AND $oper,X   3D       3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("3D {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("AND ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_and(value);

        self.cycles += 4;
    }

    fn op_rol_abs_x(&mut self) {
        // ROL - Rotate Left (Memory)
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + $ - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // absolute,X    ROL oper,X    3E       3     7

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("3E {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ROL ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let mut result = value.wrapping_shl(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x01;
        }

        self.memory_write(address as u16, result);

        let c = value & 0x80 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
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
        let p = p | (self.get_status_register() & 0b0011_0000);
        self.set_status_register(p);

        let pc = self.stack_pop_u16();
        self.pc = pc;

        self.cycles += 6;
    }

    fn op_eor_ind_x(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // (indirect,X) EOR (oper,X)   41       2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("41 {:02X}", operator),
            format!(
                "EOR (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.acc_xor(value);

        self.cycles += 6;
    }

    fn op_eor_zpg(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      EOR oper      45       2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("45 {:02X}", address),
            format!("EOR ${:02X} = {:02X}", address, value),
        );

        self.acc_xor(value);

        self.cycles += 3;
    }

    fn op_lsr_zpg(&mut self) {
        // LSR - Logical Shift Right
        // 0 -> [76543210] -> C              N Z C I D V
        //                                   0 + + - - -
        //
        // addressing    assembler     op   bytes cycles
        // ---------------------------------------------
        // zeropage      LSR oper      46       2      5

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("46 {:02X}", address),
            format!("LSR ${:02X} = {:02X}", address, value),
        );

        let result = value.wrapping_shr(1);

        self.memory_write(address, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
    }

    fn op_pha(&mut self) {
        // PHA - Push ACC
        // push A                            N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PHA          48        1      3

        self.trace_opcode(1, "48", "PHA");

        self.stack_push(self.a);

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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("49 {:02X}", value),
            format!("EOR #${:02X}", value),
        );

        self.acc_xor(value);

        self.cycles += 2;
    }

    fn op_lsr_acc(&mut self) {
        // LSR - Logical Shift Right
        // A = A >> 1                        N Z C I D V
        //                                   0 + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // accumulator   LSR A        4A        1      2

        self.trace_opcode(1, "4A", "LSR A");

        let c = self.a & 0x01 != 0;

        self.a >>= 1;

        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, false);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);

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

        let address = self.memory_read_u16(self.pc);

        self.trace_opcode(
            1,
            format!("4C {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("JMP ${:04X}", address),
        );

        self.pc = address;

        self.cycles += 3;
    }

    fn op_eor_abs(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      EOR oper     4D        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("4D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("EOR ${:04X} = {:02X}", address, value),
        );

        self.acc_xor(value);

        self.cycles += 4;
    }

    fn op_lsr_abs(&mut self) {
        // LSR - Logical Shift Right
        // 0 -> [76543210] -> C              N Z C I D V
        //                                   0 + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      LSR oper     4E        3      6

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("4E {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("LSR ${:04X} = {:02X}", address, value),
        );

        let result = value.wrapping_shr(1);

        self.memory_write(address, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("50 {:02X}", offset),
            format!("BVC ${:04X}", address),
        );

        self.branch_if(StatusFlag::Overflow, false, address);

        self.cycles += 2;
    }

    fn op_eor_ind_y(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect),Y  EOR (oper),Y 51        2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("51 {:02X}", operator),
            format!(
                "EOR (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_xor(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_eor_zpg_x(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    EOR oper,X   55        2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("55 {:02X}", operator),
            format!("EOR ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_xor(value);

        self.cycles += 4;
    }

    fn op_lsr_zpg_x(&mut self) {
        // LSR - Logical Shift Right
        // 0 -> [76543210] -> C              N Z C I D V
        //                                   0 + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    LSR oper,X   56        2      6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("56 {:02X}", operator),
            format!("LSR ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_shr(1);

        self.memory_write(address as u16, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    fn op_cli(&mut self) {
        // CLI - Clear Interrupt Disable
        // I = 0                              N Z C I D V
        //                                    - - - 0 - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       CLI          58        1      2

        self.trace_opcode(1, "58", "CLI");

        self.set_status_flag(StatusFlag::Interrupt, false);

        self.cycles += 2;
    }

    fn op_eor_abs_y(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,Y    EOR oper,Y   59        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("59 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("EOR ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_xor(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_eor_abs_x(&mut self) {
        // EOR - Exclusive OR
        // A = A XOR M                       N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    EOR oper,X   5D        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("5D {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("EOR ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_xor(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_lsr_abs_x(&mut self) {
        // LSR - Logical Shift Right
        // 0 -> [76543210] -> C              N Z C I D V
        //                                   0 + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    LSR oper,X   5E        3      7

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("5E {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("LSR ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_shr(1);

        self.memory_write(address as u16, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
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
        self.pc = pc.wrapping_add(1);

        self.cycles += 6;
    }

    fn op_adc_ind_x(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect,X)  ADC (oper,X) 61        2     6*

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("61 {:02X}", operator),
            format!(
                "ADC (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.acc_add(value);

        self.cycles += 6;
    }

    fn op_adc_zpg(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      ADC oper      65        2     3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("65 {:02X}", address),
            format!("ADC ${:02X} = {:02X}", address, value),
        );

        self.acc_add(value);

        self.cycles += 3;
    }

    fn op_ror_zpg(&mut self) {
        // ROR - Rotate Right
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      ROR oper     66        2      5

        let (address, mut value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("66 {:02X}", address),
            format!("ROR ${:02X} = {:02X}", address, value),
        );

        let c = value & 0x01 != 0;

        value = value.wrapping_shr(1);

        if self.get_status_flag(StatusFlag::Carry) {
            value |= 0x80;
        }

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
    }

    fn op_pla(&mut self) {
        // PLA - Pull ACC From Stack
        // pop A                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       PLA          68        1      4

        self.trace_opcode(1, "68", "PLA");

        let value = self.stack_pop();
        self.a = value;

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("69 {:02X}", value),
            format!("ADC #${:02X}", value),
        );

        self.acc_add(value);

        self.cycles += 2;
    }

    fn op_ror_acc(&mut self) {
        // ROR - Rotate Right One Bit (ACC)
        // C -> [76543210] -> C              N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // accumulator   ROR A        6A        1      2

        self.trace_opcode(1, "6A", "ROR A");

        let a = self.a;
        let c = self.get_status_flag(StatusFlag::Carry) as u8;

        let carry = if a & 0x01 != 0 { 1 } else { 0 };
        let result = (a >> 1) | (c << 7);

        self.a = result;

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = carry != 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);

        self.cycles += 2;
    }

    fn op_jmp_ind(&mut self) {
        // JMP - Jump To New Location
        // (PC+1) -> PCL                    N Z C I D V
        // (PC+2) -> PCH                    - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // indirect      JMP (oper)   6C        3      5
        //
        // Edge case: An indirect jump must never use a
        // vector beginning on the last byte of a page.
        //
        // If address $3000 contains $40, $30FF contains
        // $80, and $3100 contains $50, the result of
        // JMP ($30FF) will be a transfer of control to
        // $4080 rather than $5080 as you intended i.e.
        // the 6502 took the low byte of the address
        // from $30FF and the high byte from $3000.

        let operator_lo = self.memory_read(self.pc);
        let operator_hi = self.memory_read(self.pc.wrapping_add(1));
        let operator = ((operator_hi as u16) << 8) | operator_lo as u16;

        let address_lo = self.memory_read(operator);
        let address_hi = if operator_lo == 0xFF {
            // Wrap around the page
            self.memory_read(operator & 0xFF00)
        } else {
            self.memory_read(operator.wrapping_add(1))
        };
        let address = ((address_hi as u16) << 8) | address_lo as u16;

        self.trace_opcode(
            1,
            format!("6C {:02X} {:02X}", operator_lo, operator_hi),
            format!("JMP (${:04X}) = {:04X}", operator, address),
        );

        self.pc = address;

        self.cycles += 5;
    }

    fn op_adc_abs(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                      N Z C I D V
        //                                    + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      ADC oper     6D        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("6D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ADC ${:04X} = {:02X}", address, value),
        );

        self.acc_add(value);

        self.cycles += 4;
    }

    fn op_ror_abs(&mut self) {
        // ROR - Rotate Right
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      ROR oper     6E        3      6

        let (address, mut value) = self.absolute();

        self.trace_opcode(
            3,
            format!("6E {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("ROR ${:04X} = {:02X}", address, value),
        );

        let c = value & 0x01 != 0;

        value >>= 1;

        if self.get_status_flag(StatusFlag::Carry) {
            value |= 0x80;
        }

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("70 {:02X}", offset),
            format!("BVS ${:04X}", address),
        );

        self.branch_if(StatusFlag::Overflow, true, address);

        self.cycles += 2;
    }

    fn op_adc_ind_y(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect),Y  ADC (oper),Y 71        2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("71 {:02X}", operator),
            format!(
                "ADC (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_add(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_adc_zpg_x(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                      N Z C I D V
        //                                    + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    ADC oper,X   75        2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("75 {:02X}", operator),
            format!("ADC ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_add(value);

        self.cycles += 4;
    }

    fn op_ror_zpg_x(&mut self) {
        // ROR - Rotate Right
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    ROR oper,X   76        2      6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("76 {:02X}", operator),
            format!("ROR ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        let mut result = value.wrapping_shr(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x80;
        }

        self.memory_write(address as u16, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        self.set_status_flag(StatusFlag::Interrupt, true);

        self.cycles += 2;
    }

    fn op_adc_abs_y(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                      N Z C I D V
        //                                    + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,Y    ADC oper,Y   79        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("79 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ADC ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_add(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_adc_abs_x(&mut self) {
        // ADC - Add Memory To ACC With Carry
        // A = A + M + C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    ADC oper,X   7D        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("7D {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ADC ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_add(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_ror_abs_x(&mut self) {
        // ROR - Rotate Right
        // C <- [76543210] <- C              N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    ROR oper,X   7E        3      7

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("7E {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("ROR ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let mut result = value.wrapping_shr(1);

        if self.get_status_flag(StatusFlag::Carry) {
            result |= 0x80;
        }

        self.memory_write(address as u16, result);

        let c = value & 0x01 != 0;
        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Carry, c);
        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
    }

    // Opcodes 80-8F
    fn op_nop_imm(&mut self) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     NOP #$oper   80        2      2

        let opcode = self.memory_read(self.pc - 1);

        let address = self.immediate();

        self.trace_opcode(
            2,
            format!("{:02X} {:02X}", opcode, address),
            format!("*NOP #${:02X}", address),
        );

        self.cycles += 2;
    }

    fn op_sta_ind_x(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect,X)  STA (oper,X) 81        2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("81 {:02X}", operator),
            format!(
                "STA (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.memory_write(address, self.a);

        self.cycles += 6;
    }

    fn op_sty_zpg(&mut self) {
        // STY - Store Index Y In Memory
        // M = Y                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      STY oper     84        2      3

        let (address, initial) = self.zeropage();

        self.trace_opcode(
            2,
            format!("84 {:02X}", address),
            format!("STY ${:02X} = {:02X}", address, initial),
        );

        self.memory_write(address as u16, self.y);

        self.cycles += 3;
    }

    fn op_sta_zpg(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      STA oper     85        2      3

        let (address, initial) = self.zeropage();

        self.trace_opcode(
            2,
            format!("85 {:02X}", address),
            format!("STA ${:02X} = {:02X}", address, initial),
        );

        self.memory_write(address as u16, self.a);

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

        let (address, initial) = self.zeropage();

        self.trace_opcode(
            2,
            format!("86 {:02X}", address),
            format!("STX ${:02X} = {:02X}", address, initial),
        );

        self.memory_write(address as u16, self.x);

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

        self.y = self.y.wrapping_sub(1);

        let n = self.y & 0x80 != 0;
        let z = self.y == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_txa(&mut self) {
        // TXA - Transfer Index X To ACC
        // A = X                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TXA          8A        1      2

        self.trace_opcode(1, "8A", "TXA");

        self.a = self.x;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_sty_abs(&mut self) {
        // STY - Store Index Y In Memory
        // M = Y                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      STY oper     8C        3      4

        let (address, initial) = self.absolute();

        self.trace_opcode(
            3,
            format!("8C {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("STY ${:04X} = {:02X}", address, initial),
        );

        self.memory_write(address, self.y);

        self.cycles += 4;
    }

    fn op_sta_abs(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      STA oper     8D        3      4

        let (address, initial) = self.absolute();

        self.trace_opcode(
            3,
            format!("8D {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("STA ${:04X} = {:02X}", address, initial),
        );

        self.memory_write(address, self.a);

        self.cycles += 4;
    }

    fn op_stx_abs(&mut self) {
        // STX - Store Index X In Memory
        // M = X                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      STX oper     8E        3      4

        let (address, initial) = self.absolute();

        self.trace_opcode(
            3,
            format!("8E {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("STX ${:04X} = {:02X}", address, initial),
        );

        self.memory_write(address, self.x);

        self.cycles += 4;
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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("90 {:02X}", offset),
            format!("BCC ${:04X}", address),
        );

        self.branch_if(StatusFlag::Carry, false, address);

        self.cycles += 2;
    }

    fn op_sta_ind_y(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // indirect,Y    STA (oper),Y 91        2      6

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("91 {:02X}", operator),
            format!(
                "STA (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.memory_write(address, self.a);

        self.cycles += 6;
    }

    fn op_sty_zpg_x(&mut self) {
        // STY - Store Index Y In Memory
        // M = Y                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    STY oper,X   94        2      4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("94 {:02X}", operator),
            format!("STY ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.memory_write(address as u16, self.y);

        self.cycles += 4;
    }

    fn op_sta_zpg_x(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    STA oper,X   95        2      4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("95 {:02X}", operator),
            format!("STA ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.memory_write(address as u16, self.a);

        self.cycles += 4;
    }

    fn op_stx_zpg_y(&mut self) {
        // STX - Store Index X In Memory
        // M = X                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,Y    STX oper,Y   96        2      4

        let (operator, address, value) = self.indexed_zeropage(self.y);

        self.trace_opcode(
            2,
            format!("96 {:02X}", operator),
            format!("STX ${:02X},Y @ {:02X} = {:02X}", operator, address, value),
        );

        self.memory_write(address as u16, self.x);

        self.cycles += 4;
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

        self.a = self.y;

        let n = self.a & 0x80 != 0;
        let z = self.a == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_sta_abs_y(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,Y    STA oper,Y   99        3      5

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("99 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("STA ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.memory_write(address, self.a);

        self.cycles += 5;
    }

    fn op_txs(&mut self) {
        // TXS - Transfer Index X To SP
        // SP = X                            N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TXS          9A        1      2

        self.trace_opcode(1, "9A", "TXS");

        self.sp = self.x;

        self.cycles += 2;
    }

    fn op_sta_abs_x(&mut self) {
        // STA - Store ACC In Memory
        // M = A                             N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    STA oper,X   9D        3      5

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("9D {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("STA ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.memory_write(address, self.a);

        self.cycles += 5;
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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("A0 {:02X}", value),
            format!("LDY #${:02X}", value),
        );

        self.y = value;

        let n = self.y & 0x80 != 0;
        let z = self.y == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_lda_ind_x(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect,X)  LDA (oper,X) A1        2     6*

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("A1 {:02X}", operator),
            format!(
                "LDA (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.acc_load(value);

        self.cycles += 6;
    }

    fn op_ldx_imm(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     LDX #oper    A2        2      2

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("A2 {:02X}", value),
            format!("LDX #${:02X}", value),
        );

        self.x_load(value);

        self.cycles += 2;
    }

    fn op_ldy_zpg(&mut self) {
        // LDY - Load Index Y With Memory
        // Y = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      LDY oper     A4        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("A4 {:02X}", address),
            format!("LDY ${:02X} = {:02X}", address, value),
        );

        self.y = value;

        let n = self.y & 0x80 != 0;
        let z = self.y == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 3;
    }

    fn op_lda_zpg(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      LDA oper     A5        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("A5 {:02X}", address),
            format!("LDA ${:02X} = {:02X}", address, value),
        );

        self.acc_load(value);

        self.cycles += 3;
    }

    fn op_ldx_zpg(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      LDX oper     A6        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("A6 {:02X}", address),
            format!("LDX ${:02X} = {:02X}", address, value),
        );

        self.x_load(value);

        self.cycles += 3;
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

        self.y = self.a;

        let n = self.y & 0x80 != 0;
        let z = self.y == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_lda_imm(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     LDA #oper    A9        2      2

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("A9 {:02X}", value),
            format!("LDA #${:02X}", value),
        );

        self.acc_load(value);

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

        self.x = self.a;

        let n = self.x & 0x80 != 0;
        let z = self.x == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_ldy_abs(&mut self) {
        // LDY - Load Index Y With Memory
        // Y = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      LDY oper     AC        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("AC {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("LDY ${:04X} = {:02X}", address, value),
        );

        self.y_load(value);

        self.cycles += 4;
    }

    fn op_lda_abs(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      LDA oper     AD        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("AD {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("LDA ${:04X} = {:02X}", address, value),
        );

        self.acc_load(value);

        self.cycles += 4;
    }

    fn op_ldx_abs(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      LDX oper     AE        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("AE {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("LDX ${:04X} = {:02X}", address, value),
        );

        self.x_load(value);

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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("B0 {:02X}", offset),
            format!("BCS ${:04X}", address),
        );

        self.branch_if(StatusFlag::Carry, true, address);

        self.cycles += 2;
    }

    fn op_lda_ind_y(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect),y  LDA (oper),Y B1        2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("B1 {:02X}", operator),
            format!(
                "LDA (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_load(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_ldy_zpg_x(&mut self) {
        // LDY - Load Index Y With Memory
        // Y = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,x    LDY oper,X   B4        2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("B4 {:02X}", operator),
            format!("LDY ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.y_load(value);

        self.cycles += 4;
    }

    fn op_lda_zpg_x(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,x    LDA oper,X   B5        2      4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("B5 {:02X}", operator),
            format!("LDA ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_load(value);

        self.cycles += 4;
    }

    fn op_ldx_zpg_y(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,y    LDX oper,Y   B6        2     4

        let (operator, address, value) = self.indexed_zeropage(self.y);

        self.trace_opcode(
            2,
            format!("B6 {:02X}", operator),
            format!("LDX ${:02X},Y @ {:02X} = {:02X}", operator, address, value),
        );

        self.x_load(value);

        self.cycles += 4;
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

        self.set_status_flag(StatusFlag::Overflow, false);

        self.cycles += 2;
    }

    fn op_lda_abs_y(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,y    LDA oper,Y   B9        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("B9 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("LDA ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_load(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_tsx(&mut self) {
        // TSX - Transfer SP To Index X
        // X = S                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       TSX          BA        1      2

        self.trace_opcode(1, "BA", "TSX");

        self.x = self.sp;

        let n = self.x & 0x80 != 0;
        let z = self.x == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_ldy_abs_x(&mut self) {
        // LDY - Load Index Y With Memory
        // Y = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,x    LDY oper,X   BC        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("BC {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("LDY ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.y_load(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_lda_abs_x(&mut self) {
        // LDA - Load ACC With Memory
        // A = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,x    LDA oper,X   BD        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("BD {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("LDA ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_load(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_ldx_abs_y(&mut self) {
        // LDX - Load Index X With Memory
        // X = M                             N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,y    LDX oper,Y   BE        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("BE {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("LDX ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.x_load(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("C0 {:02X}", value),
            format!("CPY #${:02X}", value),
        );

        let y = self.y;
        let result = y.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = y >= value;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);

        self.cycles += 2;
    }

    fn op_cmp_ind_x(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect,X)  CMP (oper,X) C1        2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("C1 {:02X}", operator),
            format!(
                "CMP (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.acc_compare(value);

        self.cycles += 6;
    }

    fn op_cpy_zpg(&mut self) {
        // CPY - Compare Memory And Index Y
        // Y - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      CPY oper     C4        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("C4 {:02X}", address),
            format!("CPY ${:02X} = {:02X}", address, value),
        );

        self.index_compare(self.y, value);

        self.cycles += 3;
    }

    fn op_cmp_zpg(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      CMP oper     C5        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("C5 {:02X}", address),
            format!("CMP ${:02X} = {:02X}", address, value),
        );

        self.acc_compare(value);

        self.cycles += 3;
    }

    fn op_dec_zpg(&mut self) {
        // DEC - Decrement Memory By One
        // M = M - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      DEC oper     C6        2      5

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("C6 {:02X}", address),
            format!("DEC ${:02X} = {:02X}", address, value),
        );

        let result = value.wrapping_sub(1);

        self.memory_write(address, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
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

        self.y = self.y.wrapping_add(1);

        let n = self.y & 0x80 != 0;
        let z = self.y == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_cmp_imm(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // immediate     CMP #oper    C9        2      2

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("C9 {:02X}", value),
            format!("CMP #${:02X}", value),
        );

        let a = self.a;
        let result = a.wrapping_sub(value);

        let n = result & 0x80 != 0;
        let z = result == 0;
        let c = a >= value;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);
        self.set_status_flag(StatusFlag::Carry, c);

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

        self.x = self.x.wrapping_sub(1);

        let n = self.x & 0x80 != 0;
        let z = self.x == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 2;
    }

    fn op_cpy_abs(&mut self) {
        // CPY - Compare Memory And Index Y
        // Y - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      CPY oper     CC        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("CC {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("CPY ${:04X} = {:02X}", address, value),
        );

        self.index_compare(self.y, value);

        self.cycles += 4;
    }

    fn op_cmp_abs(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      CMP oper     CD        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("CD {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("CMP ${:04X} = {:02X}", address, value),
        );

        self.acc_compare(value);

        self.cycles += 4;
    }

    fn op_dec_abs(&mut self) {
        // DEC - Decrement Memory By One
        // M = M - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      DEC oper     CE        3      6

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("CE {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("DEC ${:04X} = {:02X}", address, value),
        );

        let result = value.wrapping_sub(1);

        self.memory_write(address, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
    }

    // Opcodes D0-DF
    fn op_bne(&mut self) {
        // BNE - Branch If Not Equal
        // branch if Z eq 0                  N Z C I D V
        //                                   - - - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // relative      BNE oper     D0        2    2**

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("D0 {:02X}", offset),
            format!("BNE ${:04X}", address),
        );

        self.branch_if(StatusFlag::Zero, false, address);

        self.cycles += 2;
    }

    fn op_cmp_ind_y(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // ind, Y        CMP (oper),Y D1        2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("D1 {:02X}", operator),
            format!(
                "CMP (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_compare(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_cmp_zpg_x(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    CMP oper,X   D5        2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("D5 {:02X}", operator),
            format!("CMP ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_compare(value);

        self.cycles += 4;
    }

    fn op_dec_zpg_x(&mut self) {
        // DEC - Decrement Memory By One
        // M = M - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    DEC oper,X   D6        2     6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("D6 {:02X}", operator),
            format!("DEC ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_sub(1);

        self.memory_write(address as u16, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        self.set_status_flag(StatusFlag::Decimal, false);

        self.cycles += 2;
    }

    fn op_cmp_abs_y(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,Y    CMP oper,Y   D9        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("D9 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("CMP ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_compare(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_cmp_abs_x(&mut self) {
        // CMP - Compare Memory With ACC
        // A - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    CMP oper,X   DD        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("DD {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("CMP ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_compare(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_dec_abs_x(&mut self) {
        // DEC - Decrement Memory By One
        // M = M - 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    DEC oper,X   DE        3      7

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("DE {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("DEC ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_sub(1);

        self.memory_write(address as u16, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("E0 {:02X}", value),
            format!("CPX #${:02X}", value),
        );

        self.index_compare(self.x, value);

        self.cycles += 2;
    }

    fn op_sbc_ind_x(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A = A - M - C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect,X)  SBC (oper,X) E1        2      6

        let (operator, indirect, address, value) = self.pre_indexed_indirect();

        self.trace_opcode(
            2,
            format!("E1 {:02X}", operator),
            format!(
                "SBC (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                operator, indirect, address, value
            ),
        );

        self.acc_subtract(value);

        self.cycles += 6;
    }

    fn op_cpx_zpg(&mut self) {
        // CPX - Compare Memory And Index X
        // X - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      CPX oper     E4        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("E4 {:02X}", address),
            format!("CPX ${:02X} = {:02X}", address, value),
        );

        self.index_compare(self.x, value);

        self.cycles += 3;
    }

    fn op_sbc_zpg(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A = A - M - C                     N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      SBC oper     E5        2      3

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("E5 {:02X}", address),
            format!("SBC ${:02X} = {:02X}", address, value),
        );

        self.acc_subtract(value);

        self.cycles += 3;
    }

    fn op_inc_zpg(&mut self) {
        // INC - Increment Memory By One
        // M = M + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage      INC oper     E6        2      5

        let (address, value) = self.zeropage();

        self.trace_opcode(
            2,
            format!("E6 {:02X}", address),
            format!("INC ${:02X} = {:02X}", address, value),
        );

        let value = value.wrapping_add(1);

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 5;
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

        self.x = self.x.wrapping_add(1);

        let n = self.x & 0x80 != 0;
        let z = self.x == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

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

        let value = self.immediate();

        self.trace_opcode(
            2,
            format!("E9 {:02X}", value),
            format!("SBC #${:02X}", value),
        );

        self.acc_subtract(value);

        self.cycles += 2;
    }

    fn op_nop_official(&mut self) {
        // NOP - No Operation
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // implied       NOP          EA        1      2

        self.trace_opcode(1, "EA", "NOP");

        self.cycles += 2;
    }

    fn op_cpx_abs(&mut self) {
        // CPX - Compare Memory And Index X
        // X - M                             N Z C I D V
        //                                   + + + - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      CPX oper     EC        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("EC {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("CPX ${:04X} = {:02X}", address, value),
        );

        self.index_compare(self.x, value);

        self.cycles += 4;
    }

    fn op_sbc_abs(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      SBC oper     ED        3      4

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("ED {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("SBC ${:04X} = {:02X}", address, value),
        );

        self.acc_subtract(value);

        self.cycles += 4;
    }

    fn op_inc_abs(&mut self) {
        // INC - Increment Memory By One
        // M = M + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute      INC oper     EE        3      6

        let (address, value) = self.absolute();

        self.trace_opcode(
            3,
            format!("EE {:02X} {:02X}", address & 0xFF, address >> 8),
            format!("INC ${:04X} = {:02X}", address, value),
        );

        let value = value.wrapping_add(1);

        self.memory_write(address, value);

        let n = value & 0x80 != 0;
        let z = value == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        let (offset, address) = self.relative();

        self.trace_opcode(
            2,
            format!("F0 {:02X}", offset),
            format!("BEQ ${:04X}", address),
        );

        self.branch_if(StatusFlag::Zero, true, address);

        self.cycles += 2;
    }

    fn op_sbc_ind_y(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // (indirect),Y  SBC (oper),Y F1        2     5*

        let (operator, base, address, value) = self.post_indexed_indirect();

        self.trace_opcode(
            2,
            format!("F1 {:02X}", operator),
            format!(
                "SBC (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                operator, base, address, value
            ),
        );

        self.acc_subtract(value);

        if address & 0xFF00 != base & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 5;
    }

    fn op_sbc_zpg_x(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    SBC oper,X   F5        2     4

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("F5 {:02X}", operator),
            format!("SBC ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        self.acc_subtract(value);

        self.cycles += 4;
    }

    fn op_inc_zpg_x(&mut self) {
        // INC - Increment Memory By One
        // M = M + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // zeropage,X    INC oper,X   F6        2     6

        let (operator, address, value) = self.indexed_zeropage(self.x);

        self.trace_opcode(
            2,
            format!("F6 {:02X}", operator),
            format!("INC ${:02X},X @ {:02X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_add(1);

        self.memory_write(address as u16, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 6;
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

        self.set_status_flag(StatusFlag::Decimal, true);

        self.cycles += 2;
    }

    fn op_sbc_abs_y(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,Y    SBC oper,Y   F9        3     4*

        let (operator, address, value) = self.indexed_absolute(self.y);

        self.trace_opcode(
            3,
            format!("F9 {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("SBC ${:04X},Y @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_subtract(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_sbc_abs_x(&mut self) {
        // SBC - Subtract Memory From ACC With Borrow
        // A - M - C                         N Z C I D V
        //                                   + + + - - +
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    SBC oper,X   FD        3     4*

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("FD {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("SBC ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        self.acc_subtract(value);

        if address & 0xFF00 != operator & 0xFF00 {
            self.cycles += 1;
        }

        self.cycles += 4;
    }

    fn op_inc_abs_x(&mut self) {
        // INC - Increment Memory By One
        // M = M + 1                         N Z C I D V
        //                                   + + - - - -
        //
        // addressing    assembler    op    bytes cycles
        // ---------------------------------------------
        // absolute,X    INC oper,X   FE        3     7

        let (operator, address, value) = self.indexed_absolute(self.x);

        self.trace_opcode(
            3,
            format!("FE {:02X} {:02X}", operator & 0xFF, operator >> 8),
            format!("INC ${:04X},X @ {:04X} = {:02X}", operator, address, value),
        );

        let result = value.wrapping_add(1);

        self.memory_write(address as u16, result);

        let n = result & 0x80 != 0;
        let z = result == 0;

        self.set_status_flag(StatusFlag::Negative, n);
        self.set_status_flag(StatusFlag::Zero, z);

        self.cycles += 7;
    }
}

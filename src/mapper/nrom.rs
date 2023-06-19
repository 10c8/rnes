use log::trace;

use crate::cartridge::Cartridge;

use super::Mapper;

pub struct NROMMapper {
    rom: Vec<u8>,
    rom_bank_count: u8,
}

impl NROMMapper {
    pub fn new(cart: &Cartridge) -> Self {
        let mut rom = cart.get_prg_rom().to_vec();
        let chr_data = cart.get_chr_rom().to_vec();
        rom.extend(chr_data.to_vec());

        let rom_bank_count = cart.rom_bank_count;

        Self {
            rom,
            rom_bank_count,
        }
    }
}

impl Mapper for NROMMapper {
    fn get_name(&self) -> &'static str {
        "NROM"
    }

    fn read(&self, address: u16) -> u8 {
        match address {
            0x4020..=0x5FFF => panic!("NROM mapper has no EROM: {:04X}", address),
            0x6000..=0x7FFF => panic!("NROM mapper has no SRAM: {:04X}", address),
            0x8000..=0xBFFF => self.rom[address as usize - 0x8000],
            0xC000..=0xFFFF => {
                if self.rom_bank_count == 1 {
                    self.rom[address as usize - 0xC000]
                } else {
                    self.rom[address as usize - 0x8000]
                }
            }
            _ => panic!("Invalid mapper read address: {:04X}", address),
        }
    }

    fn write(&mut self, address: u16, _value: u8) {
        match address {
            0x6000..=0x7FFF => panic!("NROM mapper has no SRAM: {:04X}", address),
            _ => panic!("Invalid mapper write address: {:04X}", address),
        }
    }
}

use log::debug;

use super::Mapper;

pub struct NROMMapper {
    rom: Vec<u8>,
    rom_bank_count: u8,
    vrom: Vec<u8>,
}

impl NROMMapper {
    pub fn new(data: &Vec<u8>, rom_bank_count: u8, vrom_bank_count: u8) -> Self {
        let prg_length = rom_bank_count as usize * 0x4000 + 0x10;
        let rom = data[0x10..prg_length - 1].to_vec();

        debug!("PRG-ROM length: {} bytes", prg_length);

        let vrom = if vrom_bank_count > 0 {
            let chr_start = rom_bank_count as usize * 0x4000 + 0x10;
            debug!("CHR-ROM length: {} bytes", data.len() - chr_start);

            data[chr_start..chr_start + 0x2000].to_vec()
        } else {
            debug!("No CHR-ROM found");
            vec![0; 0x2000]
        };

        Self {
            rom,
            rom_bank_count,
            vrom,
        }
    }
}

impl Mapper for NROMMapper {
    fn get_name(&self) -> &'static str {
        "NROM"
    }

    fn get_vrom(&self) -> &Vec<u8> {
        &self.vrom
    }

    fn get_vrom_bank_count(&self) -> usize {
        1
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

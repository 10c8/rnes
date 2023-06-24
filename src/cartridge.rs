use std::{fs::File, io::Read};

use log::debug;

use crate::mapper::{nrom, Mapper};

const MAGIC_ADDR: u8 = 0x00;
const MAGIC_LEN: usize = 4;
const MAGIC_BYTES: [u8; MAGIC_LEN] = [0x4E, 0x45, 0x53, 0x1A];
const ROM_BANK_COUNT_ADDR: u8 = 0x04;
const VROM_BANK_COUNT_ADDR: u8 = 0x05;
const ROM_CTRL1_ADDR: u8 = 0x06;
const ROM_CTRL2_ADDR: u8 = 0x07;
const RAM_BANK_COUNT_ADDR: u8 = 0x08;

#[derive(Debug)]
pub enum MirrorMode {
    Horizontal,
    Vertical,
    FourScreen,
}

pub struct Cartridge {
    pub is_magic_valid: bool,
    pub magic_bytes: Vec<u8>,
    pub rom_bank_count: u8,
    pub vrom_bank_count: u8,
    pub mirror_mode: MirrorMode,
    pub has_battery_backed_ram: bool,
    pub has_trainer: bool,
    pub mapper_number: u8,
    pub ram_bank_count: u8,
    pub mapper: Box<dyn Mapper>,
}

impl Cartridge {
    pub fn from_rom_file(filename: &str) -> Self {
        // Read data
        let mut f = File::open(&filename).expect("Unable to open ROM file");
        let mut data = Vec::new();
        f.read_to_end(&mut data).expect("Unable to read ROM file");

        debug!("Loaded {} bytes from ROM file", data.len());

        // Parse header
        let magic_bytes = &data[MAGIC_ADDR as usize..MAGIC_ADDR as usize + MAGIC_LEN as usize];
        let is_magic_valid = magic_bytes == &MAGIC_BYTES;
        let rom_bank_count = data[ROM_BANK_COUNT_ADDR as usize];
        let vrom_bank_count = data[VROM_BANK_COUNT_ADDR as usize];

        let rom_ctrl1 = data[ROM_CTRL1_ADDR as usize];
        let mut mirror_mode = match rom_ctrl1 & 0b0000_1000 {
            0b0000_0000 => MirrorMode::Horizontal,
            0b0000_1000 => MirrorMode::Vertical,
            _ => panic!("Invalid mirror mode"),
        };
        let has_battery_backed_ram = rom_ctrl1 & 0b0000_0010 != 0;
        let has_trainer = rom_ctrl1 & 0b0000_0001 != 0;
        if rom_ctrl1 & 0b0000_1000 != 0 {
            mirror_mode = MirrorMode::FourScreen;
        }
        let mapper_number_lo = rom_ctrl1 & 0b1111_0000;

        let rom_ctrl2 = data[ROM_CTRL2_ADDR as usize];
        let mapper_number_hi = rom_ctrl2 & 0b1111_0000;
        let ram_bank_count = data[RAM_BANK_COUNT_ADDR as usize];

        let mapper_number = (mapper_number_lo >> 4) | mapper_number_hi;

        debug!("Mapper number: {}", mapper_number);
        debug!("Mirror mode: {:?}", mirror_mode);
        debug!("ROM banks: {}", rom_bank_count);
        debug!("VROM banks: {}", vrom_bank_count);
        debug!("RAM banks: {}", ram_bank_count);
        debug!(
            "Battery-backed RAM: {}",
            if has_battery_backed_ram { "Yes" } else { "No" }
        );

        let mapper: Box<dyn Mapper> = match mapper_number {
            0 => Box::new(nrom::NROMMapper::new(
                &data,
                rom_bank_count,
                vrom_bank_count,
            )),
            _ => panic!("Unsupported mapper type: {}", mapper_number),
        };

        Self {
            is_magic_valid,
            magic_bytes: magic_bytes.to_vec(),
            rom_bank_count,
            vrom_bank_count,
            mirror_mode,
            has_battery_backed_ram,
            has_trainer,
            mapper_number,
            ram_bank_count,
            mapper,
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        self.mapper.read(address)
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.mapper.write(address, value);
    }
}

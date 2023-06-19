mod bandai;
mod nrom;

use crate::cartridge::Cartridge;

pub trait Mapper {
    fn get_name(&self) -> &'static str;
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, data: u8);
}

pub fn from_cartridge(cart: &Cartridge) -> Box<dyn Mapper> {
    match cart.mapper_number {
        0 => Box::new(nrom::NROMMapper::new(cart)),
        16 => Box::new(bandai::BandaiMapper::new(cart)),
        _ => panic!("Unsupported mapper type: {}", cart.mapper_number),
    }
}

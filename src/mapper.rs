pub mod nrom;

pub trait Mapper {
    fn get_name(&self) -> &'static str;
    fn get_vrom_bank_count(&self) -> usize;
    fn get_vrom(&self) -> &Vec<u8>;
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, data: u8);
}

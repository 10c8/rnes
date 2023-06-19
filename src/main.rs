mod cartridge;
mod cpu;
mod mapper;
mod memory;

use log::info;

use cartridge::Cartridge;
use cpu::CPU;

fn main() {
    pretty_env_logger::init();

    info!("Initializing CPU...");
    let mut cpu = CPU::new();

    info!("Loading cartridge from ROM file...");
    let cart = Cartridge::from_rom_file("resources/roms/nestest.nes");
    let mapper = mapper::from_cartridge(&cart);
    cpu.set_mapper(mapper);

    info!("Starting...");
    cpu.reset();
    cpu.run();
}

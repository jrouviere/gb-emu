use std::env;
use std::fs;

use emu;

fn main() {
    let filename = env::args()
        .nth(1)
        .expect("Provide rom filename as an argument");

    let rom_data = fs::read(filename).expect("Cannot read file");

    let bus = emu::build_bus(rom_data);
    let mut cpu = emu::build_cpu(bus);

    cpu.run();
}

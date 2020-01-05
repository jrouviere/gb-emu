use std::fmt;

#[derive(Copy, Clone, Debug)]
enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}
impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Copy, Clone, Debug)]
enum Reg16 {
    HL,
    BC,
    DE,
    AF,
    SP,
    PC,
}
impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// all 8 bit parameters
#[derive(Copy, Clone, Debug)]
enum Par8 {
    // 8-bit registers
    R8(Reg8),
    //(HL) address pointed by HL
    AHL,
    //(a16) address pointed by value
    A16(u16),
    //nn direct value
    D8(u8),
}

impl fmt::Display for Par8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Par8::R8(r) => write!(f, "{}", r),
            Par8::AHL => write!(f, "(HL)"),
            Par8::A16(n) => write!(f, "({})", n),
            Par8::D8(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Cond {
    C,
    NC,
    Z,
    NZ,
}
impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    ADD(Par8),
    ADDr16(Reg16),
    ADDr16n8(Reg16, u8),
    ADC(Par8),
    SBC(Par8),
    CP(Par8),
    SUB(Par8),
    AND(Par8),
    OR(Par8),
    XOR(Par8),

    INC8(Reg8),
    INC16(Reg16),
    DEC8(Reg8),
    DEC16(Reg16),

    LD(Reg8, Reg8),
    LDrn(Reg8, u8),
    LDr8r16(Reg8, Reg16),
    LDr16r8(Reg16, Reg8),
    LDHLn(u8),
    LDr8n16(Reg8, u16),
    LDn16r8(u16, Reg8),
    LDr16n16(Reg16, u16),

    JPn16(u16),
    JRn8(u8),
    JPCn16(Cond, u16),
    JRCn8(Cond, u8),

    PUSH(Reg16),
    POP(Reg16),

    CALL(u16),
    CALLC(Cond, u16),

    RET(),
    RETC(Cond),

    Nop(),
    Unimplemented(u8),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::ADD(p) => write!(f, "ADD A,{}", p),
            Instruction::ADDr16(r) => write!(f, "ADD HL,{}", r),
            Instruction::ADDr16n8(r16, n8) => write!(f, "ADD {},{}", r16, n8),
            Instruction::INC8(r) => write!(f, "INC {}", r),
            Instruction::INC16(r) => write!(f, "INC {}", r),
            Instruction::DEC8(r) => write!(f, "DEC {}", r),
            Instruction::DEC16(r) => write!(f, "DEC {}", r),
            Instruction::ADC(r) => write!(f, "ADC {}", r),
            Instruction::SBC(r) => write!(f, "SBC {}", r),
            Instruction::CP(r) => write!(f, "CP {}", r),
            Instruction::SUB(r) => write!(f, "SUB {}", r),
            Instruction::AND(r) => write!(f, "AND {}", r),
            Instruction::OR(r) => write!(f, "OR {}", r),
            Instruction::XOR(r) => write!(f, "XOR {}", r),
            Instruction::LD(r1, r2) => write!(f, "LD {},{}", r1, r2),
            Instruction::LDrn(r, n) => write!(f, "LD {},{}", r, n),
            Instruction::LDr8r16(r8, r16) => write!(f, "LD {},({})", r8, r16),
            Instruction::LDr16r8(r16, r8) => write!(f, "LD ({}),{}", r16, r8),
            Instruction::LDHLn(n) => write!(f, "LD (HL),{}", n),
            Instruction::LDr8n16(r8, n16) => write!(f, "LD {},({})", r8, n16),
            Instruction::LDn16r8(n16, r8) => write!(f, "LD ({}),{}", n16, r8),
            Instruction::LDr16n16(r16, n16) => write!(f, "LD {},{}", r16, n16),
            Instruction::JPn16(n16) => write!(f, "JP {}", n16),
            Instruction::JRn8(n8) => write!(f, "JR {}", n8),
            Instruction::JPCn16(c, n16) => write!(f, "JP {},{}", c, n16),
            Instruction::JRCn8(c, n8) => write!(f, "JR {},{}", c, n8),
            Instruction::PUSH(r) => write!(f, "PUSH {}", r),
            Instruction::POP(r) => write!(f, "POP {}", r),
            Instruction::CALL(n16) => write!(f, "CALL {}", n16),
            Instruction::CALLC(c, n16) => write!(f, "CALL {},{}", c, n16),
            Instruction::RET() => write!(f, "RET"),
            Instruction::RETC(c) => write!(f, "RET {}", c),
            Instruction::Nop() => write!(f, "NOP"),
            Instruction::Unimplemented(op) => write!(f, "unimplemented {}", op),
        }
    }
}

pub struct Bus {
    // 0x0000-0x3FFF: bank 0
    // 0x4000-0x7FFF: bank N
    rom_data: Vec<u8>,

    //0x8000-0x97FF: character map
    char_ram: [u8; 0x1800],

    //0x9800-0x9BFF: background map 1
    bg_map1: [u8; 0x400],
    //0x9C00-0x9FFF: background map 2
    bg_map2: [u8; 0x400],

    // 0xA000-0xBFFF: external/cartdridge ram
    ext_ram: [u8; 0x2000],
    // 0xC000-0xDFFF:
    int_ram: [u8; 0x2000],
    // 0xE000-0xFDFF: Reserved
    // -
    // 0xFE00-0xFE9F: Object Attribute Memory (OAM)
    oam: [u8; 0xA0],
    // 0xFEA0-0xFEFF - Reserved
    // -
    // 0xFF00-0xFF7F - I/O
    // -
    // 0xFF80-0xFFFE - High RAM Area
    high_ram: [u8; 0x7F],
    // 0xFFFF Interrupt Enable
    // -
}

pub fn build_bus(rom_data: Vec<u8>) -> Bus {
    assert!(rom_data.len() >= 0x8000, "rom data incomplete");
    return Bus {
        rom_data: rom_data,
        char_ram: [0; 0x1800],
        bg_map1: [0; 0x400],
        bg_map2: [0; 0x400],
        ext_ram: [0; 0x2000],
        int_ram: [0; 0x2000],
        oam: [0; 0xA0],
        high_ram: [0; 0x7F],
    };
}

impl Bus {
    fn load8(&self, addr: u16) -> u8 {
        return match addr {
            0x0000..=0x3FFF => self.rom_data[addr as usize],
            0x4000..=0x7FFF => self.rom_data[addr as usize], //TODO: handle memory bank
            0x8000..=0x97FF => self.char_ram[(addr - 0x8000) as usize],
            0x9800..=0x9BFF => self.bg_map1[(addr - 0x9800) as usize],
            0x9C00..=0x9FFF => self.bg_map2[(addr - 0x9C00) as usize],
            0xA000..=0xBFFF => self.ext_ram[(addr - 0xA000) as usize],
            0xC000..=0xDFFF => self.int_ram[(addr - 0xC000) as usize],
            0xE000..=0xFDFF => self.int_ram[(addr - 0xE000) as usize], // mirror of int ram
            0xFE00..=0xFE9F => self.oam[(addr - 0xFE00) as usize],
            0xFEA0..=0xFEFF => 0, //reserved,
            0xFF00..=0xFF7F => 0, //TODO: I/O Registers
            0xFF80..=0xFFFE => self.high_ram[(addr - 0xFF80) as usize],
            0xFFFF => 0, //TODO: interrupt enable
        };
    }
    fn store8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x3FFF => (),
            0x4000..=0x7FFF => (),
            0x8000..=0x97FF => self.char_ram[(addr - 0x8000) as usize] = val,
            0x9800..=0x9BFF => self.bg_map1[(addr - 0x9800) as usize] = val,
            0x9C00..=0x9FFF => self.bg_map2[(addr - 0x9C00) as usize] = val,
            0xA000..=0xBFFF => self.ext_ram[(addr - 0xA000) as usize] = val,
            0xC000..=0xDFFF => self.int_ram[(addr - 0xC000) as usize] = val,
            0xE000..=0xFDFF => self.int_ram[(addr - 0xE000) as usize] = val, // mirror of int ram
            0xFE00..=0xFE9F => self.oam[(addr - 0xFE00) as usize] = val,
            0xFEA0..=0xFEFF => (), //reserved
            0xFF00..=0xFF7F => (), //TODO: I/O Registers
            0xFF80..=0xFFFE => self.high_ram[(addr - 0xFF80) as usize] = val,
            0xFFFF => (), //TODO: interrupt enable
        }
    }
}

pub struct Cpu {
    bus: Bus,

    reg_pc: u16,
    reg_sp: u16,
    reg_a: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_f: u8,
    reg_h: u8,
    reg_l: u8,
}

impl Cpu {
    fn reg16(&self, r: Reg16) -> u16 {
        match r {
            Reg16::HL => return (self.reg_h as u16) << 8 | (self.reg_l as u16),
            Reg16::BC => return (self.reg_b as u16) << 8 | (self.reg_c as u16),
            Reg16::DE => return (self.reg_d as u16) << 8 | (self.reg_e as u16),
            Reg16::AF => return (self.reg_a as u16) << 8 | (self.reg_f as u16),
            Reg16::SP => return self.reg_sp,
            Reg16::PC => return self.reg_pc,
        }
    }
    fn set_reg16(&mut self, r: Reg16, val: u16) {
        match r {
            Reg16::HL => {
                self.reg_h = (val >> 8) as u8;
                self.reg_l = (val & 0xFF) as u8;
            }
            Reg16::BC => {
                self.reg_b = (val >> 8) as u8;
                self.reg_c = (val & 0xFF) as u8;
            }
            Reg16::DE => {
                self.reg_d = (val >> 8) as u8;
                self.reg_e = (val & 0xFF) as u8;
            }
            Reg16::AF => {
                self.reg_a = (val >> 8) as u8;
                self.reg_f = (val & 0xFF) as u8;
            }
            Reg16::SP => self.reg_sp = val,
            Reg16::PC => self.reg_pc = val,
        }
    }

    fn par8(&self, p: Par8) -> u8 {
        match p {
            Par8::R8(r) => self.reg8(r),
            Par8::AHL => self.bus.load8(self.reg16(Reg16::HL)),
            Par8::A16(a) => self.bus.load8(a),
            Par8::D8(d) => d,
        }
    }

    fn reg8(&self, r: Reg8) -> u8 {
        match r {
            Reg8::A => return self.reg_a,
            Reg8::B => return self.reg_b,
            Reg8::C => return self.reg_c,
            Reg8::D => return self.reg_d,
            Reg8::E => return self.reg_e,
            Reg8::F => return self.reg_f,
            Reg8::H => return self.reg_h,
            Reg8::L => return self.reg_l,
        }
    }

    fn set_reg8(&mut self, r: Reg8, val: u8) {
        match r {
            Reg8::A => self.reg_a = val,
            Reg8::B => self.reg_b = val,
            Reg8::C => self.reg_c = val,
            Reg8::D => self.reg_d = val,
            Reg8::E => self.reg_e = val,
            Reg8::F => self.reg_f = val,
            Reg8::H => self.reg_h = val,
            Reg8::L => self.reg_l = val,
        }
    }

    fn set_flags(&mut self, zero: bool, subtract: bool, carry: bool, half_carry: bool) {
        self.reg_f = if zero { 0x80 } else { 0x00 }
            | if subtract { 0x40 } else { 0x00 }
            | if half_carry { 0x20 } else { 0x00 }
            | if carry { 0x10 } else { 0x00 };
    }

    fn load8(&self, addr: u16) -> u8 {
        self.bus.load8(addr)
    }
    fn store8(&mut self, addr: u16, val: u8) {
        self.bus.store8(addr, val)
    }

    fn load16(&self, addr: u16) -> u16 {
        let lsb = self.bus.load8(addr) as u16;
        let msb = self.bus.load8(addr + 1) as u16;
        msb << 8 | lsb
    }

    fn store16(&mut self, addr: u16, val: u16) {
        let lsb = val & 0xFF;
        let msb = (val & 0xFF00) >> 8;

        self.bus.store8(addr, lsb as u8);
        self.bus.store8(addr + 1, msb as u8);
    }

    fn load_pc_inc(&mut self) -> u8 {
        let data = self.load8(self.reg_pc);
        self.reg_pc = self.reg_pc.wrapping_add(1);
        data
    }

    fn load_pc_n16(&mut self) -> u16 {
        let lsb = self.load_pc_inc() as u16;
        let msb = self.load_pc_inc() as u16;
        msb << 8 | lsb
    }

    fn load_instruction(&mut self) -> Instruction {
        let opcode = self.load_pc_inc();
        return match opcode {
            0x00 => Instruction::Nop(),

            0x03 => Instruction::INC16(Reg16::BC),
            0x13 => Instruction::INC16(Reg16::DE),
            0x23 => Instruction::INC16(Reg16::HL),
            0x33 => Instruction::INC16(Reg16::SP),
            0x04 => Instruction::INC8(Reg8::B),
            0x14 => Instruction::INC8(Reg8::D),
            0x24 => Instruction::INC8(Reg8::H),
            0x05 => Instruction::DEC8(Reg8::B),
            0x15 => Instruction::DEC8(Reg8::D),
            0x25 => Instruction::DEC8(Reg8::H),
            0x0C => Instruction::INC8(Reg8::C),
            0x1C => Instruction::INC8(Reg8::E),
            0x2C => Instruction::INC8(Reg8::L),
            0x3C => Instruction::INC8(Reg8::A),
            0x0D => Instruction::DEC8(Reg8::C),
            0x1D => Instruction::DEC8(Reg8::E),
            0x2D => Instruction::DEC8(Reg8::L),
            0x3D => Instruction::DEC8(Reg8::A),

            0x0B => Instruction::DEC16(Reg16::BC),
            0x1B => Instruction::DEC16(Reg16::DE),
            0x2B => Instruction::DEC16(Reg16::HL),
            0x3B => Instruction::DEC16(Reg16::SP),

            0x80 => Instruction::ADD(Par8::R8(Reg8::B)),
            0x81 => Instruction::ADD(Par8::R8(Reg8::C)),
            0x82 => Instruction::ADD(Par8::R8(Reg8::D)),
            0x83 => Instruction::ADD(Par8::R8(Reg8::E)),
            0x84 => Instruction::ADD(Par8::R8(Reg8::H)),
            0x85 => Instruction::ADD(Par8::R8(Reg8::L)),
            0x86 => Instruction::ADD(Par8::AHL),
            0x87 => Instruction::ADD(Par8::R8(Reg8::A)),
            0xC6 => Instruction::ADD(Par8::D8(self.load_pc_inc())),

            0x90 => Instruction::SUB(Par8::R8(Reg8::B)),
            0x91 => Instruction::SUB(Par8::R8(Reg8::C)),
            0x92 => Instruction::SUB(Par8::R8(Reg8::D)),
            0x93 => Instruction::SUB(Par8::R8(Reg8::E)),
            0x94 => Instruction::SUB(Par8::R8(Reg8::H)),
            0x95 => Instruction::SUB(Par8::R8(Reg8::L)),
            0x96 => Instruction::SUB(Par8::AHL),
            0x97 => Instruction::SUB(Par8::R8(Reg8::A)),
            0xD6 => Instruction::SUB(Par8::D8(self.load_pc_inc())),

            0xA0 => Instruction::AND(Par8::R8(Reg8::B)),
            0xA1 => Instruction::AND(Par8::R8(Reg8::C)),
            0xA2 => Instruction::AND(Par8::R8(Reg8::D)),
            0xA3 => Instruction::AND(Par8::R8(Reg8::E)),
            0xA4 => Instruction::AND(Par8::R8(Reg8::H)),
            0xA5 => Instruction::AND(Par8::R8(Reg8::L)),
            0xA6 => Instruction::AND(Par8::AHL),
            0xA7 => Instruction::AND(Par8::R8(Reg8::A)),
            0xE6 => Instruction::AND(Par8::D8(self.load_pc_inc())),

            0xB0 => Instruction::OR(Par8::R8(Reg8::B)),
            0xB1 => Instruction::OR(Par8::R8(Reg8::C)),
            0xB2 => Instruction::OR(Par8::R8(Reg8::D)),
            0xB3 => Instruction::OR(Par8::R8(Reg8::E)),
            0xB4 => Instruction::OR(Par8::R8(Reg8::H)),
            0xB5 => Instruction::OR(Par8::R8(Reg8::L)),
            0xB6 => Instruction::OR(Par8::AHL),
            0xB7 => Instruction::OR(Par8::R8(Reg8::A)),
            0xF6 => Instruction::OR(Par8::D8(self.load_pc_inc())),

            0x88 => Instruction::ADC(Par8::R8(Reg8::B)),
            0x89 => Instruction::ADC(Par8::R8(Reg8::C)),
            0x8A => Instruction::ADC(Par8::R8(Reg8::D)),
            0x8B => Instruction::ADC(Par8::R8(Reg8::E)),
            0x8C => Instruction::ADC(Par8::R8(Reg8::H)),
            0x8D => Instruction::ADC(Par8::R8(Reg8::L)),
            0x8E => Instruction::ADC(Par8::AHL),
            0x8F => Instruction::ADC(Par8::R8(Reg8::A)),
            0xCE => Instruction::ADC(Par8::D8(self.load_pc_inc())),

            0x98 => Instruction::SBC(Par8::R8(Reg8::B)),
            0x99 => Instruction::SBC(Par8::R8(Reg8::C)),
            0x9A => Instruction::SBC(Par8::R8(Reg8::D)),
            0x9B => Instruction::SBC(Par8::R8(Reg8::E)),
            0x9C => Instruction::SBC(Par8::R8(Reg8::H)),
            0x9D => Instruction::SBC(Par8::R8(Reg8::L)),
            0x9E => Instruction::SBC(Par8::AHL),
            0x9F => Instruction::SBC(Par8::R8(Reg8::A)),
            0xDE => Instruction::SBC(Par8::D8(self.load_pc_inc())),

            0xA8 => Instruction::XOR(Par8::R8(Reg8::B)),
            0xA9 => Instruction::XOR(Par8::R8(Reg8::C)),
            0xAA => Instruction::XOR(Par8::R8(Reg8::D)),
            0xAB => Instruction::XOR(Par8::R8(Reg8::E)),
            0xAC => Instruction::XOR(Par8::R8(Reg8::H)),
            0xAD => Instruction::XOR(Par8::R8(Reg8::L)),
            0xAE => Instruction::XOR(Par8::AHL),
            0xAF => Instruction::XOR(Par8::R8(Reg8::A)),
            0xEE => Instruction::XOR(Par8::D8(self.load_pc_inc())),

            0xB8 => Instruction::CP(Par8::R8(Reg8::B)),
            0xB9 => Instruction::CP(Par8::R8(Reg8::C)),
            0xBA => Instruction::CP(Par8::R8(Reg8::D)),
            0xBB => Instruction::CP(Par8::R8(Reg8::E)),
            0xBC => Instruction::CP(Par8::R8(Reg8::H)),
            0xBD => Instruction::CP(Par8::R8(Reg8::L)),
            0xBE => Instruction::CP(Par8::AHL),
            0xBF => Instruction::CP(Par8::R8(Reg8::A)),
            0xFE => Instruction::CP(Par8::D8(self.load_pc_inc())),

            0x09 => Instruction::ADDr16(Reg16::BC),
            0x19 => Instruction::ADDr16(Reg16::DE),
            0x29 => Instruction::ADDr16(Reg16::HL),
            0x39 => Instruction::ADDr16(Reg16::SP),

            0xE8 => Instruction::ADDr16n8(Reg16::SP, self.load_pc_inc()),

            0x36 => Instruction::LDHLn(self.load_pc_inc()),

            0x02 => Instruction::LDr16r8(Reg16::BC, Reg8::A),
            0x12 => Instruction::LDr16r8(Reg16::DE, Reg8::A),

            0x0A => Instruction::LDr8r16(Reg8::A, Reg16::BC),
            0x1A => Instruction::LDr8r16(Reg8::A, Reg16::DE),

            0xEA => Instruction::LDn16r8(self.load_pc_n16(), Reg8::A),
            0xFA => Instruction::LDr8n16(Reg8::A, self.load_pc_n16()),

            0x01 => Instruction::LDr16n16(Reg16::BC, self.load_pc_n16()),
            0x11 => Instruction::LDr16n16(Reg16::DE, self.load_pc_n16()),
            0x21 => Instruction::LDr16n16(Reg16::HL, self.load_pc_n16()),
            0x31 => Instruction::LDr16n16(Reg16::SP, self.load_pc_n16()),

            0x06 => Instruction::LDrn(Reg8::B, self.load_pc_inc()),
            0x0E => Instruction::LDrn(Reg8::C, self.load_pc_inc()),
            0x16 => Instruction::LDrn(Reg8::D, self.load_pc_inc()),
            0x1E => Instruction::LDrn(Reg8::E, self.load_pc_inc()),
            0x26 => Instruction::LDrn(Reg8::H, self.load_pc_inc()),
            0x2E => Instruction::LDrn(Reg8::L, self.load_pc_inc()),
            0x3E => Instruction::LDrn(Reg8::A, self.load_pc_inc()),

            0x46 => Instruction::LDr8r16(Reg8::B, Reg16::HL),
            0x4E => Instruction::LDr8r16(Reg8::C, Reg16::HL),
            0x56 => Instruction::LDr8r16(Reg8::D, Reg16::HL),
            0x5E => Instruction::LDr8r16(Reg8::E, Reg16::HL),
            0x66 => Instruction::LDr8r16(Reg8::H, Reg16::HL),
            0x6E => Instruction::LDr8r16(Reg8::L, Reg16::HL),
            0x7E => Instruction::LDr8r16(Reg8::A, Reg16::HL),

            0x70 => Instruction::LDr16r8(Reg16::HL, Reg8::B),
            0x71 => Instruction::LDr16r8(Reg16::HL, Reg8::C),
            0x72 => Instruction::LDr16r8(Reg16::HL, Reg8::D),
            0x73 => Instruction::LDr16r8(Reg16::HL, Reg8::E),
            0x74 => Instruction::LDr16r8(Reg16::HL, Reg8::H),
            0x75 => Instruction::LDr16r8(Reg16::HL, Reg8::L),
            0x77 => Instruction::LDr16r8(Reg16::HL, Reg8::A),

            0x40 => Instruction::LD(Reg8::B, Reg8::B),
            0x41 => Instruction::LD(Reg8::B, Reg8::C),
            0x42 => Instruction::LD(Reg8::B, Reg8::D),
            0x43 => Instruction::LD(Reg8::B, Reg8::E),
            0x44 => Instruction::LD(Reg8::B, Reg8::H),
            0x45 => Instruction::LD(Reg8::B, Reg8::L),
            0x47 => Instruction::LD(Reg8::B, Reg8::A),
            0x48 => Instruction::LD(Reg8::C, Reg8::B),
            0x49 => Instruction::LD(Reg8::C, Reg8::C),
            0x4A => Instruction::LD(Reg8::C, Reg8::D),
            0x4B => Instruction::LD(Reg8::C, Reg8::E),
            0x4C => Instruction::LD(Reg8::C, Reg8::H),
            0x4D => Instruction::LD(Reg8::C, Reg8::L),
            0x4F => Instruction::LD(Reg8::C, Reg8::A),
            0x50 => Instruction::LD(Reg8::D, Reg8::B),
            0x51 => Instruction::LD(Reg8::D, Reg8::C),
            0x52 => Instruction::LD(Reg8::D, Reg8::D),
            0x53 => Instruction::LD(Reg8::D, Reg8::E),
            0x54 => Instruction::LD(Reg8::D, Reg8::H),
            0x55 => Instruction::LD(Reg8::D, Reg8::L),
            0x57 => Instruction::LD(Reg8::D, Reg8::A),
            0x58 => Instruction::LD(Reg8::E, Reg8::B),
            0x59 => Instruction::LD(Reg8::E, Reg8::C),
            0x5A => Instruction::LD(Reg8::E, Reg8::D),
            0x5B => Instruction::LD(Reg8::E, Reg8::E),
            0x5C => Instruction::LD(Reg8::E, Reg8::H),
            0x5D => Instruction::LD(Reg8::E, Reg8::L),
            0x5F => Instruction::LD(Reg8::E, Reg8::A),
            0x60 => Instruction::LD(Reg8::H, Reg8::B),
            0x61 => Instruction::LD(Reg8::H, Reg8::C),
            0x62 => Instruction::LD(Reg8::H, Reg8::D),
            0x63 => Instruction::LD(Reg8::H, Reg8::E),
            0x64 => Instruction::LD(Reg8::H, Reg8::H),
            0x65 => Instruction::LD(Reg8::H, Reg8::L),
            0x67 => Instruction::LD(Reg8::H, Reg8::A),
            0x68 => Instruction::LD(Reg8::L, Reg8::B),
            0x69 => Instruction::LD(Reg8::L, Reg8::C),
            0x6A => Instruction::LD(Reg8::L, Reg8::D),
            0x6B => Instruction::LD(Reg8::L, Reg8::E),
            0x6C => Instruction::LD(Reg8::L, Reg8::H),
            0x6D => Instruction::LD(Reg8::L, Reg8::L),
            0x6F => Instruction::LD(Reg8::H, Reg8::A),
            0x78 => Instruction::LD(Reg8::A, Reg8::B),
            0x79 => Instruction::LD(Reg8::A, Reg8::C),
            0x7A => Instruction::LD(Reg8::A, Reg8::D),
            0x7B => Instruction::LD(Reg8::A, Reg8::E),
            0x7C => Instruction::LD(Reg8::A, Reg8::H),
            0x7D => Instruction::LD(Reg8::A, Reg8::L),
            0x7F => Instruction::LD(Reg8::A, Reg8::A),

            0x18 => Instruction::JRn8(self.load_pc_inc()),
            0x20 => Instruction::JRCn8(Cond::NZ, self.load_pc_inc()),
            0x28 => Instruction::JRCn8(Cond::Z, self.load_pc_inc()),
            0x30 => Instruction::JRCn8(Cond::NC, self.load_pc_inc()),
            0x38 => Instruction::JRCn8(Cond::C, self.load_pc_inc()),

            0xC3 => Instruction::JPn16(self.load_pc_n16()),
            0xC2 => Instruction::JPCn16(Cond::NZ, self.load_pc_n16()),
            0xCA => Instruction::JPCn16(Cond::Z, self.load_pc_n16()),
            0xD2 => Instruction::JPCn16(Cond::NC, self.load_pc_n16()),
            0xDA => Instruction::JPCn16(Cond::C, self.load_pc_n16()),
            0xC1 => Instruction::POP(Reg16::BC),
            0xD1 => Instruction::POP(Reg16::DE),
            0xE1 => Instruction::POP(Reg16::HL),
            0xF1 => Instruction::POP(Reg16::AF),
            0xC5 => Instruction::PUSH(Reg16::BC),
            0xD5 => Instruction::PUSH(Reg16::DE),
            0xE5 => Instruction::PUSH(Reg16::HL),
            0xF5 => Instruction::PUSH(Reg16::AF),

            0xCD => Instruction::CALL(self.load_pc_n16()),
            0xC4 => Instruction::CALLC(Cond::NZ, self.load_pc_n16()),
            0xCC => Instruction::CALLC(Cond::Z, self.load_pc_n16()),
            0xD4 => Instruction::CALLC(Cond::NC, self.load_pc_n16()),
            0xDC => Instruction::CALLC(Cond::C, self.load_pc_n16()),

            0xC9 => Instruction::RET(),
            0xC0 => Instruction::RETC(Cond::NZ),
            0xC8 => Instruction::RETC(Cond::Z),
            0xD0 => Instruction::RETC(Cond::NC),
            0xD8 => Instruction::RETC(Cond::C),

            _ => Instruction::Unimplemented(opcode),
        };
    }
    fn execute(&mut self, inst: &Instruction) {
        match inst {
            Instruction::ADD(p) => {
                let param = self.par8(*p);
                let (new_val, carry) = self.reg_a.overflowing_add(param);
                let half_carry = (self.reg_a & 0x0F) + (param & 0x0F) > 0x0F;
                self.reg_a = new_val;
                self.set_flags(new_val == 0, false, carry, half_carry)
            }
            Instruction::ADDr16(r) => {
                self.set_reg16(
                    Reg16::HL,
                    self.reg16(Reg16::HL).wrapping_add(self.reg16(*r)),
                )
                // TODO: flags
            }
            Instruction::ADDr16n8(r16, n8) => {
                self.set_reg16(*r16, self.reg16(*r16).wrapping_add(*n8 as u16))
                // TODO: flags
            }
            Instruction::SUB(p) => {
                self.reg_a = self.reg_a.wrapping_sub(self.par8(*p))
                // TODO: flags
            }
            Instruction::ADC(p) => {
                self.reg_a = self.reg_a.wrapping_add(self.par8(*p))
                // TODO: carry flag?
                // TODO: flags
            }
            Instruction::SBC(p) => {
                self.reg_a = self.reg_a.wrapping_sub(self.par8(*p))
                // TODO: carry flag?
                // TODO: flags
            }
            Instruction::CP(p) => {
                if self.reg_a == self.par8(*p) {
                    self.reg_a = 0
                } else {
                    self.reg_a = 1
                }
                // TODO: flags
            }

            Instruction::AND(p) => {
                if (self.reg_a != 0) && (self.par8(*p) != 0) {
                    self.reg_a = 1
                } else {
                    self.reg_a = 0
                }
            }
            Instruction::OR(p) => {
                if (self.reg_a != 0) || (self.par8(*p) != 0) {
                    self.reg_a = 1
                } else {
                    self.reg_a = 0
                }
            }
            Instruction::XOR(p) => {
                if (self.reg_a != 0) != (self.par8(*p) != 0) {
                    self.reg_a = 1
                } else {
                    self.reg_a = 0
                }
            }
            Instruction::INC8(r) => {
                self.set_reg8(*r, self.reg8(*r).wrapping_add(1))
                // TODO: flags
            }
            Instruction::DEC8(r) => {
                self.set_reg8(*r, self.reg8(*r).wrapping_sub(1))
                // TODO: flags
            }
            Instruction::INC16(r) => {
                self.set_reg16(*r, self.reg16(*r).wrapping_add(1))
                // TODO: flags
            }
            Instruction::DEC16(r) => {
                self.set_reg16(*r, self.reg16(*r).wrapping_sub(1))
                // TODO: flags
            }

            Instruction::LD(r1, r2) => {
                self.set_reg8(*r1, self.reg8(*r2));
            }
            Instruction::LDrn(r, v) => {
                self.set_reg8(*r, *v);
            }
            Instruction::LDr8r16(r8, r16) => {
                let addr = self.reg16(*r16);
                let value = self.load8(addr);
                self.set_reg8(*r8, value);
            }
            Instruction::LDr16r8(r16, r8) => {
                let addr = self.reg16(*r16);
                let value = self.reg8(*r8);
                self.store8(addr, value);
            }
            Instruction::LDHLn(n) => {
                let hl = self.reg16(Reg16::HL);
                self.store8(hl, *n);
            }
            Instruction::LDr8n16(r8, n16) => {
                let value = self.load8(*n16);
                self.set_reg8(*r8, value);
            }
            Instruction::LDn16r8(n16, r8) => {
                let value = self.reg8(*r8);
                self.store8(*n16, value);
            }
            Instruction::LDr16n16(r16, n16) => {
                self.set_reg16(*r16, *n16);
            }

            Instruction::JPn16(n16) => {
                self.reg_pc = *n16;
            }
            Instruction::JPCn16(cond, n16) => {
                if self.check(*cond) {
                    self.reg_pc = *n16;
                }
            }
            Instruction::JRn8(n8) => {
                self.reg_pc = self.reg_pc.wrapping_add(*n8 as u16);
            }
            Instruction::JRCn8(cond, n8) => {
                if self.check(*cond) {
                    self.reg_pc = self.reg_pc.wrapping_add(*n8 as u16);
                }
            }

            Instruction::PUSH(r) => {
                let val = self.reg16(*r);
                self.push(val);
            }
            Instruction::POP(r) => {
                let val = self.pop();
                self.set_reg16(*r, val);
            }
            Instruction::CALL(n16) => {
                self.push(self.reg_pc);
                self.reg_pc = *n16;
            }
            Instruction::CALLC(cond, n16) => {
                if self.check(*cond) {
                    self.push(self.reg_pc);
                    self.reg_pc = *n16;
                }
            }

            Instruction::RET() => {
                let pc = self.pop();
                self.reg_pc = pc;
            }
            Instruction::RETC(cond) => {
                if self.check(*cond) {
                    let pc = self.pop();
                    self.reg_pc = pc;
                }
            }

            Instruction::Nop() => (),

            // not implemented
            Instruction::Unimplemented(_op) => (),
        }
    }

    fn push(&mut self, n: u16) {
        self.reg_sp = self.reg_sp.wrapping_sub(2);
        self.store16(self.reg_sp, n);
    }

    fn pop(&mut self) -> u16 {
        let val = self.load16(self.reg_sp);
        self.reg_sp = self.reg_sp.wrapping_add(2);
        val
    }

    fn check(&self, cond: Cond) -> bool {
        match cond {
            Cond::Z => (self.reg_f & 0x80) != 0,
            Cond::NZ => (self.reg_f & 0x80) == 0,

            Cond::C => (self.reg_f & 0x10) != 0,
            Cond::NC => (self.reg_f & 0x10) == 0,
        }
    }
    pub fn run(&mut self) {
        // load the start address specified in the header
        self.set_reg16(Reg16::PC, 0x102);
        let pc = self.load_pc_n16();
        self.set_reg16(Reg16::PC, pc);
        println!("Starting from {}", pc);

        loop {
            let inst = self.load_instruction();
            println!("{}", inst);
            self.execute(&inst);
        }
    }
}

pub fn build_cpu(bus: Bus) -> Cpu {
    return Cpu {
        bus,
        reg_pc: 0,
        reg_sp: 0,
        reg_a: 0,
        reg_b: 0,
        reg_c: 0,
        reg_d: 0,
        reg_e: 0,
        reg_f: 0,
        reg_h: 0,
        reg_l: 0,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init_cpu_wo_rom() -> Cpu {
        let rom_data = vec![0; 0x8000];
        let bus = build_bus(rom_data);
        return build_cpu(bus);
    }

    #[test]
    fn test_reg8() {
        for r in vec![
            Reg8::A,
            Reg8::F,
            Reg8::L,
            Reg8::H,
            Reg8::E,
            Reg8::D,
            Reg8::C,
            Reg8::B,
        ] {
            let mut cpu = init_cpu_wo_rom();
            cpu.set_reg8(r, 33);
            assert_eq!(cpu.reg8(r), 33);
        }
    }
    #[test]
    fn test_reg16() {
        for r in vec![Reg16::HL, Reg16::BC, Reg16::DE, Reg16::SP, Reg16::PC] {
            let mut cpu = init_cpu_wo_rom();
            cpu.set_reg16(r, 3342);
            assert_eq!(cpu.reg16(r), 3342);
        }
    }

    #[test]
    fn test_exec_ld() {
        let mut cpu = init_cpu_wo_rom();
        cpu.set_reg8(Reg8::A, 42);
        cpu.execute(&Instruction::LD(Reg8::D, Reg8::A));
        assert_eq!(cpu.reg8(Reg8::A), 42);
    }
}

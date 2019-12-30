use std::fmt;

#[derive(Copy, Clone)]
enum Reg {
    A, //0x07
    F, //0x06
    L, //0x05
    H, //0x04
    E, //0x03
    D, //0x02
    C, //0x01
    B, //0x00
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::A => write!(f, "A"),
            Reg::F => write!(f, "F"),
            Reg::L => write!(f, "L"),
            Reg::H => write!(f, "H"),
            Reg::E => write!(f, "E"),
            Reg::D => write!(f, "D"),
            Reg::C => write!(f, "C"),
            Reg::B => write!(f, "B"),
        }
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    LD(Reg, Reg),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::LD(r1, r2) => write!(f, "LD {}, {}", r1, r2),
        }
    }
}

impl Instruction {
    fn Execute(&self, cpu: &mut Cpu) {
        match self {
            Instruction::LD(r1, r2) => {
                cpu.set_reg8(*r1, cpu.reg8(*r2));
            }
        }
    }
}

fn GenOpcodeMap() -> Vec<(u8, Instruction)> {
    return vec![
        (0x40, Instruction::LD(Reg::B, Reg::B)),
        (0x41, Instruction::LD(Reg::B, Reg::C)),
        (0x42, Instruction::LD(Reg::B, Reg::D)),
        (0x43, Instruction::LD(Reg::B, Reg::E)),
        (0x44, Instruction::LD(Reg::B, Reg::H)),
        (0x45, Instruction::LD(Reg::B, Reg::L)),
        (0x47, Instruction::LD(Reg::B, Reg::A)),
        (0x48, Instruction::LD(Reg::C, Reg::B)),
        (0x49, Instruction::LD(Reg::C, Reg::C)),
        (0x4A, Instruction::LD(Reg::C, Reg::D)),
        (0x4B, Instruction::LD(Reg::C, Reg::E)),
        (0x4C, Instruction::LD(Reg::C, Reg::H)),
        (0x4D, Instruction::LD(Reg::C, Reg::L)),
        (0x4F, Instruction::LD(Reg::C, Reg::A)),
        (0x50, Instruction::LD(Reg::D, Reg::B)),
        (0x51, Instruction::LD(Reg::D, Reg::C)),
        (0x52, Instruction::LD(Reg::D, Reg::D)),
        (0x53, Instruction::LD(Reg::D, Reg::E)),
        (0x54, Instruction::LD(Reg::D, Reg::H)),
        (0x55, Instruction::LD(Reg::D, Reg::L)),
        (0x57, Instruction::LD(Reg::D, Reg::A)),
        (0x58, Instruction::LD(Reg::E, Reg::B)),
        (0x59, Instruction::LD(Reg::E, Reg::C)),
        (0x5A, Instruction::LD(Reg::E, Reg::D)),
        (0x5B, Instruction::LD(Reg::E, Reg::E)),
        (0x5C, Instruction::LD(Reg::E, Reg::H)),
        (0x5D, Instruction::LD(Reg::E, Reg::L)),
        (0x5F, Instruction::LD(Reg::E, Reg::A)),
        (0x60, Instruction::LD(Reg::H, Reg::B)),
        (0x61, Instruction::LD(Reg::H, Reg::C)),
        (0x62, Instruction::LD(Reg::H, Reg::D)),
        (0x63, Instruction::LD(Reg::H, Reg::E)),
        (0x64, Instruction::LD(Reg::H, Reg::H)),
        (0x65, Instruction::LD(Reg::H, Reg::L)),
        (0x67, Instruction::LD(Reg::H, Reg::A)),
        (0x68, Instruction::LD(Reg::L, Reg::B)),
        (0x69, Instruction::LD(Reg::L, Reg::C)),
        (0x6A, Instruction::LD(Reg::L, Reg::D)),
        (0x6B, Instruction::LD(Reg::L, Reg::E)),
        (0x6C, Instruction::LD(Reg::L, Reg::H)),
        (0x6D, Instruction::LD(Reg::L, Reg::L)),
        (0x6F, Instruction::LD(Reg::H, Reg::A)),
        (0x78, Instruction::LD(Reg::A, Reg::B)),
        (0x79, Instruction::LD(Reg::A, Reg::C)),
        (0x7A, Instruction::LD(Reg::A, Reg::D)),
        (0x7B, Instruction::LD(Reg::A, Reg::E)),
        (0x7C, Instruction::LD(Reg::A, Reg::H)),
        (0x7D, Instruction::LD(Reg::A, Reg::L)),
        (0x7F, Instruction::LD(Reg::A, Reg::A)),
    ];
}

struct Cpu {
    PC: u16,
    SP: u16,

    A: u8,
    B: u8,
    C: u8,
    D: u8,
    E: u8,
    F: u8,

    H: u8,
    L: u8,
}

impl Cpu {
    fn reg8(&self, r: Reg) -> u8 {
        match r {
            Reg::A => return self.A,
            Reg::B => return self.B,
            Reg::C => return self.C,
            Reg::D => return self.D,
            Reg::E => return self.E,
            Reg::F => return self.F,
            Reg::H => return self.H,
            Reg::L => return self.L,
        }
    }

    fn set_reg8(&mut self, r: Reg, val: u8) {
        match r {
            Reg::A => self.A = val,
            Reg::B => self.B = val,
            Reg::C => self.C = val,
            Reg::D => self.D = val,
            Reg::E => self.E = val,
            Reg::F => self.F = val,
            Reg::H => self.H = val,
            Reg::L => self.L = val,
        }
    }
}

fn build_cpu() -> Cpu {
    return Cpu {
        PC: 0,
        SP: 0,
        A: 0,
        B: 0,
        C: 0,
        D: 0,
        E: 0,
        F: 0,
        H: 0,
        L: 0,
    };
}

fn main() {
    let mut cpu = build_cpu();

    cpu.set_reg8(Reg::A, 33);
    println!("{}: {}", Reg::A, cpu.reg8(Reg::A));
    println!("{}", Instruction::LD(Reg::D, Reg::A));
    Instruction::LD(Reg::D, Reg::A).Execute(&mut cpu);
    println!("{}: {}", Reg::D, cpu.reg8(Reg::D));
}

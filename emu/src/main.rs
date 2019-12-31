use std::fmt;

#[derive(Copy, Clone, Debug)]
enum Reg8 {
    A, //0x07
    F, //0x06
    L, //0x05
    H, //0x04
    E, //0x03
    D, //0x02
    C, //0x01
    B, //0x00
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
    SP,
    PC,
}
impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    LD(Reg8, Reg8),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::LD(r1, r2) => write!(f, "LD {}, {}", r1, r2),
        }
    }
}

fn gen_opcode_map() -> Vec<(u8, Instruction)> {
    return vec![
        (0x40, Instruction::LD(Reg8::B, Reg8::B)),
        (0x41, Instruction::LD(Reg8::B, Reg8::C)),
        (0x42, Instruction::LD(Reg8::B, Reg8::D)),
        (0x43, Instruction::LD(Reg8::B, Reg8::E)),
        (0x44, Instruction::LD(Reg8::B, Reg8::H)),
        (0x45, Instruction::LD(Reg8::B, Reg8::L)),
        (0x47, Instruction::LD(Reg8::B, Reg8::A)),
        (0x48, Instruction::LD(Reg8::C, Reg8::B)),
        (0x49, Instruction::LD(Reg8::C, Reg8::C)),
        (0x4A, Instruction::LD(Reg8::C, Reg8::D)),
        (0x4B, Instruction::LD(Reg8::C, Reg8::E)),
        (0x4C, Instruction::LD(Reg8::C, Reg8::H)),
        (0x4D, Instruction::LD(Reg8::C, Reg8::L)),
        (0x4F, Instruction::LD(Reg8::C, Reg8::A)),
        (0x50, Instruction::LD(Reg8::D, Reg8::B)),
        (0x51, Instruction::LD(Reg8::D, Reg8::C)),
        (0x52, Instruction::LD(Reg8::D, Reg8::D)),
        (0x53, Instruction::LD(Reg8::D, Reg8::E)),
        (0x54, Instruction::LD(Reg8::D, Reg8::H)),
        (0x55, Instruction::LD(Reg8::D, Reg8::L)),
        (0x57, Instruction::LD(Reg8::D, Reg8::A)),
        (0x58, Instruction::LD(Reg8::E, Reg8::B)),
        (0x59, Instruction::LD(Reg8::E, Reg8::C)),
        (0x5A, Instruction::LD(Reg8::E, Reg8::D)),
        (0x5B, Instruction::LD(Reg8::E, Reg8::E)),
        (0x5C, Instruction::LD(Reg8::E, Reg8::H)),
        (0x5D, Instruction::LD(Reg8::E, Reg8::L)),
        (0x5F, Instruction::LD(Reg8::E, Reg8::A)),
        (0x60, Instruction::LD(Reg8::H, Reg8::B)),
        (0x61, Instruction::LD(Reg8::H, Reg8::C)),
        (0x62, Instruction::LD(Reg8::H, Reg8::D)),
        (0x63, Instruction::LD(Reg8::H, Reg8::E)),
        (0x64, Instruction::LD(Reg8::H, Reg8::H)),
        (0x65, Instruction::LD(Reg8::H, Reg8::L)),
        (0x67, Instruction::LD(Reg8::H, Reg8::A)),
        (0x68, Instruction::LD(Reg8::L, Reg8::B)),
        (0x69, Instruction::LD(Reg8::L, Reg8::C)),
        (0x6A, Instruction::LD(Reg8::L, Reg8::D)),
        (0x6B, Instruction::LD(Reg8::L, Reg8::E)),
        (0x6C, Instruction::LD(Reg8::L, Reg8::H)),
        (0x6D, Instruction::LD(Reg8::L, Reg8::L)),
        (0x6F, Instruction::LD(Reg8::H, Reg8::A)),
        (0x78, Instruction::LD(Reg8::A, Reg8::B)),
        (0x79, Instruction::LD(Reg8::A, Reg8::C)),
        (0x7A, Instruction::LD(Reg8::A, Reg8::D)),
        (0x7B, Instruction::LD(Reg8::A, Reg8::E)),
        (0x7C, Instruction::LD(Reg8::A, Reg8::H)),
        (0x7D, Instruction::LD(Reg8::A, Reg8::L)),
        (0x7F, Instruction::LD(Reg8::A, Reg8::A)),
    ];
}

struct Cpu {
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
            Reg16::SP => self.reg_sp = val,
            Reg16::PC => self.reg_pc = val,
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

    fn execute(&mut self, inst: &Instruction) {
        match inst {
            Instruction::LD(r1, r2) => {
                self.set_reg8(*r1, self.reg8(*r2));
            }
        }
    }
}

fn build_cpu() -> Cpu {
    return Cpu {
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

fn main() {
    let mut cpu = build_cpu();

    cpu.set_reg8(Reg8::A, 33);
    cpu.execute(&Instruction::LD(Reg8::D, Reg8::A));
    println!("{}: {}", Reg8::D, cpu.reg8(Reg8::D));
}

#[cfg(test)]
mod tests {
    use super::*;
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
            let mut cpu = build_cpu();
            cpu.set_reg8(r, 33);
            assert_eq!(cpu.reg8(r), 33);
        }
    }
    #[test]
    fn test_reg16() {
        for r in vec![Reg16::HL, Reg16::BC, Reg16::DE, Reg16::SP, Reg16::PC] {
            let mut cpu = build_cpu();
            cpu.set_reg16(r, 3342);
            assert_eq!(cpu.reg16(r), 3342);
        }
    }

    #[test]
    fn test_exec_ld() {
        let mut cpu = build_cpu();
        cpu.set_reg8(Reg8::A, 42);
        cpu.execute(&Instruction::LD(Reg8::D, Reg8::A));
        assert_eq!(cpu.reg8(Reg8::A), 42);
    }
}

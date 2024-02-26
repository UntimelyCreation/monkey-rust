use std::fmt;
use std::mem::transmute;

mod test_code;

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    OpConstant = 0,
    OpNull,
    OpPop,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJump,
    OpJumpCond,
    OpGetGlobal,
    OpSetGlobal,
    OpArray,
    OpHash,
    OpIndex,
    OpCall,
    OpReturnValue,
    OpReturn,
    OpGetLocal,
    OpSetLocal,
    OpGetBuiltin,
    OpClosure,
    OpGetFree,
    OpCurrentClosure,
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        unsafe { transmute(value) }
    }
}

#[derive(Clone, PartialEq)]
pub struct Instructions {
    pub stream: Vec<u8>,
}

impl Instructions {
    pub fn new() -> Self {
        Self { stream: Vec::new() }
    }
}

impl fmt::Debug for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stream = &self.stream;
        let mut position = 0;

        while position < self.stream.len() {
            let op = Opcode::from(stream[position]);
            let definition = lookup(&op);
            let instruction_len = definition.operand_widths.iter().sum::<usize>() + 1;
            let (operands, _) = read_operands(
                &definition,
                &stream[(position + 1)..(position + instruction_len)],
            );

            let instr_str = match operands.len() {
                2 => format!("{} {} {}", definition.name, operands[0], operands[1]),
                1 => format!("{} {}", definition.name, operands[0]),
                0 => definition.name.to_string(),
                _ => panic!(
                    "unsupported operand width: {}",
                    definition.operand_widths.len()
                ),
            };
            write!(f, "{:03} {}; ", position, instr_str)?;

            position += instruction_len;
        }
        Ok(())
    }
}

pub struct Definition {
    name: &'static str,
    operand_widths: Vec<usize>,
}

pub fn lookup(op: &Opcode) -> Definition {
    match op {
        Opcode::OpConstant => Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        },
        Opcode::OpNull => Definition {
            name: "OpNull",
            operand_widths: vec![],
        },
        Opcode::OpPop => Definition {
            name: "OpPop",
            operand_widths: vec![],
        },
        Opcode::OpAdd => Definition {
            name: "OpAdd",
            operand_widths: vec![],
        },
        Opcode::OpSub => Definition {
            name: "OpSub",
            operand_widths: vec![],
        },
        Opcode::OpMul => Definition {
            name: "OpMul",
            operand_widths: vec![],
        },
        Opcode::OpDiv => Definition {
            name: "OpDiv",
            operand_widths: vec![],
        },
        Opcode::OpTrue => Definition {
            name: "OpTrue",
            operand_widths: vec![],
        },
        Opcode::OpFalse => Definition {
            name: "OpFalse",
            operand_widths: vec![],
        },
        Opcode::OpEqual => Definition {
            name: "OpEqual",
            operand_widths: vec![],
        },
        Opcode::OpNotEqual => Definition {
            name: "OpNotEqual",
            operand_widths: vec![],
        },
        Opcode::OpGreaterThan => Definition {
            name: "OpGreaterThan",
            operand_widths: vec![],
        },
        Opcode::OpMinus => Definition {
            name: "OpMinus",
            operand_widths: vec![],
        },
        Opcode::OpBang => Definition {
            name: "OpMinus",
            operand_widths: vec![],
        },
        Opcode::OpJump => Definition {
            name: "OpJump",
            operand_widths: vec![2],
        },
        Opcode::OpJumpCond => Definition {
            name: "OpJumpCond",
            operand_widths: vec![2],
        },
        Opcode::OpGetGlobal => Definition {
            name: "OpGetGlobal",
            operand_widths: vec![2],
        },
        Opcode::OpSetGlobal => Definition {
            name: "OpSetGlobal",
            operand_widths: vec![2],
        },
        Opcode::OpArray => Definition {
            name: "OpArray",
            operand_widths: vec![2],
        },
        Opcode::OpHash => Definition {
            name: "OpHash",
            operand_widths: vec![2],
        },
        Opcode::OpIndex => Definition {
            name: "OpIndex",
            operand_widths: vec![],
        },
        Opcode::OpCall => Definition {
            name: "OpCall",
            operand_widths: vec![1],
        },
        Opcode::OpReturnValue => Definition {
            name: "OpReturnValue",
            operand_widths: vec![],
        },
        Opcode::OpReturn => Definition {
            name: "OpReturn",
            operand_widths: vec![],
        },
        Opcode::OpGetLocal => Definition {
            name: "OpGetLocal",
            operand_widths: vec![1],
        },
        Opcode::OpSetLocal => Definition {
            name: "OpSetLocal",
            operand_widths: vec![1],
        },
        Opcode::OpGetBuiltin => Definition {
            name: "OpSetLocal",
            operand_widths: vec![1],
        },
        Opcode::OpClosure => Definition {
            name: "OpClosure",
            operand_widths: vec![2, 1],
        },
        Opcode::OpGetFree => Definition {
            name: "OpGetFree",
            operand_widths: vec![1],
        },
        Opcode::OpCurrentClosure => Definition {
            name: "OpGetFree",
            operand_widths: vec![],
        },
    }
}

pub fn make(op: Opcode, operands: &[i32]) -> Vec<u8> {
    let definition = lookup(&op);

    let instruction_len = definition.operand_widths.iter().sum::<usize>() + 1;

    let mut instruction = vec![0; instruction_len];
    instruction[0] = op.clone() as u8;

    let mut offset = 1;
    for (i, o) in operands.iter().enumerate() {
        let width = definition.operand_widths[i];
        match width {
            2 => instruction[offset..(offset + 2)].copy_from_slice(&(*o as u16).to_be_bytes()),
            1 => instruction[offset..(offset + 1)].copy_from_slice(&(*o as u8).to_be_bytes()),
            _ => panic!("unsupported operand width: {}", width),
        }
        offset += width;
    }

    instruction
}

pub fn read_operands(def: &Definition, instruction: &[u8]) -> (Vec<i32>, usize) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => match instruction[offset..(offset + 2)].try_into() {
                Ok(bytes) => {
                    operands[i] = u16::from_be_bytes(bytes) as i32;
                }
                Err(..) => todo!(),
            },
            1 => operands[i] = instruction[offset] as i32,
            _ => panic!("unsupported operand width: {}", width),
        }
        offset += width;
    }

    (operands, offset)
}

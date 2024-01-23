mod test_code;

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    OpConstant,
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
}

impl TryFrom<u8> for Opcode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Opcode::OpConstant),
            _ => Err("no opcode found"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instructions {
    pub stream: Vec<(Opcode, Vec<u8>)>,
}

impl Default for Instructions {
    fn default() -> Self {
        Self::new()
    }
}

impl Instructions {
    pub fn new() -> Self {
        Self { stream: Vec::new() }
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
    }
}

pub fn make(op: Opcode, operands: &[i32]) -> (Opcode, Vec<u8>) {
    let definition = lookup(&op);

    let operands_len = definition.operand_widths.iter().sum();

    let mut instr_operands = vec![0; operands_len];

    let mut offset = 0;
    for (i, o) in operands.iter().enumerate() {
        let width = definition.operand_widths[i];
        match width {
            2 => instr_operands[offset..(offset + 2)].copy_from_slice(&(*o as u16).to_be_bytes()),
            _ => todo!(),
        }
        offset += width;
    }

    (op, instr_operands)
}

pub fn parse(def: &Definition, instruction: (Opcode, Vec<u8>)) -> (Vec<i32>, usize) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => match instruction.1[offset..(offset + 2)].try_into() {
                Ok(bytes) => {
                    operands[i] = u16::from_be_bytes(bytes) as i32;
                }
                Err(..) => todo!(),
            },
            _ => todo!(),
        }
        offset += width;
    }
    (operands, offset)
}

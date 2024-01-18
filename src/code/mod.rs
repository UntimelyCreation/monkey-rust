mod test_code;

pub type Instructions = Vec<u8>;
pub type Opcode = u8;

pub const OP_CONSTANT: Opcode = 0;

pub struct Definition {
    name: String,
    operand_widths: Vec<usize>,
}

pub fn lookup(op: Opcode) -> Option<Definition> {
    match op {
        OP_CONSTANT => Some(Definition {
            name: "OpConstant".to_owned(),
            operand_widths: vec![2],
        }),
        _ => None,
    }
}

pub fn make(op: Opcode, operands: &[i32]) -> Vec<u8> {
    let definition = lookup(op);

    if let Some(def) = definition {
        let mut instr_len = 1;
        for w in def.operand_widths.iter() {
            instr_len += w;
        }

        let mut instruction = vec![0; instr_len];
        instruction[0] = op;

        let mut offset = 1;
        for (i, o) in operands.iter().enumerate() {
            let width = def.operand_widths[i];
            match width {
                2 => instruction[offset..(offset + 2)].copy_from_slice(&(*o as u16).to_be_bytes()),
                _ => todo!(),
            }
            offset += width;
        }

        instruction
    } else {
        Vec::new()
    }
}

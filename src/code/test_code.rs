#[cfg(test)]
mod tests {
    use crate::code::{lookup, make, parse, Opcode};

    #[test]
    fn test_make() {
        let instructions = [
            make(Opcode::OpConstant, &[65534]),
            make(Opcode::OpPop, &[]),
            make(Opcode::OpAdd, &[]),
            make(Opcode::OpSub, &[]),
            make(Opcode::OpMul, &[]),
            make(Opcode::OpDiv, &[]),
            make(Opcode::OpTrue, &[]),
            make(Opcode::OpFalse, &[]),
            make(Opcode::OpEqual, &[]),
            make(Opcode::OpNotEqual, &[]),
            make(Opcode::OpGreaterThan, &[]),
            make(Opcode::OpMinus, &[]),
            make(Opcode::OpBang, &[]),
        ];
        let expected_instrs = [
            (Opcode::OpConstant, vec![255, 254]),
            (Opcode::OpPop, vec![]),
            (Opcode::OpAdd, vec![]),
            (Opcode::OpSub, vec![]),
            (Opcode::OpMul, vec![]),
            (Opcode::OpDiv, vec![]),
            (Opcode::OpTrue, vec![]),
            (Opcode::OpFalse, vec![]),
            (Opcode::OpEqual, vec![]),
            (Opcode::OpNotEqual, vec![]),
            (Opcode::OpGreaterThan, vec![]),
            (Opcode::OpMinus, vec![]),
            (Opcode::OpBang, vec![]),
        ];

        for (i, instr) in instructions.into_iter().enumerate() {
            let expected = expected_instrs[i].clone();
            assert_eq!(expected, instr);
        }
    }

    #[test]
    fn test_parse() {
        let operands = [
            parse(
                &lookup(&Opcode::OpConstant),
                (Opcode::OpConstant, vec![255, 254]),
            ),
            parse(&lookup(&Opcode::OpPop), (Opcode::OpPop, vec![])),
            parse(&lookup(&Opcode::OpAdd), (Opcode::OpAdd, vec![])),
            parse(&lookup(&Opcode::OpSub), (Opcode::OpSub, vec![])),
            parse(&lookup(&Opcode::OpMul), (Opcode::OpMul, vec![])),
            parse(&lookup(&Opcode::OpDiv), (Opcode::OpDiv, vec![])),
            parse(&lookup(&Opcode::OpTrue), (Opcode::OpTrue, vec![])),
            parse(&lookup(&Opcode::OpFalse), (Opcode::OpFalse, vec![])),
            parse(&lookup(&Opcode::OpEqual), (Opcode::OpEqual, vec![])),
            parse(&lookup(&Opcode::OpNotEqual), (Opcode::OpNotEqual, vec![])),
            parse(
                &lookup(&Opcode::OpGreaterThan),
                (Opcode::OpGreaterThan, vec![]),
            ),
            parse(&lookup(&Opcode::OpMinus), (Opcode::OpMinus, vec![])),
            parse(&lookup(&Opcode::OpBang), (Opcode::OpBang, vec![])),
        ];
        let expected_operands = [
            (vec![65534], 2),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
            (vec![], 0),
        ];

        for (i, operand) in operands.into_iter().enumerate() {
            let expected = expected_operands[i].clone();
            assert_eq!(expected, operand);
        }
    }
}

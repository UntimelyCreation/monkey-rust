#[cfg(test)]
mod tests {
    use crate::code::{lookup, make, parse, Opcode};

    #[test]
    fn test_make() {
        let instructions = [
            make(Opcode::OpConstant, &[65534]),
            make(Opcode::OpNull, &[]),
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
            make(Opcode::OpJump, &[42]),
            make(Opcode::OpJumpCond, &[267]),
        ];
        let expected_instrs = [
            (Opcode::OpConstant, vec![255, 254]),
            (Opcode::OpNull, vec![]),
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
            (Opcode::OpJump, vec![0, 42]),
            (Opcode::OpJumpCond, vec![1, 11]),
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
            parse(&lookup(&Opcode::OpNull), (Opcode::OpNull, vec![])),
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
            parse(&lookup(&Opcode::OpJump), (Opcode::OpConstant, vec![0, 42])),
            parse(
                &lookup(&Opcode::OpJumpCond),
                (Opcode::OpConstant, vec![1, 11]),
            ),
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
            (vec![], 0),
            (vec![42], 2),
            (vec![267], 2),
        ];

        for (i, operand) in operands.into_iter().enumerate() {
            let expected = expected_operands[i].clone();
            assert_eq!(expected, operand);
        }
    }
}

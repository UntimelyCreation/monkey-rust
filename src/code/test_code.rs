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
            make(Opcode::OpGetGlobal, &[5]),
            make(Opcode::OpSetGlobal, &[6]),
            make(Opcode::OpArray, &[512]),
            make(Opcode::OpHash, &[524]),
            make(Opcode::OpIndex, &[]),
            make(Opcode::OpCall, &[]),
            make(Opcode::OpReturnValue, &[]),
            make(Opcode::OpReturn, &[]),
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
            (Opcode::OpGetGlobal, vec![0, 5]),
            (Opcode::OpSetGlobal, vec![0, 6]),
            (Opcode::OpArray, vec![2, 0]),
            (Opcode::OpHash, vec![2, 12]),
            (Opcode::OpIndex, vec![]),
            (Opcode::OpCall, vec![]),
            (Opcode::OpReturnValue, vec![]),
            (Opcode::OpReturn, vec![]),
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
            parse(&lookup(&Opcode::OpJump), (Opcode::OpJump, vec![0, 42])),
            parse(
                &lookup(&Opcode::OpJumpCond),
                (Opcode::OpJumpCond, vec![1, 11]),
            ),
            parse(
                &lookup(&Opcode::OpGetGlobal),
                (Opcode::OpGetGlobal, vec![0, 5]),
            ),
            parse(
                &lookup(&Opcode::OpSetGlobal),
                (Opcode::OpSetGlobal, vec![0, 6]),
            ),
            parse(&lookup(&Opcode::OpArray), (Opcode::OpArray, vec![2, 0])),
            parse(&lookup(&Opcode::OpHash), (Opcode::OpHash, vec![2, 12])),
            parse(&lookup(&Opcode::OpIndex), (Opcode::OpIndex, vec![])),
            parse(&lookup(&Opcode::OpCall), (Opcode::OpCall, vec![])),
            parse(
                &lookup(&Opcode::OpReturnValue),
                (Opcode::OpReturnValue, vec![]),
            ),
            parse(&lookup(&Opcode::OpReturn), (Opcode::OpReturn, vec![])),
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
            (vec![5], 2),
            (vec![6], 2),
            (vec![512], 2),
            (vec![524], 2),
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

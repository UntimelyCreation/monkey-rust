#[cfg(test)]
mod tests {
    use crate::code::{lookup, make, read_operands, Opcode};

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
            make(Opcode::OpCall, &[100]),
            make(Opcode::OpReturnValue, &[]),
            make(Opcode::OpReturn, &[]),
            make(Opcode::OpGetLocal, &[9]),
            make(Opcode::OpSetLocal, &[255]),
            make(Opcode::OpGetBuiltin, &[128]),
            make(Opcode::OpClosure, &[65534, 255]),
            make(Opcode::OpGetFree, &[19]),
            make(Opcode::OpCurrentClosure, &[]),
        ];
        let expected_instrs = [
            (vec![0, 255, 254]),
            (vec![1]),
            (vec![2]),
            (vec![3]),
            (vec![4]),
            (vec![5]),
            (vec![6]),
            (vec![7]),
            (vec![8]),
            (vec![9]),
            (vec![10]),
            (vec![11]),
            (vec![12]),
            (vec![13]),
            (vec![14, 0, 42]),
            (vec![15, 1, 11]),
            (vec![16, 0, 5]),
            (vec![17, 0, 6]),
            (vec![18, 2, 0]),
            (vec![19, 2, 12]),
            (vec![20]),
            (vec![21, 100]),
            (vec![22]),
            (vec![23]),
            (vec![24, 9]),
            (vec![25, 255]),
            (vec![26, 128]),
            (vec![27, 255, 254, 255]),
            (vec![28, 19]),
            (vec![29]),
        ];

        for (i, instr) in instructions.into_iter().enumerate() {
            let expected = expected_instrs[i].clone();
            assert_eq!(expected, instr);
        }
    }

    #[test]
    fn test_read_operands() {
        let operands = [
            read_operands(&lookup(&Opcode::OpConstant), &[255, 254]),
            read_operands(&lookup(&Opcode::OpNull), &[]),
            read_operands(&lookup(&Opcode::OpPop), &[]),
            read_operands(&lookup(&Opcode::OpAdd), &[]),
            read_operands(&lookup(&Opcode::OpSub), &[]),
            read_operands(&lookup(&Opcode::OpMul), &[]),
            read_operands(&lookup(&Opcode::OpDiv), &[]),
            read_operands(&lookup(&Opcode::OpTrue), &[]),
            read_operands(&lookup(&Opcode::OpFalse), &[]),
            read_operands(&lookup(&Opcode::OpEqual), &[]),
            read_operands(&lookup(&Opcode::OpNotEqual), &[]),
            read_operands(&lookup(&Opcode::OpGreaterThan), &[]),
            read_operands(&lookup(&Opcode::OpMinus), &[]),
            read_operands(&lookup(&Opcode::OpBang), &[]),
            read_operands(&lookup(&Opcode::OpJump), &[0, 42]),
            read_operands(&lookup(&Opcode::OpJumpCond), &[1, 11]),
            read_operands(&lookup(&Opcode::OpGetGlobal), &[0, 5]),
            read_operands(&lookup(&Opcode::OpSetGlobal), &[0, 6]),
            read_operands(&lookup(&Opcode::OpArray), &[2, 0]),
            read_operands(&lookup(&Opcode::OpHash), &[2, 12]),
            read_operands(&lookup(&Opcode::OpIndex), &[]),
            read_operands(&lookup(&Opcode::OpCall), &[100]),
            read_operands(&lookup(&Opcode::OpReturnValue), &[]),
            read_operands(&lookup(&Opcode::OpReturn), &[]),
            read_operands(&lookup(&Opcode::OpGetLocal), &[9]),
            read_operands(&lookup(&Opcode::OpSetLocal), &[255]),
            read_operands(&lookup(&Opcode::OpGetBuiltin), &[128]),
            read_operands(&lookup(&Opcode::OpClosure), &[255, 254, 255]),
            read_operands(&lookup(&Opcode::OpGetFree), &[19]),
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
            (vec![100], 1),
            (vec![], 0),
            (vec![], 0),
            (vec![9], 1),
            (vec![255], 1),
            (vec![128], 1),
            (vec![65534, 255], 3),
            (vec![19], 1),
        ];

        for (i, operand) in operands.into_iter().enumerate() {
            let expected = expected_operands[i].clone();
            assert_eq!(expected, operand);
        }
    }
}

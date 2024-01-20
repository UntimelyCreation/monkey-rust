#[cfg(test)]
mod tests {
    use crate::code::{lookup, make, parse, Opcode};

    #[test]
    fn test_make() {
        let instructions = [make(Opcode::OpConstant, &[65534]), make(Opcode::OpAdd, &[])];
        let expected_instrs = [
            (Opcode::OpConstant, vec![255, 254]),
            (Opcode::OpAdd, vec![]),
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
            parse(&lookup(&Opcode::OpAdd), (Opcode::OpAdd, vec![])),
        ];
        let expected_operands = [(vec![65534], 2), (vec![], 0)];

        for (i, operand) in operands.into_iter().enumerate() {
            let expected = expected_operands[i].clone();
            assert_eq!(expected, operand);
        }
    }
}

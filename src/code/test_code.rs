#[cfg(test)]
mod tests {
    use crate::code::{lookup, make, parse, Opcode};

    #[test]
    fn test_make() {
        let instruction = make(Opcode::OpConstant, &[65534]);
        let expected = (Opcode::OpConstant, vec![255, 254]);
        assert_eq!(expected, instruction);
    }

    #[test]
    fn test_parse() {
        let instruction = parse(
            &lookup(&Opcode::OpConstant),
            (Opcode::OpConstant, vec![255, 254]),
        );
        let expected = (vec![65534], 2);
        assert_eq!(expected, instruction);
    }
}

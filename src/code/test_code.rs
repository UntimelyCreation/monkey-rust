#[cfg(test)]
mod tests {
    use crate::code::{make, OP_CONSTANT};

    #[test]
    fn test_make() {
        let instruction = make(OP_CONSTANT, &[65534]);
        let expected = vec![OP_CONSTANT, 255, 254];
        assert_eq!(expected, instruction);
    }
}

pub fn parse_input<T: std::io::BufRead>(input: T) -> std::io::Result<Vec<String>> {
    input.lines().collect()
}

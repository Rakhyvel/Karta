#[derive(Clone, Copy, Debug)]
/// Represents a position in a text file
pub struct Span {
    /// Beginning of the token
    pub start: u32,
    /// End of the token
    pub end: u32,
}

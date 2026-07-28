#[derive(Clone, Copy, Debug)]
/// Represents a position in a text file
pub(crate) struct Span {
    /// Beginning of the token
    pub(crate) start: u32,
    /// End of the token
    pub(crate) end: u32,
}

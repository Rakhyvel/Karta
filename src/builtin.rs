#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Builtin {
    Add,
}

impl Builtin {
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "@add" => Some(Builtin::Add),
            _ => None,
        }
    }
}

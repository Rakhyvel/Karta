// Sure would like Builtin'Range from Ada...
macro_rules! builtins {
    ($($variant:ident => $name:literal),* $(,)?) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub enum Builtin { $($variant),* }

        impl Builtin {
            pub const ALL: &'static [Builtin] = &[$(Builtin::$variant),*];

            pub fn repr(self) -> &'static str {
                match self { $(Builtin::$variant => $name),* }
            }

            pub fn parse(name: &str) -> Option<Self> {
                Self::ALL.iter().copied().find(|b| b.repr() == name)
            }
        }
    };
}

builtins! {
    Eql => "@eql",
    Neq => "@neq",
    Lsr => "@lsr",
    Lte => "@lte",
    Gtr => "@gtr",
    Gte => "@gte",
    Neg => "@neg",
    Add => "@add",
    Sub => "@sub",
    Mul => "@mul",
    Div => "@div",
    Mod => "@mod",
    And => "@and",
    Or => "@or",
    Not => "@not",
}

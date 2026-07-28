use crate::span::Span;

pub struct SourceFile {
    pub text: String,
}

impl SourceFile {
    pub fn span_text(&self, span: Span) -> &str {
        &self.text[(span.start as usize)..(span.end as usize)]
    }
}

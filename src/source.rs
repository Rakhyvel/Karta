use crate::span::Span;

pub struct SourceFile {
    text: String,
}

impl SourceFile {
    pub fn new(mut text: String) -> Self {
        text.push('\n');
        Self { text }
    }

    pub fn span_text(&self, span: Span) -> &str {
        &self.text[(span.start as usize)..(span.end as usize)]
    }

    pub fn text(&self) -> &str {
        &self.text
    }
}

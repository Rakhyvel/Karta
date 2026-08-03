use crate::span::Span;

pub struct SourceFile {
    text: String,
    line_starts: Vec<u32>,
}

impl SourceFile {
    pub fn new(mut text: String) -> Self {
        text.push('\n');
        let mut line_starts = vec![0];
        line_starts.extend(
            text.char_indices()
                .filter_map(|(i, c)| (c == '\n').then_some(i as u32 + 1)),
        );
        Self { text, line_starts }
    }

    pub fn span_text(&self, span: Span) -> &str {
        &self.text[(span.start as usize)..(span.end as usize)]
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn line_col(&self, offset: u32) -> (u32, u32) {
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        let col = self.text[self.line_starts[line] as usize..offset as usize]
            .chars()
            .count();
        (line as u32 + 1, col as u32 + 1)
    }

    pub fn line_col_utf16(&self, offset: u32) -> (u32, u32) {
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        let col = self.text[self.line_starts[line] as usize..offset as usize]
            .encode_utf16()
            .count();
        (line as u32, col as u32)
    }
}

use std::collections::HashMap;

use karta::{analysis::SemanticKind, source::SourceFile, span::Span, KartaContext};
use lsp_server::{Connection, Message, ProtocolError, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, Position, PublishDiagnosticsParams, Range, SemanticToken,
    SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

pub fn main() -> Result<(), ProtocolError> {
    eprintln!("Karta LSP awaiting connection...");
    let (connection, io_threads) = Connection::stdio();

    eprintln!("Constructing capabilities...");
    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        semantic_tokens_provider: Some(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::STRING,
                        SemanticTokenType::ENUM_MEMBER,
                        SemanticTokenType::FUNCTION,
                        SemanticTokenType::PARAMETER,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::OPERATOR,
                    ],
                    token_modifiers: vec![
                        SemanticTokenModifier::DEFAULT_LIBRARY,
                        SemanticTokenModifier::READONLY,
                    ],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..Default::default()
            }
            .into(),
        ),
        ..Default::default()
    })
    .unwrap();

    eprintln!("Initializing...");
    let _init_params = connection.initialize(capabilities).unwrap();
    let mut docs: HashMap<Url, String> = HashMap::new();

    eprintln!("Waiting for messages...");
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    break;
                }

                if req.method == "textDocument/semanticTokens/full" {
                    let params =
                        serde_json::from_value::<SemanticTokensParams>(req.params).unwrap();

                    let doc = docs.get(&params.text_document.uri);

                    let mut data: Vec<SemanticToken> = Vec::new();

                    if let Some(text) = doc {
                        let mut kctx = KartaContext::new();
                        let analysis = kctx.analyze(text);

                        let (mut prev_line, mut prev_start) = (0u32, 0u32);
                        for (span, kind) in analysis.tokens {
                            let Some((token_type, modifiers)) = semantic_kind(kind) else {
                                continue;
                            };

                            let (line, start) = analysis.source.line_col_utf16(span.start);
                            let length =
                                analysis.source.span_text(span).encode_utf16().count() as u32;

                            let delta_line = line - prev_line;
                            let delta_start = if delta_line == 0 {
                                start - prev_start
                            } else {
                                start
                            };

                            data.push(SemanticToken {
                                delta_line,
                                delta_start,
                                length,
                                token_type,
                                token_modifiers_bitset: modifiers,
                            });
                            (prev_line, prev_start) = (line, start);
                        }
                    }

                    connection
                        .sender
                        .send(Message::Response(Response {
                            id: req.id,
                            result: Some(
                                serde_json::to_value(SemanticTokens {
                                    result_id: None,
                                    data,
                                })
                                .unwrap(),
                            ),
                            error: None,
                        }))
                        .unwrap();
                }

                eprintln!("got req: {}", req.method.as_str());
            }

            Message::Notification(not) => {
                let doc = match not.method.as_str() {
                    "textDocument/didOpen" => {
                        let params =
                            serde_json::from_value::<DidOpenTextDocumentParams>(not.params)
                                .unwrap();

                        docs.insert(
                            params.text_document.uri.clone(),
                            params.text_document.text.clone(),
                        );

                        Some((
                            params.text_document.uri,
                            params.text_document.version,
                            params.text_document.text,
                        ))
                    }

                    "textDocument/didChange" => {
                        let params =
                            serde_json::from_value::<DidChangeTextDocumentParams>(not.params)
                                .unwrap();

                        let text = params.content_changes.into_iter().next().unwrap().text;

                        docs.insert(params.text_document.uri.clone(), text.clone());

                        Some((params.text_document.uri, params.text_document.version, text))
                    }

                    "textDocument/didClose" => {
                        let params =
                            serde_json::from_value::<DidCloseTextDocumentParams>(not.params)
                                .unwrap();

                        docs.remove(&params.text_document.uri);

                        None
                    }

                    _ => None,
                };

                if let Some((uri, version, text)) = doc {
                    publish_diagnostics(&connection, uri, version, text);
                }
            }

            Message::Response(_) => {}
        }
    }

    io_threads.join().unwrap();

    Ok(())
}

fn publish_diagnostics(connection: &Connection, uri: Url, _version: i32, text: String) {
    let mut kctx = KartaContext::new();
    let analysis = kctx.analyze(text);

    let publish = PublishDiagnosticsParams {
        uri,
        diagnostics: analysis
            .diagnostics
            .iter()
            .map(|err| Diagnostic {
                range: span_to_range(&analysis.source, err.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("karta".to_string()),
                message: format!("{}", err.kind),
                ..Default::default()
            })
            .collect(),
        version: None,
    };

    connection
        .sender
        .send(Message::Notification(lsp_server::Notification {
            method: "textDocument/publishDiagnostics".to_string(),
            params: serde_json::to_value(publish).unwrap(),
        }))
        .unwrap();
}

fn span_to_range(src: &SourceFile, span: Span) -> Range {
    let (sl, sc) = src.line_col_utf16(span.start);
    let (el, ec) = src.line_col_utf16(span.end);
    Range {
        start: Position {
            line: sl,
            character: sc,
        },
        end: Position {
            line: el,
            character: ec,
        },
    }
}

fn semantic_kind(kind: SemanticKind) -> Option<(u32, u32)> {
    const KEYWORD: u32 = 0;
    const NUMBER: u32 = 1;
    const STRING: u32 = 2;
    const ATOM: u32 = 3;
    const FUNCTION: u32 = 4;
    const PARAMETER: u32 = 5;
    const VARIABLE: u32 = 6;
    const OPERATOR: u32 = 7;

    const DEFAULT_LIBRARY: u32 = 1 << 0;
    const READONLY: u32 = 1 << 1;

    Some(match kind {
        SemanticKind::Keyword => return None, // Keep the textmate grammar!

        SemanticKind::Number => (NUMBER, 0),
        SemanticKind::String => (STRING, 0),
        SemanticKind::Atom => (ATOM, 0),
        SemanticKind::Builtin => (FUNCTION, DEFAULT_LIBRARY),
        SemanticKind::Function => (FUNCTION, 0),
        SemanticKind::Parameter => (PARAMETER, 0),
        SemanticKind::Constant => (VARIABLE, READONLY),
        SemanticKind::Variable => (VARIABLE, 0),

        SemanticKind::Operator => (OPERATOR, 0),

        _ => return None,
    })
}

use karta::{source::SourceFile, span::Span, KartaContext};
use lsp_server::{Connection, Message, ProtocolError};
use lsp_types::{
    notification::DidChangeTextDocument, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Position, PublishDiagnosticsParams,
    Range, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

pub fn main() -> Result<(), ProtocolError> {
    eprintln!("Karta LSP awaiting connection...");
    let (connection, io_threads) = Connection::stdio();

    eprintln!("Constructing capabilities...");
    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();

    eprintln!("Initializing...");
    let _init_params = connection.initialize(capabilities).unwrap();

    eprintln!("Waiting for messages...");
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    break;
                }

                eprintln!("got req: {}", req.method.as_str());
            }

            Message::Notification(not) => {
                let doc = match not.method.as_str() {
                    "textDocument/didOpen" => {
                        serde_json::from_value::<DidOpenTextDocumentParams>(not.params)
                            .ok()
                            .map(|p| {
                                (
                                    p.text_document.uri,
                                    p.text_document.version,
                                    p.text_document.text,
                                )
                            })
                    }
                    "textDocument/didChange" => {
                        serde_json::from_value::<DidChangeTextDocumentParams>(not.params)
                            .ok()
                            .and_then(|p| {
                                let text = p.content_changes.into_iter().next()?.text;
                                Some((p.text_document.uri, p.text_document.version, text))
                            })
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
                message: format!("{:?}", err.kind), // TODO: kctx.render(err.kind)
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

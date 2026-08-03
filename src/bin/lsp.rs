use lsp_server::{Connection, Message, ProtocolError};
use lsp_types::{
    PositionEncodingKind, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

pub fn main() -> Result<(), ProtocolError> {
    eprintln!("Karta LSP awaiting connection...");
    let (connection, io_threads) = Connection::stdio();

    eprintln!("Constructing capabilities...");
    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        position_encoding: Some(PositionEncodingKind::UTF8),
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

                // TODO: Run analyze() on text here?

                eprintln!("got req: {}", req.method.as_str());
            }

            Message::Notification(not) => {
                // TODO: Run analyze() on text here?
                eprintln!("got not: {:?}", not);
            }

            Message::Response(_) => {}
        }
    }

    io_threads.join().unwrap();

    Ok(())
}

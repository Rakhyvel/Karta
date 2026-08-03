import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
    LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const exe = process.platform == 'win32' ? 'karta-lsp.exe' : 'karta-lsp';
    const configured = workspace.getConfiguration('karta').get<string>('server.path');

    const command = configured && configured.length > 0
        ? configured
        : context.asAbsolutePath(path.join('..', '..', 'target', 'debug', exe));

    const serverOptions: ServerOptions = {
        run: { command, transport: TransportKind.stdio },
        debug: { command, transport: TransportKind.stdio },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'karta' }],
    };

    client = new LanguageClient('karta', 'Karta Language Server', serverOptions, clientOptions);
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}
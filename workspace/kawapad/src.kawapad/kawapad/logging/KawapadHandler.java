package kawapad.logging;

import java.util.logging.LogRecord;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import kawapad.KawapadFrame;

public class KawapadHandler extends java.util.logging.Handler {
	private KawapadFrame kawapadFrame = KawapadFrame.createStaticInstance();
	@Override
	public void publish(LogRecord record) {
		if ( isLoggable(record) ) {
			String str = getFormatter().format(record);
			Document document = kawapadFrame.getKawapad().getDocument();
			try {
				document.insertString(document.getLength(), str, null );
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public void flush() {
	}
	@Override
	public void close() throws SecurityException {
		kawapadFrame.requestClose();
	}
}

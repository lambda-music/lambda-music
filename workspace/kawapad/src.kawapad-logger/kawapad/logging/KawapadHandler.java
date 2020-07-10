package kawapad.logging;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import kawapad.Kawapad;
import kawapad.KawapadFrame;

public class KawapadHandler extends java.util.logging.Handler {
	public static final String KAWAPAD_LOGGING_HANDLER = "kawapad-logging-handler";

	public static List<java.util.logging.Handler> getHandlers(Kawapad kawapad ) {
		synchronized ( kawapad ) {
			Map<Object, Object> map = kawapad.getMemoMap();
			if (! map.containsKey( KawapadHandler.KAWAPAD_LOGGING_HANDLER ) ) {
				map.put( KawapadHandler.KAWAPAD_LOGGING_HANDLER,
						Collections.synchronizedList( new ArrayList<java.util.logging.Handler>()) );
			}
			List<java.util.logging.Handler> list = 
					(List<java.util.logging.Handler>) map.get(KawapadHandler.KAWAPAD_LOGGING_HANDLER);
			return list;
		}
	}

	public static void setLoggingLevel( Kawapad kawapad, String level ) {
		synchronized ( kawapad ) {
			List<Handler> handlers = getHandlers( kawapad );
			for ( Handler h : handlers ) {
				h.setLevel( Level.parse(level));
			}
		}
	}
	public static KawapadFrame createFrame() {
		KawapadFrame kawapadFrame = KawapadFrame.createStaticInstance(false);
		kawapadFrame.getKawapad().setEnabledConfirmation(false);
		kawapadFrame.setHideOnClose(true);
		return kawapadFrame;
				
	}
	private final KawapadFrame kawapadFrame;
	public KawapadHandler(KawapadFrame kawapadFrame) {
		super();
		this.kawapadFrame = kawapadFrame;
		{
			Kawapad kawapad = this.kawapadFrame.getKawapad();
			synchronized ( kawapad ) {
				getHandlers( kawapad ).add(this);
			}
		}
	}
	public KawapadHandler() {
		this( createFrame() );
	}
	public KawapadFrame getKawapadFrame() {
		return kawapadFrame;
	}

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
			SwingUtilities.invokeLater( new Runnable() {
				@Override
				public void run() {
					kawapadFrame.setFocusableWindowState(false);
					kawapadFrame.setVisible(true);
					kawapadFrame.setFocusableWindowState(true);
				}
			});
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

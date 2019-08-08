package kawapad;

import java.io.StringReader;

import javax.swing.JFrame;

import gnu.kawa.io.InPort;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.secretary.SecretaryMessage;

public class KawaEvalMessage extends SecretaryMessage.NoThrow<Scheme, KawaEvalMessage.Result> {
	public static final class Result {
		public final Object result;
		public final Throwable error;
		public Result( Object result ) {
			this.result = result;
			this.error = null; 
		}
		public Result( Throwable error ) {
			this.result = null;
			this.error = error; 
		}
	}
	private final JFrame frame;
	private final String script;
	public KawaEvalMessage( String script, JFrame frame ) {
		this.frame  = frame;
		this.script = script;
	}
	@Override
	public KawaEvalMessage.Result execute0(Scheme scheme, Object[] args )  {
		StringReader reader = new StringReader( this.script );
		try {
			SchemeUtils.putVar( scheme, "scheme", scheme );
			SchemeUtils.putVar( scheme, "frame", frame );
			
			return new KawaEvalMessage.Result( scheme.eval( new InPort(reader) ) ); 
		} catch ( Throwable e ) {
			return new KawaEvalMessage.Result( e ); 
		} finally {
			SchemeUtils.putVar( scheme, "scheme", false );
			SchemeUtils.putVar( scheme, "frame", false );
			
			reader.close();
		}
	}
}

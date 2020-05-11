package lamu.lib.secretary;

import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import lamu.lib.Invokable;
import lamu.lib.log.Logger;

public class Secretary<R> extends SecretaryMessageQueue<R> {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
    public static void main(String[] args) {
        logInfo( "HELLO" );
    }
    public Secretary() {
    }

    private volatile R executive;

    @Override
    protected R getExecutive() {
        return this.executive;
    }
    protected void setExecutive( R executive ) {
        this.executive = executive;
    }

    public void setExecutiveSecretarially( R executive ) {
        executeSecretarially( new SecretaryMessage.NoReturnNoThrow<R>() {
            @Override
            public void execute0(R currentExecutive, Object[] args ) {
                Secretary.this.setExecutive( executive );
            }
        }, Invokable.NOARG );
    }
}

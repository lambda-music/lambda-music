package lamu.lib.secretary;

public abstract interface SecretaryMessage<R,T,E extends Throwable>  {
    public abstract T execute( R resource, Object[] args ) throws E;
    
    public abstract class NoThrow<R,T> implements SecretaryMessage<R,T,RuntimeException>  {
        public final T execute( R resource, Object[] args ) throws RuntimeException {
            return this.execute0( resource, args );
        }
        public abstract T execute0( R resource, Object[] args );
    }
    public abstract class NoReturnNoThrow<R> implements SecretaryMessage<R,Void,RuntimeException>  {
        public final Void execute( R resource, Object[] args ) throws RuntimeException {
            this.execute0( resource, args );
            return null;
        }
        public abstract void execute0( R resource, Object[] args );
    }
    public abstract class NoReturn<R,E extends Throwable> implements SecretaryMessage<R,Void,E>  {
        public final Void execute( R resource, Object[] args ) throws E {
            this.execute0( resource, args );
            return null;
        }
        public abstract void execute0( R resource, Object[] args ) throws E;
    }
        
}

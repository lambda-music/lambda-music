package pulsar.lib.secretary;

public class InvokableMessage<R> extends SecretaryMessage.NoThrow<R, Object> {
    final Invokable invokable;
    public InvokableMessage( Invokable invokable ) {
        super();
        this.invokable = invokable;
    }

    @Override
    public Object execute0( R resource, Object[] args ) {
        return invokable.invoke( args );
    }
}

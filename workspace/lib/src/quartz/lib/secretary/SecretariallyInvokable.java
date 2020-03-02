package quartz.lib.secretary;

public class SecretariallyInvokable<R> implements Invokable {
    private SecretaryMessageQueue<R> messageQueue;
    private Invokable invokable;
    public SecretariallyInvokable( SecretaryMessageQueue<R> messageQueue, Invokable invokable ) {
        this.messageQueue = messageQueue;
        this.invokable = invokable;
    }

    @Override
    public Object invoke(Object... args) {
        return messageQueue.executeSecretarially( new InvokableMessage<R>( invokable ), args );
    }
}

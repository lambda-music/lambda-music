package lamu.lib.secretary;

public interface SecretaryMessageExecutor<R> {
    public <T,E extends Throwable> T executeSecretarially( SecretaryMessage<R,T,E> message, Object... args ) throws E, InterruptedException;
}

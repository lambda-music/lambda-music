package metro;

public class MetroException extends Exception {
    public MetroException() {
        super();
    }
    public MetroException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public MetroException(String message, Throwable cause) {
        super(message, cause);
    }
    public MetroException(String message) {
        super(message);
    }

    public MetroException(Throwable cause) {
        super(cause);
    }
}

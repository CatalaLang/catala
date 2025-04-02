package catala.runtime.exception;

// I guess catala exceptions should be unchecked?
// (hence the RuntimeException base)
// Rationale: I'm not sure an attempt to recover from a 
// CatalaException would make much sense?
public class CatalaException extends RuntimeException {

    public CatalaException(String message, Throwable t) {
        super(message, t);
    }

    public CatalaException(String message) {
        super(message);
    }
}

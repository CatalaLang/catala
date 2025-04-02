package catala.runtime.exception;

public class ConflictException extends CatalaException {
    public ConflictException(String message, Throwable t){
        super(message, t);
    }
}
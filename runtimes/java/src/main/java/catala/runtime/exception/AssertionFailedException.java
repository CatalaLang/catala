package catala.runtime.exception;

public class AssertionFailedException extends CatalaException {
    public AssertionFailedException(String message, Throwable t){
        super(message, t);
    }
}
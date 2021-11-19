package logger;

public class Logger<W extends Logger.Writer> {
    
    public interface Writer {
        
        public void write(String message);
        
    }

    private Writer writer;

    public Logger(W writer) {
        this.writer = writer;
    }

    public void log(String message) {
        writer.write(message);
    }
    
    public static class NetworkWriter implements Writer {
        
        public void write(String message) {
            System.out.println(message);
        }
        
    }
    
    public static class FileWriter implements Writer {
        
        public void write(String message) {
            System.out.println(message);
        }
        
    }
    
}

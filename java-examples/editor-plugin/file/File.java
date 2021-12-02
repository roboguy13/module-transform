package file;

import logger.Logger;

public class File implements Logger.Writer {
    public void open() {
        System.out.println("Opening file...");
    }

    @Override
    public void write(String message) {
        System.out.println("File is writing: " + message);        
    }
}

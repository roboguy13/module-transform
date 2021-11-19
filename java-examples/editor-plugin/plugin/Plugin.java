package plugin;

import logger.Logger;

public class Plugin <W extends Logger.Writer> {

    private Logger<W> logger;

    public Plugin(Logger<W> logger) {
        this.logger = logger;
    }

    public void run() {
        logger.log("Opening plugin...");
    }
    
}

package network;

import file.File; // How can we stop this?

import logger.Logger;

public class Network implements Logger.Writer {
    File file = new File(); // How can we stop this?

    public void open() {
        System.out.println("Opening network...");
        file.open(); // How can we stop this?
    }    

    @Override
    public void write(String message) {
        System.out.println("Network is writing: " + message);   
        file.write(message); //How can we stop this?
    }
}

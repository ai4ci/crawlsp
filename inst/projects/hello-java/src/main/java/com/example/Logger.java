package com.example;

/**
 * Simple console logger used by other classes in this toy project.
 */
public class Logger implements Formatter {

    private final String prefix;

    public Logger(String prefix) {
        this.prefix = prefix;
    }

    public void log(String message) {
        System.out.println(format(message));
    }
    
    /**
     * Wrap a plain log string.
     *
     * @param message the raw log message
     * @return the formatted message
     */
    public String format(String message) {
        return "[" + prefix + "] " + message;
    }
}

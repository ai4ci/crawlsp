package com.example;

/**
 * Formats greeting messages. Uses {@link Logger} to record each formatting
 * operation.
 */
public class MessageFormatter implements Formatter {

    private final Logger logger;

    public MessageFormatter() {
        this.logger = new Logger("MessageFormatter");
    }

    /**
     * Wrap a plain greeting string in a decorative border.
     *
     * @param greeting the raw greeting text produced by {@link Greeter}
     * @return the formatted message
     */
    public String format(String greeting) {
        String border = "=".repeat(greeting.length() + 4);
        String formatted = border + "\n| " + greeting + " |\n" + border;
        logger.log("Formatted message of length " + greeting.length());
        return formatted;
    }
}

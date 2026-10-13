package com.example;

/**
 * Produces personalised greeting strings. Delegates formatting to
 * {@link MessageFormatter} and logs activity via {@link Logger}.
 */
public class Greeter {

    private final MessageFormatter formatter;
    private final Logger logger;

    public Greeter() {
        this.formatter = new MessageFormatter();
        this.logger = new Logger("Greeter");
    }

    /**
     * Build a formatted greeting for the given name.
     *
     * @param name the recipient's name
     * @return a fully formatted greeting string
     */
    public String greet(String name) {
        String raw = "Hello, " + name + "!";
        logger.log("Greeting: " + name);
        return formatter.format(raw);
    }
}

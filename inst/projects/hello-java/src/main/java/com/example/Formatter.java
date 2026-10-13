package com.example;

/**
 * Formats greeting messages. Uses {@link Logger} to record each formatting
 * operation.
 */
public interface Formatter {

    /**
     * Wrap a plain string in a decorative.
     *
     * @param toFormat the raw text
     * @return the formatted message
     */
    public String format(String toFormat);
}

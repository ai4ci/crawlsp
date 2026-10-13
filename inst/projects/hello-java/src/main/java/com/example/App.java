package com.example;

/**
 * Entry point for the hello-java toy project.
 * Demonstrates the call chain: {@link App} → {@link Greeter} →
 * {@link MessageFormatter} → {@link Logger}.
 */
public class App {

    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        String[] names = {"World", "Alice", "Bob"};
        for (String name : names) {
            System.out.println(greeter.greet(name));
            System.out.println();
        }
    }
}

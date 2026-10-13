# Examples

Worked examples for the functions documented in the [API Reference](./api.md).

Worked examples for the functions documented in the [API Reference](./api.md#API Reference).

## Basic Greeting

Call `greet` with a name to produce a simple hello message.

```
greet("Alice")
# => "Hello, Alice!"
```

## Farewell

Call `farewell` to close a conversation.

```
farewell("Alice")
# => "Goodbye, Alice."
```

## Formatted Message

Pass any string through `format_message` to add a decorative border.

```
format_message("Hello, Alice!")
# =>
# ===============
# | Hello, Alice! |
# ===============
```

## Chained Usage

The functions are designed to be composed. A typical sequence:

```
message = greet("Bob")
output  = format_message(message)
farewell("Bob")
```

---

Back to [Getting Started](./getting-started.md) or [README](../test-readme.md).

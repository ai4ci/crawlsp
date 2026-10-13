# API Reference

This page documents the toy functions provided by the hello-markdown project.

See [Getting Started](./getting-started.md) for setup instructions,
or jump straight to [Examples](./examples.md).

## Functions

### `greet(name)`

Returns a greeting string for the given name.

| Parameter | Type   | Description              |
|-----------|--------|--------------------------|
| `name`    | string | The name to greet        |

**Returns:** a string of the form `"Hello, <name>!"`.

---

### `farewell(name)`

Returns a farewell string for the given name.

| Parameter | Type   | Description              |
|-----------|--------|--------------------------|
| `name`    | string | The name to say goodbye to |

**Returns:** a string of the form `"Goodbye, <name>."`.

---

### `format_message(text)`

Wraps a plain string in a decorative border.

| Parameter | Type   | Description           |
|-----------|--------|-----------------------|
| `text`    | string | The message to format |

**Returns:** a multi-line string with the message surrounded by `=` characters.

---

Back to [README](../test-readme.md).

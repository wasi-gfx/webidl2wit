# `webidl2wit` - WebIDL to wit converter

> [!WARNING]
> This project is a Work In Progress ("WIP")

This project converts [WebIDL][webidl] to [WebAssembly Interface Types ("WIT")][wit], enabling WebAssembly components
to interact with interfaces (most importantly browser primitives) defined via WebIDL.

[webidl]: https://developer.mozilla.org/en-US/docs/Glossary/WebIDL
[wit]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md

## Usage

`webidl2wit` can be used as a library from your project:

```toml
webidl2wit = "0.1.0"
```

`webidl2wit` can also be used as a CLI, via the workspace [member project `webidl2wit-cli`](./crates/webidl2wit-cli`):

```console
cargo install webidl2wit-cli
```

## Type conversions

This section lists the type conversions between WebIDL and WIT, as implemented by `webidl2wit`:

| WebIDL type                     | wit type                  |
|:--------------------------------|:--------------------------|
| `any`                           | ❓                        |
| `undefined`                     | just leave it out         |
| `boolean`                       | `bool`                    |
| `byte`                          | `s8`                      |
| `octet`                         | `u8`                      |
| `short`                         | `s16`                     |
| `unsigned short`                | `u16`                     |
| `long`                          | `s32`                     |
| `unsigned long`                 | `u32`                     |
| `long long`                     | `s64`                     |
| `unsigned long long`            | `u64`                     |
| `float`                         | `f32`                     |
| `unrestricted float`            | `f32`                     |
| `double`                        | `f64`                     |
| `unrestricted double`           | `f64`                     |
| `bigint`                        | ❓                        |
| `DOMString`                     | `string`                  |
| `ByteString`                    | `string`                  |
| `USVString`                     | `string`                  |
| `object`                        | ❓                        |
| `symbol`                        | ❓                        |
| `interface`                     | `resource` or `interface` |
| `callback interface`            | ❓                        |
| `dictionary`                    | `record`                  |
| `enum`                          | `enum`                    |
| `callback`                      | `func`                    |
| `constructor`                   | `constructor`             |
| `T?` (optional)                 | `option<T>`               |
| `sequence<T>`                   | `list<T>`                 |
| `record<K, V>`                  | A custom resource         |
| `Promise<T>`                    | ❓                        |
| `or` (union)                    | `variant`                 |
| Buffer types (e.g. `Int8Array`) | A custom resource         |
| `FrozenArray<T>`                | `list<T>`                 |
| `ObservableArray<T>`            | ❓                        |
| `setlike<T>`                    | Add set methods           |
| `maplike<K, V>`                 | ❓                        |
| `namespace`                     | `resource` or `interface` |
| `typedef`                       | `type`                    |
| `const`                         | A getter                  |
| `EventHandler`                  | `stream`                  |

❓ = Not yet sure how to deal with this type.

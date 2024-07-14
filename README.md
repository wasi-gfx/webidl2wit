# WIP WebIDL to wit convertor.


### Conversions of types from WebIDL to wit.

| WebIDL type                     | wit type              |
|:--------------------------------|:----------------------|
| `any`                           | ❓                    |
| `undefined`                     | just leave it out     |
| `boolean`                       | `bool`                |
| `byte`                          | `s8`                  |
| `octet`                         | `u8`                  |
| `short`                         | `s16`                 |
| `unsigned short`                | `u16`                 |
| `long`                          | `s32`                 |
| `unsigned long`                 | `u32`                 |
| `long long`                     | `s64`                 |
| `unsigned long long`            | `u64`                 |
| `float`                         | `f32`                 |
| `unrestricted float`            | `f32`                 |
| `double`                        | `f64`                 |
| `unrestricted double`           | `f64`                 |
| `bigint`                        | ❓                    |
| `DOMString`                     | `string`              |
| `ByteString`                    | `string`              |
| `USVString`                     | `string`              |
| `object`                        | ❓                    |
| `symbol`                        | ❓                    |
| `interface`                     | `resource`            |
| `callback interface`            | ❓                    |
| `dictionary`                    | `record`              |
| `enum`                          | `enum`                |
| `callback`                      | `func`                |
| `constructor`                   | `constructor`         |
| `T?` (optional)                 | `option<T>`           |
| `sequence<T>`                   | `list<T>`             |
| `record<K, V>`                  | A custom resource     |
| `Promise<T>`                    | ❓                    |
| `or` (union)                    | `variant`             |
| Buffer types (e.g. `Int8Array`) | A custom resource     |
| `FrozenArray<T>`                | `list<T>`             |
| `ObservableArray<T>`            | ❓                    |
| `setlike<T>`                    | Add set methods       |
| `maplike<K, V>`                 | ❓                    |
| `namespace`                     | ❓                    |
| `typedef`                       | `type`                |

❓ = Not yet sure how to deal with this type.

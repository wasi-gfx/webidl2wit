# WIP WebIDL to wit conversion.
This code was written in a hacky way, plans to improve it soon.

Side effects of reading this code may include facepalming, mouth drops, and a strong desire to rewrite all of it. Proceed at your own risk.


## Conversions from WebIDL to wit types.
> **_NOTE:_** This does not represent what's currently implemented, this only shows future plans.

If you have any thoughts, please use the [discussion about this table](https://github.com/MendyBerger/webidl-wit/discussions/1) to chime in.

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
| `float`                         | `float32`             |
| `unrestricted float`            | `float32`             |
| `double`                        | `float64`             |
| `unrestricted double`           | `float64`             |
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
| `T?` (optional)                 | `option<T>`           |
| `sequence<T>`                   | `list<T>`             |
| `record<K, V>`                  | ❓                    |
| `Promise<T>`                    | ❓                    |
| `or` (union)                    | ❓ (maybe `enum`?)    |
| Buffer types (e.g. `Int8Array`) | ❓                    |
| `FrozenArray<T>`                | ❓ (maybe `list<T>`?) |
| `ObservableArray<T>`            | ❓                    |
| `setlike<T>`                    | ❓                    |
| `maplike<K, V>`                 | ❓                    |
| `namespace`                     | ❓                    |
| `typedef`                       | `type`                |

❓ = Not yet sure how to deal with this type.

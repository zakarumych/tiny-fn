# TinyFn crate

[![crates](https://img.shields.io/crates/v/tiny-fn.svg?style=for-the-badge&label=tiny-fn)](https://crates.io/crates/tiny-fn)
[![docs](https://img.shields.io/badge/docs.rs-tiny--fn-66c2a5?style=for-the-badge&labelColor=555555&logoColor=white)](https://docs.rs/tiny-fn)
[![actions](https://img.shields.io/github/workflow/status/zakarumych/tiny-fn/badge/master?style=for-the-badge)](https://github.com/zakarumych/tiny-fn/actions?query=workflow%3ARust)
[![MIT/Apache](https://img.shields.io/badge/license-MIT%2FApache-blue.svg?style=for-the-badge)](COPYING)
![loc](https://img.shields.io/tokei/lines/github/zakarumych/tiny-fn?style=for-the-badge)

# Motivation

Did you ever boxed closure and wondered:
"Is there a crate to avoid heap allocations for small closures?"

Well, wonder no more, this is the crate.

# How to use

This crate provides declarative macro [`tiny_fn!`] which generates closure wrappers
to store closures without original closure type.
Generated closure wrappers avoid heap allocations when wrapped closure fits inline storage.

The macro is designed to be easy to write with simple syntax that mostly reuse constructs existing in Rust.
Behavior of generated wrappers should be obvious from the first glance.

# Example

```rust
tiny_fn! {
    struct Foo = Fn(a: i32, b: i32) -> i32;
}
```

Macro expands to `struct Foo` definition with two public methods.

* `Foo::new` accepts any value that implements `Fn(i32, i32) -> i32` and returns new instance of `Foo`.
* `Foo::call` follows signature specified to the macro. e.g. `Foo::call` accepts `a: i32` and `b: i32` and returns `i32`.\
  Plainly `Foo::call` calls closure from which this instance of `Foo` was crated using `a` and `b` arguments at the same positions.

[`tiny_fn!`] macro supports defining multiple items at once.

```rust
tiny_fn! {
    struct Foo = Fn(a: i32, b: i32) -> i32;
    struct Bar = Fn() -> String;
}
```

# Visibility

[`tiny_fn!`] macro supports visibility qualifiers.


```rust
tiny_fn! {
    pub struct Foo = Fn(a: i32, b: i32) -> i32;
    struct Bar = Fn() -> String;
    pub(crate) struct Baz = Fn();
}
```

# Attributes

[`tiny_fn!`] macro supports item attributes, including documentation.

```rust
tiny_fn! {
    /// This is `Foo` wrapper for that takes two `i32`s and return `i32`.
    pub struct Foo = Fn(a: i32, b: i32) -> i32;
}
```

# Fn* traits family

[`tiny_fn!`] macro can generate closure wrappers for any of the `Fn*` traits family.

```rust
tiny_fn! {
    struct A = Fn();
    struct B = FnMut();
    struct C = FnOnce();
}
```

* `A` can wrap only closures that are callable when immutably borrowed. And so `A::call` takes `&self`.
* `B` can wrap only closures that are callable when borrowed. And so `B::call` takes `&mut self`.
* `C` can wrap any closures, even ones that are callable once. And so `C::call` takes `self`.

# Generics

Closure wrappers can be declared generic over number of types and those types should be used in function signature.

```rust
tiny_fn! {
    struct BinOp<T> = Fn(a: T, b: T) -> T;
}
```

Here `BinOp` is generic over `T`.\
`BiOp::<T>::new` accepts closures bounds by `Fn(T, T) -> T`.

Notably `T` is not constrained by traits in `BinOp`.\
Closure wrappers only move arguments and return values, so they don't need to know anything else about the type.

# Special generic parameters

Closure wrapper generated by [`tiny_fn!`] macro always have two generic parameters besides generic types specified by macro caller:
* Lifetime `'closure`.\
  Wrapper contains closures bound by `'closure` lifetime.
* Constant `INLINE_SIZE: usize`.\
  Closures with size up to `INLINE_SIZE` and alignment requirement not exceeding [`tiny_fn::ALIGN`] will be inlined into wrapper structure directly.\
  Otherwise heap allocation will occur.\
  `INLINE_SIZE` parameter is defaulted to `tiny_fn::DEFAULT_INLINE_SIZE` which is `24` currently.


[`tiny_fn!`]: https://docs.rs/tiny-fn/latest/tiny_fn/macro.tiny_fn.html
[`tiny_fn::ALIGN`]: https://docs.rs/tiny-fn/latest/tiny_fn/constant.ALIGN.html
[`tiny_fn::DEFAULT_INLINE_SIZE`]: https://docs.rs/tiny-fn/latest/tiny_fn/constant.DEFAULT_INLINE_SIZE.html

## License

Licensed under either of

* Apache License, Version 2.0, ([license/APACHE](license/APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([license/MIT](license/MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

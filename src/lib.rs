//!
//! Have you ever being placing closure into [`Box<dyn Fn(...)>`] and wondered:
//! "Is there a crate to avoid heap allocations for small closures?"
//!
//! Wonder no more, this is the crate.
//!
//! # How to use
//!
//! This crate provides declarative macro [`tiny_fn!`] to generate closure wrappers
//! able store closure erasing its type.
//!
//! Generated closure wrappers avoid heap allocations when wrapped closure fits inline storage.
//!
//! The macro is designed to be easy to write with simple syntax that mostly reuse constructs already existing in Rust.\
//! Behavior of generated wrappers should be obvious from the first glance.
//!
//! # Example
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     struct Foo = Fn(a: i32, b: i32) -> i32;
//! }
//! ```
//!
//! Macro expands to `struct Foo` definition with two public methods.
//!
//! * `Foo::new` accepts any value that implements [`Fn(i32, i32) -> i32`] and returns new instance of `Foo`.
//! * `Foo::call` follows signature specified to the macro. e.g. `Foo::call` accepts `a: i32` and `b: i32` and returns [`i32`].\
//!   Plainly `Foo::call` calls closure from which this instance of `Foo` was crated using `a` and `b` arguments at the same positions.
//!
//! [`tiny_fn!`] macro supports defining multiple items at once.
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     struct Foo = Fn(a: i32, b: i32) -> i32;
//!     struct Bar = Fn() -> String;
//! }
//! ```
//!
//! # Visibility
//!
//! [`tiny_fn!`] macro supports visibility qualifiers.
//!
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     pub struct Foo = Fn(a: i32, b: i32) -> i32;
//!     struct Bar = Fn() -> String;
//!     pub(crate) struct Baz = Fn();
//! }
//! ```
//!
//! # Attributes
//!
//! [`tiny_fn!`] macro supports item attributes, including documentation.
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     /// This is `Foo` wrapper for that takes two `i32`s and return `i32`.
//!     pub struct Foo = Fn(a: i32, b: i32) -> i32;
//! }
//! ```
//!
//! # [`Fn*`] traits family
//!
//! [`tiny_fn!`] macro can generate closure wrappers for any of the [`Fn*`] traits family.
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     struct A = Fn();
//!     struct B = FnMut();
//!     struct C = FnOnce();
//! }
//! ```
//!
//! * `A` can wrap only closures that are callable when immutably borrowed. And so `A::call` takes `&self`.
//! * `B` can wrap only closures that are callable when borrowed. And so `B::call` takes `&mut self`.
//! * `C` can wrap any closures, even ones that are callable once. And so `C::call` takes `self`.
//!
//! # Generics
//!
//! Closure wrappers can be declared generic over number of types and those types should be used in function signature.
//!
//! ```
//! # use tiny_fn::tiny_fn;
//! tiny_fn! {
//!     struct BinOp<T> = Fn(a: T, b: T) -> T;
//! }
//! ```
//!
//! Here `BinOp` is generic over `T`.\
//! `BiOp::<T>::new` accepts closures bounds by [`Fn(T, T) -> T`].
//!
//! Notably `T` is not constrained by traits in `BinOp`.\
//! Closure wrappers only move arguments and return values, so they don't need to know anything else about the type.
//!
//! # Special generic parameters
//!
//! Closure wrapper generated by [`tiny_fn!`] macro always have two generic parameters besides generic types specified by macro caller:
//! * Lifetime `'closure`.\
//!   Wrapper contains closures bound by `'closure` lifetime.
//! * Constant `INLINE_SIZE: usize`.\
//!   Closures with size up to `INLINE_SIZE` and alignment requirement not exceeding [`tiny_fn::ALIGN`] will be inlined into wrapper structure directly.\
//!   Otherwise heap allocation will occur.\
//!   `INLINE_SIZE` parameter is defaulted to [`tiny_fn::DEFAULT_INLINE_SIZE`].
//!
//! [`Box<dyn Fn(...)>`]: https://doc.rust-lang.org/std/ops/trait.Fn.html
//! [`Fn(i32, i32) -> i32`]: https://doc.rust-lang.org/std/ops/trait.Fn.html
//! [`Fn*`]: https://doc.rust-lang.org/std/ops/trait.Fn.html
//! [`Fn(T, T) -> T`]: https://doc.rust-lang.org/std/ops/trait.Fn.html
//! [`tiny_fn::ALIGN`]: `ALIGN`
//! [`tiny_fn::DEFAULT_INLINE_SIZE`]: `DEFAULT_INLINE_SIZE`

#![no_std]

extern crate alloc;

/// Inline storage alignment.
/// Closures with stricter alignment requirements will be heap allocated by wrapper
/// even if size fits.
pub const ALIGN: usize = core::mem::align_of::<crate::private::InlineStorage<1>>();

/// Default value for `INLINE_SIZE` parameter of closure wrappers generated by [`tiny_fn!`] macro.
pub const DEFAULT_INLINE_SIZE: usize = 24;

/// Content of this module is not public API.
/// It is used by macro output.
#[doc(hidden)]
pub mod private {
    pub use alloc::boxed::Box;
    pub use core::{
        mem::{align_of, size_of, transmute, ManuallyDrop, MaybeUninit},
        ptr::{copy_nonoverlapping, drop_in_place, read, NonNull},
    };

    pub trait Closure {
        type Inner;
    }

    pub struct VTable<D, C> {
        pub drop: D,
        pub call: C,
    }

    #[derive(Clone, Copy)]
    #[allow(dead_code)]
    pub union Element {
        pub integer: usize,
        pub pointer: *mut u8,
    }

    #[repr(align(16))]
    pub struct InlineStorage<const INLINE_SIZE: usize> {
        pub bytes: [core::mem::MaybeUninit<u8>; INLINE_SIZE],
    }

    pub type DropFn<const INLINE_SIZE: usize> =
        unsafe fn(core::ptr::NonNull<[core::mem::MaybeUninit<u8>; INLINE_SIZE]>);

    #[repr(C)]
    pub struct InlineFn<const INLINE_SIZE: usize> {
        pub vtable: &'static VTable<DropFn<INLINE_SIZE>, DropFn<INLINE_SIZE>>,
        pub storage: InlineStorage<INLINE_SIZE>,
    }

    impl<const INLINE_SIZE: usize> Drop for InlineFn<INLINE_SIZE> {
        fn drop(&mut self) {
            unsafe {
                let drop_fn = self.vtable.drop;
                (drop_fn)(core::ptr::NonNull::from(&mut self.storage.bytes));
            }
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! private_tiny_fn {
    (@call Fn $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        pub fn call(&self, $($arg_name: $arg_type),*) $(-> $ret)? {
            unsafe {
                if self.boxed_if_zero == 0 {
                    (*self.boxed.closure)($($arg_name),*)
                } else {
                    let call_fn: CallFn< $($($t,)+)? INLINE_SIZE> = $crate::private::transmute(self.inline.vtable.call);
                    call_fn(
                        $crate::private::NonNull::from(&self.inline.storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };

    (@call FnMut $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        pub fn call(&mut self, $($arg_name: $arg_type),*) $(-> $ret)? {
            unsafe {
                if self.boxed_if_zero == 0 {
                    (*(*self.boxed).closure)($($arg_name),*)
                } else {
                    let call_fn: CallFn< $($($t,)+)? INLINE_SIZE> = $crate::private::transmute(self.inline.vtable.call);
                    call_fn(
                        $crate::private::NonNull::from(&mut (*self.inline).storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };

    (@call FnOnce $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        pub fn call(self, $($arg_name: $arg_type),*) $(-> $ret)? {
            let mut me = $crate::private::ManuallyDrop::new(self);
            unsafe {
                if me.boxed_if_zero == 0 {
                    ($crate::private::ManuallyDrop::take(&mut me.boxed).closure)($($arg_name),*)
                } else {
                    let call_fn: CallFn< $($($t,)+)? INLINE_SIZE> = $crate::private::transmute(me.inline.vtable.call);
                    call_fn(
                        $crate::private::NonNull::from(&mut (*(*me).inline).storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };


    (@call_outer Fn $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        /// Calls wrapped closure
        /// and returns it result.
        #[allow(dead_code)]
        pub fn call(&self, $($arg_name: $arg_type),*) $(-> $ret)? {
            self.inner.call($($arg_name,)*)
        }
    };

    (@call_outer FnMut $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        /// Calls wrapped closure
        /// and returns it result.
        #[allow(dead_code)]
        pub fn call(&mut self, $($arg_name: $arg_type),*) $(-> $ret)? {
            self.inner.call($($arg_name,)*)
        }
    };

    (@call_outer FnOnce $(< $($t:ident),+ >)? ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        /// Calls wrapped closure
        /// and returns it result.
        #[allow(dead_code)]
        pub fn call(self, $($arg_name: $arg_type),*) $(-> $ret)? {
            self.inner.call($($arg_name,)*)
        }
    };

    (@inline_call_cast Fn $ptr:ident) => {
        (*$ptr.cast::<F>().as_ptr())
    };

    (@inline_call_cast FnMut $ptr:ident) => {
        (*$ptr.cast::<F>().as_ptr())
    };

    (@inline_call_cast FnOnce $ptr:ident) => {
        $crate::private::read($ptr.cast::<F>().as_ptr())
    };
}

/// Defines new structure type.
/// This type will be constructible from any closure
/// that implement specified [`Fn*`] trait.
/// Closures that fits into inline storage will be placed there.
/// Otherwise closure will be boxed.
/// And it will have `call` method with the same signature as the closure.
///
/// Defined type can have generic parameters that can be used in function signature.
/// It will always have additional `const INLINE_SIZE: usize` generic parameter,
/// that controls size of the inline storage. Alignment of the inline storage is hardcoded to 16
/// which is enough for any primitive type and thus fits most of the types.
///
/// [`Fn*`]: https://doc.rust-lang.org/std/ops/trait.Fn.html
#[macro_export]
macro_rules! tiny_fn {
    ($(
        $(#[$meta:meta])*
        $vis:vis struct $name:ident $(< $($t:ident),+ >)? =
        $fun:ident ($(
            $arg_name:ident: $arg_type:ty
        ),* $(,)?)
        $( -> $ret:ty)?;
    )*) => {
        $(
            const _: () = {
                type CallFn< $($($t, )+)? const INLINE_SIZE: usize> = unsafe fn($crate::private::NonNull<[$crate::private::MaybeUninit<u8>; INLINE_SIZE]>, $($arg_type),*) $( -> $ret)?;

                #[repr(C)]
                struct BoxedFn <'closure $($(, $t)+)?> {
                    zero: usize,
                    closure: $crate::private::Box<dyn $fun($($arg_type),*) $(-> $ret)? + 'closure>,
                }

                #[doc(hidden)]
                pub union TinyClosure<'closure, $($($t,)+)? const INLINE_SIZE: usize> {
                    inline: $crate::private::ManuallyDrop<$crate::private::InlineFn<INLINE_SIZE>>,
                    boxed: $crate::private::ManuallyDrop<BoxedFn<'closure $($(, $t)+)?>>,
                    boxed_if_zero: usize,
                }

                impl<'closure, $($($t,)+)? const INLINE_SIZE: usize> Drop for TinyClosure<'closure, $($($t,)+)? INLINE_SIZE> {
                    fn drop(&mut self) {
                        unsafe {
                            if self.boxed_if_zero == 0 {
                                $crate::private::ManuallyDrop::drop(&mut self.boxed);
                                return;
                            } else {
                                $crate::private::ManuallyDrop::drop(&mut self.inline);
                            }
                        }
                    }
                }

                impl<'closure, $($($t,)+)? const INLINE_SIZE: usize> TinyClosure<'closure, $($($t,)+)? INLINE_SIZE> {
                    fn new<F>(f: F) -> Self
                    where
                        F: $fun ($($arg_type),*) $( -> $ret)? + 'closure,
                    {
                        let size_fits = $crate::private::size_of::<F>() <= INLINE_SIZE;
                        let align_fits = $crate::private::align_of::<F>() <= $crate::ALIGN;

                        if size_fits && align_fits {
                            let mut storage = $crate::private::InlineStorage {
                                bytes: [$crate::private::MaybeUninit::uninit(); INLINE_SIZE],
                            };

                            let f = $crate::private::ManuallyDrop::new(f);
                            unsafe {
                                $crate::private::copy_nonoverlapping(
                                    &*f,
                                    storage.bytes.as_mut_ptr() as *mut F,
                                    1,
                                );
                            }

                            let inline_fn = $crate::private::InlineFn {
                                vtable: unsafe {
                                    $crate::private::transmute(&$crate::private::VTable::< $crate::private::DropFn<INLINE_SIZE>, CallFn< $($($t,)+)? INLINE_SIZE>> {
                                        drop: |ptr: $crate::private::NonNull<[$crate::private::MaybeUninit<u8>; INLINE_SIZE]>| {
                                            $crate::private::drop_in_place(ptr.cast::<F>().as_ptr());
                                        },
                                        call: |ptr: $crate::private::NonNull<[$crate::private::MaybeUninit<u8>; INLINE_SIZE]> $(, $arg_name: $arg_type)*| $(-> $ret)? {
                                            $crate::private_tiny_fn!(@inline_call_cast $fun ptr)($($arg_name),*)
                                        },
                                    })
                                },
                                storage,
                            };
                            TinyClosure {
                                inline: $crate::private::ManuallyDrop::new(inline_fn),
                            }
                        } else {
                            let boxed_fn = BoxedFn {
                                zero: 0,
                                closure: $crate::private::Box::new(f),
                            };

                            TinyClosure {
                                boxed: $crate::private::ManuallyDrop::new(boxed_fn),
                            }
                        }
                    }

                    $crate::private_tiny_fn!(@call $fun $(< $($t),+ >)? ($($arg_name: $arg_type),*) $( -> $ret)?);
                }

                impl<'closure, $($($t,)+)? const INLINE_SIZE: usize> $crate::private::Closure for $name<'closure, $($($t,)+)? INLINE_SIZE> {
                    type Inner = TinyClosure<'closure, $($($t,)+)? INLINE_SIZE>;
                }
            };

            #[repr(transparent)]
            $(#[$meta])*
            $vis struct $name<'closure, $($($t,)+)? const INLINE_SIZE: usize = { $crate::DEFAULT_INLINE_SIZE }> {
                #[allow(dead_code)]
                inner: < $name<'closure, $($($t,)+)? INLINE_SIZE> as $crate::private::Closure>::Inner,
            }

            impl<'closure, $($($t,)+)? const INLINE_SIZE: usize> $name<'closure, $($($t,)+)? INLINE_SIZE> {
                #[inline]
                pub fn new<F>(f: F) -> Self
                where
                    F: $fun ($($arg_type),*) $( -> $ret)? + 'closure,
                {
                    $name {
                        inner: << $name< $($($t,)+)? INLINE_SIZE> as $crate::private::Closure>::Inner>::new(f),
                    }
                }

                $crate::private_tiny_fn!(@call_outer $fun $(< $($t),+ >)? ($($arg_name: $arg_type),*) $( -> $ret)?);
            }
        )*
    };
}

#[cfg(feature = "example")]
pub mod example {
    //! This module contains a few example declarations with [`tiny_fn!`] macro.\
    //! And their usage in the section below.
    //!
    //! # Usage
    //!
    //! ```
    //! # extern crate alloc;
    //! # use tiny_fn::example::*;
    //! # use alloc::{borrow::ToOwned, format};
    //! #
    //! let binop: BinOp::<i32, 24> = BinOp::new(|a, b| a + b);
    //! assert_eq!(3, binop.call(1, 2));
    //!
    //! let a = "Hello".to_owned();
    //! let b = "world".to_owned();
    //! let make_string = MakeString::<32>::new(move || format!("{a} {b}!"));
    //!
    //! assert_eq!(make_string.call(), "Hello world!");
    //! ```

    tiny_fn! {
        /// Contains any binary operation for a specific type.
        /// That takes two values of that type
        /// and returns value of that type.
        pub struct BinOp<T> = Fn(a: T, b: T) -> T;

        pub struct MakeString = Fn() -> alloc::string::String;
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_foo() {
        tiny_fn! {
            pub(crate) struct Foo<T> = FnMut(a: &T, b: T) -> T;
            pub struct Bar = FnOnce(b: u8) -> alloc::string::String;
        }

        let mut x = 3;
        let mut foo = Foo::<u8>::new(|a, b| {
            x += a + b;
            x
        });
        let bar: Bar<0> = Bar::new(|b| alloc::format!("{}", b));

        assert_eq!(6, foo.call(&1, 2));
        assert_eq!(13, foo.call(&3, 4));

        assert_eq!("3", bar.call(3));
    }
}

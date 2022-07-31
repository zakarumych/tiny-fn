//!
//! This crate provides macro to generate tiny closure types.
//! Tiny closure is capable of storing dynamically typed closures up to certain size without heap allocations.

#[doc(hidden)]
pub trait Closure {
    type Inner;
}

#[macro_export]
macro_rules! tiny_fn {
    (@call Fn ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        fn call(&self, $($arg_name: $arg_type),*) $(-> $ret)? {
            unsafe {
                if self.inner.boxed_if_zero == 0 {
                    (*self.inner.boxed.closure)($($arg_name),*)
                } else {
                    (self.inner.inline.vtable.call)(
                        core::ptr::NonNull::from(&self.inner.inline.storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };
    (@call FnMut ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        fn call(&mut self, $($arg_name: $arg_type),*) $(-> $ret)? {
            unsafe {
                if self.inner.boxed_if_zero == 0 {
                    (*(*self.inner.boxed).closure)($($arg_name),*)
                } else {
                    (self.inner.inline.vtable.call)(
                        core::ptr::NonNull::from(&self.inner.inline.storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };
    (@call FnOnce ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        fn call(self, $($arg_name: $arg_type),*) $(-> $ret)? {
            let mut me = core::mem::ManuallyDrop::new(self);
            unsafe {
                if me.inner.boxed_if_zero == 0 {
                    (core::mem::ManuallyDrop::take(&mut me.inner.boxed).closure)($($arg_name),*)
                } else {
                    (me.inner.inline.vtable.call)(
                        core::ptr::NonNull::from(&mut (*(*me).inner.inline).storage.bytes),
                        $($arg_name),*
                    )
                }
            }
        }
    };


    (@inline_call_cast Fn $ptr:ident) => {
        (*$ptr.cast::<F>().as_ptr())
    };

    (@inline_call_cast FnMut $ptr:ident) => {
        (*$ptr.cast::<F>().as_ptr())
    };

    (@inline_call_cast FnOnce $ptr:ident) => {
        core::ptr::read($ptr.cast::<F>().as_ptr())
    };

    ($name:ident $fun:ident ($($arg_name:ident: $arg_type:ty),* $(,)?) $( -> $ret:ty)?) => {
        const _: () = {
            struct VTable<const N: usize> {
                call: unsafe fn(core::ptr::NonNull<[u8; N]>, $($arg_type),*) $( -> $ret)?,
                drop: unsafe fn(core::ptr::NonNull<[u8; N]>),
            }

            #[repr(align(16))]
            struct InlineStorage<const N: usize> {
                bytes: [u8; N],
            }

            #[repr(C)]
            struct InlineFn<const N: usize> {
                vtable: &'static VTable<N>,
                storage: InlineStorage<N>,
            }

            impl<const N: usize> Drop for InlineFn<N> {
                fn drop(&mut self) {
                    unsafe {
                        (self.vtable.drop)(core::ptr::NonNull::from(&mut self.storage.bytes));
                    }
                }
            }

            #[repr(C)]
            struct BoxedFn {
                zero: usize,
                closure: Box<dyn $fun($($arg_type),*) $(-> $ret)?>,
            }

            #[doc(hidden)]
            pub union TinyClosure<const N: usize> {
                inline: core::mem::ManuallyDrop<InlineFn<N>>,
                boxed: core::mem::ManuallyDrop<BoxedFn>,
                boxed_if_zero: usize,
            }

            impl<const N: usize> Drop for TinyClosure<N> {
                fn drop(&mut self) {
                    unsafe {
                        if self.boxed_if_zero == 0 {
                            core::mem::ManuallyDrop::drop(&mut self.boxed);
                            return;
                        } else {
                            core::mem::ManuallyDrop::drop(&mut self.inline);
                        }
                    }
                }
            }

            impl<const N: usize> $crate::Closure for $name<N> {
                type Inner = TinyClosure<N>;
            }

            impl<const N: usize> TinyClosure<N> {
                fn new<F>(f: F) -> Self
                where
                    F: $fun ($($arg_type),*) $( -> $ret)? + 'static,
                {
                    if core::mem::size_of::<F>() <= N && core::mem::align_of::<F>() <= 16 {
                        let mut storage = InlineStorage {
                            bytes: [0; N],
                        };

                        let f = core::mem::ManuallyDrop::new(f);
                        unsafe {
                            core::ptr::copy_nonoverlapping(
                                &*f as *const F as *const u8,
                                storage.bytes.as_mut_ptr(),
                                core::mem::size_of::<F>(),
                            );
                        }

                        let inline_fn = InlineFn {
                            vtable: &VTable {
                                drop: |ptr: core::ptr::NonNull<[u8; N]>| unsafe {
                                    core::ptr::drop_in_place(ptr.cast::<F>().as_ptr());
                                },
                                call: |ptr: core::ptr::NonNull<[u8; N]>, $($arg_name: $arg_type),*| $(-> $ret)? { unsafe {
                                    $crate::tiny_fn!(@inline_call_cast $fun ptr)($($arg_name),*)
                                }},
                            },
                            storage,
                        };

                        TinyClosure {
                            inline: core::mem::ManuallyDrop::new(inline_fn),
                        }
                    } else {
                        let boxed_fn = BoxedFn {
                            zero: 0,
                            closure: Box::new(f),
                        };

                        TinyClosure {
                            boxed: core::mem::ManuallyDrop::new(boxed_fn),
                        }
                    }
                }

            }
        };

        #[repr(transparent)]
        pub struct $name<const N: usize> {
            inner: <$name<N> as $crate::Closure>::Inner,
        }

        impl<const N: usize> $name<N> {
            pub fn new<F>(f: F) -> Self
            where
                F: $fun ($($arg_type),*) $( -> $ret)? + 'static,
            {
                $name {
                    inner: <<$name<N> as $crate::Closure>::Inner>::new(f),
                }
            }

            $crate::tiny_fn!(@call $fun ($($arg_name: $arg_type),*) $( -> $ret)?);
        }
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_foo() {
        tiny_fn!(Foo FnOnce(a: u8, b: u8) -> u8);
        tiny_fn!(Bar Fn(b: u8) -> String);

        let x = 3;
        let foo = Foo::<24>::new(move |a, b| x + a + b);
        let bar = Bar::<0>::new(move |b| format!("{}", b));

        assert_eq!(6, foo.call(1, 2));
        assert_eq!("3", bar.call(3));
    }
}

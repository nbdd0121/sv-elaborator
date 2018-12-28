use std::mem;
use std::ptr;

/// In some cases we will need to replace a mutable reference but are unable to use
/// `std::mem::replace` because we need the old value to create the new value.
pub fn replace_with<T>(val: &mut T, f: impl FnOnce(T)->T) {
    unsafe {
        let mut value: T = mem::uninitialized();
        ptr::copy_nonoverlapping(val, &mut value, 1);
        // TODO This is not exception safe!
        value = f(value);
        ptr::copy_nonoverlapping(&value, val, 1);
        mem::forget(value);
    }
}

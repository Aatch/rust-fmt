// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use parse::*;
use core::io::Writer;

pub static printf_desc : ParserDesc<'static> = ParserDesc {
    indicator: '%',
    special_flag: Some('!'),
    flags: ['#', '0', '-', ' ', '+', '\''],
    specifiers: ['d', 'f', 's', '?']
};

// unfortunately the cruft in io means not using @Writer is hard, this
// should change when io gets redone
pub trait Formatter {
    fn format(&self, @Writer, Spec);
}

pub trait IntFormat {
    fn format_d(&self, @Writer, Spec);
}

pub trait FloatFormat {
    fn format_f(&self, @Writer, Spec);
}

pub trait StrFormat {
    fn format_s(&self, @Writer, Spec);
}

// completely ignores the spec, and just calls to_str
macro_rules! naive_impl{
    ($trt:ident, $mthd:ident, $ty:ty) => {
        impl $trt for $ty {
            fn $mthd(&self, w: @Writer, _s: Spec) {
                w.write_str(self.to_str());
            }
        }
    }
}
macro_rules! df{ ($ty:ty) => { naive_impl!(IntFormat, format_d, $ty) } }
macro_rules! ff{ ($ty:ty) => { naive_impl!(FloatFormat, format_f, $ty) } }
macro_rules! sf{ ($ty:ty) => { naive_impl!(StrFormat, format_s, $ty) } }

df!{int}  df!{i8} df!{i16} df!{i32} df!{i64}
df!{uint} df!{u8} df!{u16} df!{u32} df!{u64}

ff!{float} ff!{f32} ff!{f64}

// needs char::to_str
// sf!{char}
impl StrFormat for char {
    fn format_s(&self, w: @Writer, _s: Spec) {
        w.write_char(*self)
    }
}
impl<'self> StrFormat for &'self str {
    fn format_s(&self, w: @Writer, _s: Spec) {
        w.write_str(*self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parse::*;

    #[test]
    fn test_int() {
        let spec = Spec {
            specifier: 'd',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert_eq!(
                    io::with_str_writer(|w| $tst.format_d(w, copy spec)),
                    ~"1")
            }
        );
        t!(1i);
        t!(1i8);
        t!(1i16);
        t!(1i32);
        t!(1i64);

        t!(1u);
        t!(1u8);
        t!(1u16);
        t!(1u32);
        t!(1u64);
    }

    #[test]
    fn test_float() {
        let spec = Spec {
            specifier: 'f',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert!(
                    str::starts_with(
                        io::with_str_writer(|w| $tst.format_f(w, copy spec)),
                        "1.25"))
            }
        );
        t!(1.25f);
        t!(1.25f32);
        t!(1.25f64);
    }

    #[test]
    fn test_str() {
        let spec = Spec {
            specifier: 's',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert_eq!(
                    io::with_str_writer(|w| $tst.format_s(w, copy spec)),
                    ~"x");
            }
        );
        t!("x");
        t!('x');
    }
}

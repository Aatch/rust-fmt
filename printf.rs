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

pub trait Formatter {
    fn format(&self, &Writer, Spec);
}

pub trait IntFormat {
    fn format_d(&self, &Writer, Spec);
}

pub trait FloatFormat {
    fn format_f(&self, &Writer, Spec);
}

pub trait StrFormat {
    fn format_s(&self, &Writer, Spec);
}

// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
 * Various formatting and format string handling.
 */

/// Result of parsing an item. See Parser::parse_item for details
pub enum ParseResult<'self> {
    Place(Spec),
    Raw(&'self str),
    Error{msg:~str, pos:uint},
    End,
}

/// Field description
#[deriving(Eq)]
pub enum Field {
    /// No field given in placeholder
    None,
    /// Field is specific value
    Value(int),
    /// Field value is the next argument
    Next,
    /// Field value is in the nth argument
    Arg(int),
}

/**
 * Specifier for a parsed placeholder
 */
pub struct Spec {
    specifier: char,
    position: Option<int>,
    width: Field,
    precision: Field,
    flags: ~[char],
    special_flag: bool,
    num_arg: Option<int>,
}

pub struct ParserDesc<'self> {
    indicator: char,
    special_flag: Option<char>,
    flags: &'self [char],
    specifiers: &'self [char]
}

pub struct Parser<'self> {
    desc: &'self ParserDesc<'self>,
    source: &'self str,
    priv pos: uint,
    priv state: State,
}

#[deriving(Eq)]
enum State {
    Position, Flags, Width, Precision, Specifier
}

static forbidden_chars : &'static [char] = &'static [
    '1', '2', '3', '4', '5', '6', '7','8', '9',
    '.', '*', '[', ']' ,'{', '}', '<', '>'
];

/**
 * The format placeholder structure that `Parser` expects is similar to the standard
 * printf/scanf style placeholders, but more generic, allowing for the creation of custom
 * formatting placeholders.
 *
 * A parser is created by providing an indicator character, an optional special flag, a set of flag
 * characters and a set conversion characters. The two sets should be disjoint, but this isn't
 * enforced.
 *
 * The grammar of a format placeholder is as follows:
 *
 *      Placeholder := Indicator SpecialFlag? Position? Flag* Width? Precision? Specifier
 *      Position := '{' '-'? [0-9]+ '}'
 *      Width := [1-9]+ | '[' ('-'? [0-9]+ | '*') ']'
 *      Precision := '.' ([1-9]+ | '[' ('-'? [0-9]+ | '*' ) ']')
 *
 *      SpecialFlag := <given>
 *      Flag := <From supplied set>
 *      Specifier := NumArg? <From supplied set>
 *
 *      NumArg:= '<' '-'? [0-9]+ '>'
 *
 * Invalid character(s) for indicators, flags and specifiers are:
 *      '1'..'9', '{', '}', '[', ']', '<', '>', '*' and '.'.
 * Invalid character(s) for indicators and specifiers are ' '
 *
 * The parser is greedy, so it will only skip to the next section if there is no way to continue on
 * the current section.
 */
pub impl<'self> Parser<'self> {
    /**
     * Creates a new parser following the rules of the given ParserDesc
     */
    pub fn new<'a>(src: &'a str, desc: &'a ParserDesc) -> Parser<'a> {
        assert!(!forbidden_chars.contains(&desc.indicator));

        Parser {
            desc: desc,
            source: src,
            pos: 0,
            state: Position
        }
    }

    /**
     * Returns the next item that the parser gets.
     *
     * The four variants of ParseResult have the following meaning:
     *
     *      Place(Spec)         Parser found a specifier
     *      Raw(&str)           Parser found regular text. Contents is a slice into the source string
     *      Error {msg, pos}    Parser encountered an error described by `msg` at the given position.
     *      End                 Parser encounted the end of the string
     */
    pub fn next_item(&mut self) -> ParseResult<'self> {
        if self.pos == self.source.len() {
            return End;
        }

        if self.cur() == self.desc.indicator {
            self.parse_placeholder()
        } else {
            let old_pos = self.pos;
            self.skip_to_next_indicator();

            let s = self.source.slice(old_pos, self.pos);
            Raw(s)
        }
    }

    /**
     * Parses a placeholder at the current position.
     *
     * See next_item for explaination of the return values
     */
    pub fn parse_placeholder(&mut self) -> ParseResult<'self> {
        let c = self.pop();
        if c != self.desc.indicator {
            return Error {
                msg: fmt!("Unexpected character '%c' found, expected '%c'", c, self.desc.indicator),
                pos: self.pos-1,
            };
        }

        if self.eat(self.desc.indicator) {
            return Raw(self.source.slice(self.pos-1, self.pos));
        }

        let special_flag = match self.desc.special_flag {
            Some(f) if self.eat(f) => true,
            _ => false
        };

        let mut width = None;
        let mut precision = None;

        let position = match self.parse_position() {
            Ok(p) => p,
            Err((msg, pos)) => return Error {msg:msg, pos:pos}
        };

        let flags = match self.parse_flags() {
            Ok(f) => f,
            Err((msg, pos)) => return Error { msg: msg, pos: pos}
        };

        if self.state == Width {
            width = match self.parse_field() {
                Ok(w) => w,
                Err((msg, pos)) => return Error { msg: msg, pos: pos}
            };
            if self.eat('.') {
                self.state = Precision;
            } else {
                self.state = Specifier;
            }
        }

        if self.state == Precision {
            precision = match self.parse_field() {
                Ok(p) => p,
                Err((msg, pos)) => return Error { msg: msg, pos: pos}
            };
            self.state = Specifier;
        }

        let num_arg = match self.parse_num_arg() {
            Ok(s) => s,
            Err((msg, pos)) => return Error { msg: msg, pos: pos}
        };

        let spec = match self.pop_spec() {
            Ok(s) => s,
            Err((msg, pos)) => return Error { msg: msg, pos: pos}
        };

        Place(Spec {
            specifier: spec,
            position: position,
            width: width,
            precision: precision,
            flags: flags,
            special_flag: special_flag,
            num_arg: num_arg
        })
    }

    /**
     * Advances the parser to the start of the next placeholder indicator
     */
    pub fn skip_to_next_indicator(&mut self) {
        let new_pos = str::find_from(self.source, self.pos, |c| c == self.desc.indicator);
        self.pos = new_pos.get_or_default(self.source.len());
    }

    /**
     * Resets the parser, so it starts from beginning of the source string again
     */
    pub fn reset(&mut self) {
        self.pos = 0;
        self.state = Position;
    }

    /**
     * Returns whether or not the parser is at the end of the source string
     */
    pub fn at_end(&self) -> bool {
        self.pos != self.source.len()
    }

    /**
     * Returns the current position of the parser
     */
    pub fn cur_pos(&self) -> uint {
        self.pos
    }

    // Parses a number
    priv fn parse_num(&mut self) -> Option<int> {
        let mut sign = 1;
        let mut val = option::None;
        if self.eat('-') {
            sign = -1;
        }
        loop {
            match self.cur() {
                '0'..'9' => {
                    let d = (self.pop() as int)-0x30;
                    val.mutate_default(d, |v| (v*10)+d);
                }
                _ => {
                    val.mutate(|v| v*sign);
                    break;
                }
            }
        }

        val
    }

    priv fn parse_position(&mut self) -> Result<Option<int>, (~str, uint)> {
        if self.eat('{') {
            match self.parse_num() {
                Some(n) => {
                    do self.expect('}') {
                        Some(n)
                    }
                }
                option::None => Ok(option::None)
            }
        } else {
            Ok(option::None)
        }
    }

    priv fn parse_flags(&mut self) -> Result<~[char], (~str, uint)> {
        let mut flags = ~[];
        loop {
            match self.cur() {
                '1'..'9' | '[' => {
                    self.state = Width;
                    return Ok(flags);
                }
                '.' => {
                    self.pop();
                    self.state = Precision;
                    return Ok(flags);
                }
                f if flags.contains(&f) => {
                    self.state = Specifier;
                    break;
                }
                _ => {
                    match self.pop_flag() {
                        Some(f) => flags.push(f),
                        _ => { //If it was a width or precision, it would have triggered above
                            self.state = Specifier;
                            break;
                        }
                    }
                }
            }
        }

        Ok(flags)
    }

    priv fn parse_field(&mut self) -> Result<Field,(~str, uint)> {
        if self.eat('[') {
            if self.eat('*') {
                do self.expect(']') {
                    Next
                }
            } else {
                match self.parse_num() {
                    Some(n) => {
                        do self.expect(']') {
                            Arg(n)
                        }
                    }
                    option::None =>
                        return Err((fmt!("Unexpected character '%c', expected '0'..'9' or '*'", self.cur()), self.pos))
                }
            }
        } else {
            Ok(match self.parse_num() {
                Some(n) => Value(n),
                option::None => None
            })
        }
    }

    priv fn parse_num_arg(&mut self) -> Result<Option<int>, (~str, uint)> {
        if self.eat('<') {
            match self.parse_num() {
                Some(n) => {
                    do self.expect('>') {
                        Some(n)
                    }
                }
                option::None =>
                    return Err((fmt!("Unexpected character '%c', expected '0'..'9'", self.cur()), self.pos))
            }
        } else {
            Ok(option::None)
        }
    }

    priv fn pop_flag(&mut self) -> Option<char> {
        if self.pos == self.source.len() {
            return option::None;
        }
        let c = self.cur();
        if self.desc.flags.contains(&c) && !forbidden_chars.contains(&c) {
            Some(self.pop())
        } else {
            option::None
        }
    }

    priv fn pop_spec(&mut self) -> Result<char, (~str, uint)> {
        if self.pos == self.source.len() {
            return Err((~"Unexpected end-of-string", self.pos))
        }
        let c = self.cur();
        if c != ' ' && self.desc.specifiers.contains(&c) && !forbidden_chars.contains(&c) {
            Ok(self.pop())
        } else {
            Err((fmt!("Unexpected character '%c' found, expected specifier", c), self.pos))
        }
    }

    // Returns the current character
    priv fn cur(&self) -> char {
        let str::CharRange {ch, _} = str::char_range_at(self.source, self.pos);
        ch
    }

    // Pops the current character off and returns it
    priv fn pop(&mut self) -> char {
        let str::CharRange {ch, next} = str::char_range_at(self.source, self.pos);
        self.pos = next;
        ch
    }

    priv fn eat(&mut self, c:char) -> bool {
        if self.cur() == c {
            self.pop();
            true
        } else {
            false
        }
    }

    priv fn expect<T>(&mut self, c: char, f: &fn() -> T) -> Result<T,(~str,uint)> {
        if self.eat(c) {
            Ok(f())
        } else {
            Err((fmt!("Unexpected character '%c', expected '%c'", self.cur(), c), self.pos))
        }
    }
}

pub fn write_format<'a, W:Writer>(writer:W, parser: &'a mut Parser, f: &fn(Spec) -> ~str) {
    let mut pos;
    loop {
        pos = parser.cur_pos();
        match parser.next_item() {
            Place(holder) => writer.write_str(f(holder)),
            Error { _ } => {
                parser.skip_to_next_indicator();
                let p = parser.cur_pos();
                let slice = parser.source.slice(pos,p);
                writer.write_str(slice);
            }
            Raw(s) => writer.write_str(s),
            End => break
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    static test_desc : ParserDesc<'static>  = ParserDesc{
        indicator: '%',
        flags: ['#', '^',' '],
        specifiers: ['a','b','c'],
        special_flag: Some('!')
    };

    #[test]
    fn parse_empty() {
        let mut parser = Parser::new("", &test_desc);
        let res = parser.next_item();

        match res {
            End => (),
            a => fail!("Didn't get expected parse result. Expected End, got %?", a)
        }
    }

    #[test]
    fn parse_indicator1() {
        let mut parser = Parser::new("%%", &test_desc);
        let res = parser.next_item();
        match res {
            Raw("%") => (),
            a => fail!("Didn't get expected parse result. Expected Raw(~\"%%\"), got %?", a)
        }
    }

    #[test]
    fn parse_indicator2() {
        let mut parser = Parser::new("%%a", &test_desc);
        let res = parser.next_item();
        match res {
            Raw("%") => (),
            a => fail!("Didn't get expected parse result. Expected Raw(~\"%%\"), got %?", a)
        }
    }

    #[test]
    fn parse_single_basic() {
        let mut parser = Parser::new("%a", &test_desc);
        let res = parser.next_item();
        match res {
            Place(holder) => assert_eq!(holder.specifier, 'a'),
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_complex() {
        let mut parser = Parser::new("%#^30.20a", &test_desc);
        let res = parser.next_item();
        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.width, Value(30));
                assert_eq!(holder.precision, Value(20));
                assert!(holder.flags.contains(&'#'));
                assert!(holder.flags.contains(&'^'));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_position1() {
        let mut parser = Parser::new("%{5}a", &test_desc);
        let res = parser.next_item();

        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.position, Some(5));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_position2() {
        let mut parser = Parser::new("%{2}[*].3a", &test_desc);
        let res = parser.next_item();

        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.position, Some(2));
                assert_eq!(holder.width, Next);
                assert_eq!(holder.precision, Value(3));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_fields() {
        let mut parser = Parser::new("%[*].[5]a", &test_desc);
        let res = parser.next_item();

        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.width, Next);
                assert_eq!(holder.precision, Arg(5));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_special() {
        let mut parser = Parser::new("%!a", &test_desc);
        let res = parser.next_item();

        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert!(holder.special_flag);
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_single_num_arg() {
        let mut parser = Parser::new("%<12>a", &test_desc);
        let res = parser.next_item();

        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.num_arg, Some(12));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_multi_basic1() {
        let mut parser = Parser::new("%a%b", &test_desc);

        let res = parser.next_item();
        match res {
            Place(holder) => assert_eq!(holder.specifier, 'a'),
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Place(holder) => assert_eq!(holder.specifier, 'b'),
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_multi_basic2() {
        let mut parser = Parser::new("%a %b", &test_desc);

        let res = parser.next_item();
        match res {
            Place(holder) => assert_eq!(holder.specifier, 'a'),
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Raw(" ") => (),
            a => fail!("Didn't get expected parse result. Expected Raw(\" \"), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Place(holder) => assert_eq!(holder.specifier, 'b'),
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_multi_complex() {
        let mut parser = Parser::new("% #a %^20.5b", &test_desc);
        let res = parser.next_item();
        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert!(holder.flags.contains(&' '));
                assert!(holder.flags.contains(&'#'));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Raw(" ") => (),
            a => fail!("Didn't get expected parse result. Expected Raw(\" \"), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Place(holder) =>{
                assert_eq!(holder.specifier, 'b');
                assert_eq!(holder.width, Value(20));
                assert_eq!(holder.precision, Value(5));
                assert!(holder.flags.contains(&'^'));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_multi_fields() {
        let mut parser = Parser::new("%{2} #.[6]aqq%^[*].5b", &test_desc);
        let res = parser.next_item();
        match res {
            Place(holder) => {
                assert_eq!(holder.specifier, 'a');
                assert_eq!(holder.position, Some(2));
                assert_eq!(holder.precision, Arg(6));
                assert!(holder.flags.contains(&' '));
                assert!(holder.flags.contains(&'#'));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Raw("qq") => (),
            a => fail!("Didn't get expected parse result. Expected Raw(\"qq\"), got %?", a)
        }

        let res = parser.next_item();
        match res {
            Place(holder) =>{
                assert_eq!(holder.specifier, 'b');
                assert_eq!(holder.width, Next);
                assert_eq!(holder.precision, Value(5));
                assert!(holder.flags.contains(&'^'));
            }
            a => fail!("Didn't get expected parse result. Expected Place(Holder), got %?", a)
        }
    }

    #[test]
    fn parse_loop() {
        let mut parser = Parser::new("There are %a things with %2.4b items reading %[20]c, %%x", &test_desc);
        let mut s = ~"";
        loop {
            let res = parser.next_item();
            match res {
                Place(holder) => {
                    match holder.specifier {
                        'a' => s = s + ~"42",
                        'b' => {
                            if holder.width == Value(2) && holder.precision == Value(4) {
                                s = s + ~"22.5000";
                            } else {
                                fail!("Width or Precision are wrong");
                            }
                        }
                        'c' => {
                            if holder.width == Arg(20) {
                                s = s + "Mary Poppins"
                            } else {
                                fail!("Width is wrong");
                            }
                        }
                        _ => fail!("Got a specifier that wasn't expected")
                    }
                }
                Raw(r) => s = s + r.to_owned(),
                Error { msg:msg, pos:_ } => fail!(msg),
                End => break
            }
        }

        assert_eq!(~"There are 42 things with 22.5000 items reading Mary Poppins, %x", s);
    }

    macro_rules! parse_fail (
        ($s:expr) => (
            let _parser = Parser::new($s, &test_desc);
            let res = _parser.next_item();

            match res {
                Error { _ } => (),
                Place(holder) => fail!("Unexpected %?", holder),
                a => fail!("Didn't get expected parse result. Expected Error, got %?", a)
            }
        )
    )

    #[test]
    fn parse_fail_spec() {
        parse_fail!("%z");
    }

    #[test]
    fn parse_fail_flag() {
        parse_fail!("%pa");
    }

    #[test]
    fn parse_fail_flag_repeat() {
        parse_fail!("%  a");
    }

    #[test]
    fn parse_fail_num_arg() {
        parse_fail!("%<a>5a");
    }

    #[test]
    fn parse_fail_position() {
        parse_fail!("%{}5a");
    }

    #[test]
    fn parse_fail_field() {
        parse_fail!("%[]a");
    }

    fn write_format_str() {
        let mut parser = Parser::new("%1a %.2b %3c %{0}b", &test_desc);
        let res = parser.next_item();
        let args = ["An A", "T", "3"];
        let mut pos = 0;
        let s = do with_str_writer |w| {
            do write_format(w, parser) |spec| {
                let a = match spec.position {
                    None => {
                        let tmp = args[pos];
                        pos += 1;
                        tmp
                    }
                    Some(n) => {
                        arg[n]
                    }
                };
            }
        };
    }
}

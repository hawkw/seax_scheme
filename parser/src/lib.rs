#![crate_name = "parser"]
#![unstable(feature="scheme")]
#![crate_type = "lib"]
#![feature(scheme)]
#![feature(box_syntax,box_patterns)]
#![feature(slice_patterns)]
#![feature(staged_api)]
#![staged_api]

extern crate combine;
extern crate combine_language;
extern crate ast;

use combine::*;
use combine::primitives::Stream;
use combine_language::{LanguageEnv, LanguageDef, Identifier};

use ast::*;
use ast::ExprNode::*;
use ast::NumNode::*;

use std::str::FromStr;
use std::error::Error;
use std::ops::Deref;

#[cfg(test)]
mod tests;

type ParseFn<'a, I, T> = fn (&SchemeEnv<'a, I>, State<I>)
                            -> ParseResult<T, I>;

struct SchemeParser<'a: 'b, 'b, I, T>
where I: Stream<Item=char>,
      I: 'b {
    env: &'b SchemeEnv<'a, I>,
    parser: ParseFn<'a, I, T>
}

impl<'a, 'b, I, T> Parser for SchemeParser<'a, 'b, I, T>
where I: Stream<Item=char> {
    type Output = T;
    type Input = I;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<T, I> {
        (self.parser)(self.env, input)
    }
}

impl <'a, 'b, I, T> Clone for SchemeParser<'a, 'b, I, T>
where I: Stream<Item=char> {

    fn clone(&self) -> Self {
        SchemeParser { env: self.env, parser: self.parser }
    }
}

struct SchemeEnv<'a, I>
where I: Stream<Item=char> {
    env: LanguageEnv<'a, I>
}

impl<'a, I> Deref for SchemeEnv<'a, I>
where I: Stream<Item=char> {
    type Target = LanguageEnv<'a, I>;

    fn deref(&self) -> &LanguageEnv<'a, I> {
        &self.env
    }
}

impl<'a, I> SchemeEnv<'a, I>
where I: Stream<Item=char> {

    fn parser<'b, T>(&'b self,
                    parser: ParseFn<'a, I, T>)
                    -> SchemeParser<'a, 'b, I, T> {
        SchemeParser { env: self, parser: parser }
    }

    fn hex_scalar(input: State<I>) -> ParseResult<String, I> {
        satisfy(|c| c == 'x' || c == 'X')
            .with( many1(hex_digit()) )
            .parse_state(input)
    }

    fn hex_int(&self, input: State<I>) -> ParseResult<i64, I> {
        satisfy(|c| c == '#')
            .with(parser(SchemeEnv::hex_scalar))
            .map(|x| i64::from_str_radix(x.as_ref(), 16)
                         .unwrap())
            .parse_state(input)
    }

    fn dec_int(&self, input: State<I>) -> ParseResult<i64, I> {
        optional(satisfy(|c| c == '#')
                .and(satisfy(|c| c == 'd' || c == 'D')))
                .with(many1::<String, _>(digit())
                        .map(|x| i64::from_str(x.as_ref()).unwrap() )
                )
            .parse_state(input)
    }

    /// Parser for signed integer constants.
    ///
    /// This parses signed integer constants in decimal and hexadecimal.
    ///
    /// TODO: add support for octal
    /// TODO: add support for binary
    /// TODO: add support for R6RS exponents
    fn sint_const(&self, input: State<I>) -> ParseResult<NumNode, I> {

        let signed = optional(satisfy(|c| c == '-'))
                .and(try(self.parser(SchemeEnv::hex_int))
                        .or(self.parser(SchemeEnv::dec_int))
                    );

        signed.map(|x| {
                if let Some(sign) = x.0 {
                    let mut s = String::new();
                    s.push(sign);
                    s.push('1');
                    x.1 * i64::from_str(s.as_ref()).unwrap()
                } else {
                    x.1
                }
                })
            .skip(not_followed_by(satisfy(|c|
                c == 'u' || c == 'U' || c == '.' || c == 'f' || c == 'F')
            ))
            .map(|x: i64| NumNode::IntConst(IntNode{value: x}))
            .parse_state(input)
    }

    fn hex_uint(&self, input: State<I>) -> ParseResult<u64, I> {
        satisfy(|c| c == '#')
            .with(parser(SchemeEnv::hex_scalar)
            .map(|x| u64::from_str_radix(x.as_ref(), 16).unwrap()) )
            .parse_state(input)
    }

    fn dec_uint(&self, input: State<I>) -> ParseResult<u64, I> {
        many1::<String, _>(digit())
            .map(|x| u64::from_str(x.as_ref()).unwrap() )
            .parse_state(input)
    }

    /// Parser for unsigned integer constants.
    ///
    /// This parses unssigned integer constants in decimal and hexadecimal.
    ///
    /// TODO: add support for octal
    /// TODO: add support for binary
    /// TODO: add support for R6RS exponents
    fn uint_const(&self, input: State<I>) -> ParseResult<NumNode, I> {
        try(self.parser(SchemeEnv::hex_uint))
            .or(self.parser(SchemeEnv::dec_uint))
            .skip(satisfy(|c| c == 'u' || c == 'U'))
            .map(|x: u64| NumNode::UIntConst(UIntNode{value: x}))
            .parse_state(input)
    }


    /// Parser for floating-point constants.
    ///
    /// This parses floating-point constants. Currently, this parser
    /// recognizes numbers with decimal points as floating point, followed
    /// by an optional `f` or `F`. Numbers with `f`s but no decimal points,
    /// i.e. `1F`, are currently not recognized. While this form of number
    /// is not specified by R6RS, I'd like to support it anyway as it's
    /// a common form for floating-point numbers. Priority: low.
     fn float_const(&self, input: State<I>) -> ParseResult<NumNode, I> {
        let float_str =  many1::<String,_>(digit())
            .and(satisfy(|c| c == '.'))
            .and(many1::<String, _>(digit()));

        float_str.map(|x| {
                let s = format!("{}{}{}", (x.0).0, (x.0).1, x.1);
                NumNode::FloatConst(FloatNode{
                    value: f64::from_str(s.as_ref()).unwrap()
                })
            })
            .skip(optional(satisfy(|c| c == 'f' || c == 'F')))
            .parse_state(input)
    }

    /// Parses a floating-point, signed integer, or unsigned integer constant.
    fn number(&self, input: State<I>) -> ParseResult<NumNode, I> {
        choice([
            try(self.parser(SchemeEnv::sint_const)),
            try(self.parser(SchemeEnv::uint_const)),
            try(self.parser(SchemeEnv::float_const))
            ])
            .parse_state(input)
    }

    fn escape_char(input: State<I>) -> ParseResult<char, I> {
        satisfy(|c| c == '\\')
            .with( satisfy(|c|
                    c == 'a' || c == 'b' || c == 't' || c == 'n' ||
                    c == 'v' || c == 'f' || c == 'r' || c == '\\' || c == '"')
                    .map(|c| match c {
                        '"'     => '"',
                        '\\'    => '\\',
                        '/'     => '/',
                        'b'     => '\u{0008}',
                        'f'     => '\u{000c}',
                        'n'     => '\n',
                        'r'     => '\r',
                        't'     => '\t',
                        _       => unreachable!()
                    }) )
            .parse_state(input)
    }

    fn char_name(input: State<I>) -> ParseResult<char, I> {
        choice([
            string("tab"), string("vtab"), string("page"), string("alarm"),
            string("nul"), string("esc"), string("defne"), string("space"),
            string("backspace"), string("newline"), string("linefeed"),
            string("return")
        ]).map(|x| match x {
            "newline" | "linefeed" => '\n',
            "tab"                  => '\t',
            "nul"                  => '\u{0000}',
            "alarm"                => '\u{0007}',
            "backspace"            => '\u{0008}',
            "vtab"                 => '\u{000B}',
            "page"                 => '\u{000C}',
            "return"               => '\u{000D}',
            "esc"                  => '\u{001B}',
            "space"                => '\u{0020}',
            "defne"                => '\u{007F}',
            _                      => unreachable!()
        }).parse_state(input)
    }


    /// Recognizes R<sup>6</sup>RS character constants.
    ///
    /// Character constants begin with the delimiter `#\` and may take
    /// one of three forms:
    ///
    /// 1. single ASCII character
    ///     + e.g. `#\a`, `#\Q`, `#\&` etc.
    /// 2. R<sup>6</sup>RS named character
    ///     + e.g. `#\newline`, `#\tab` etc.
    ///     + please consult the
    ///      [Revised<sup>6</sup> Report on Scheme](http://www.r6rs.org/)
    ///      for a compfne listing of valid character names
    /// 3. Hex scalar value
    ///     + delimited with the character `x`
    ///     + e.g. `#\x1B` etc.
    fn character(&self, input: State<I>) -> ParseResult<CharNode, I> {
        let any_char = any();
        let hex_char = parser(SchemeEnv::hex_scalar)
                           .map(|x| std::char::from_u32(
                                    u32::from_str_radix(&x,16).unwrap()
                                ).unwrap() );

        string("#\\")
            .with(parser(SchemeEnv::char_name).or(hex_char).or(any_char))
            .map(|c| CharNode { value: c})
            .parse_state(input)
    }

    fn string_const(&self, input: State<I>) -> ParseResult<StringNode, I> {

        let string_char = satisfy(|c| c != '\\' && c!= '"')
                            .or(parser(SchemeEnv::escape_char));

        between(
            satisfy(|c| c == '"'),
            satisfy(|c| c == '"'),
            many(string_char) )
        .map(|x| StringNode { value: x })
        .parse_state(input)
    }

    /// Parses boolean constants.
    ///
    /// `#t`, `#T` -> `true`
    /// `#f`, `#F` -> `false`
    fn bool_const(&self, input: State<I>) -> ParseResult<BoolNode, I> {

        let t_const = try(satisfy(|c| c == 't' || c == 'T'))
                .map(|_| BoolNode{ value: true });

        let f_const = try(satisfy(|c| c == 'f' || c == 'F'))
                .map(|_| BoolNode{ value: false });

        satisfy(|c| c == '#')
            .with( t_const.or(f_const) )
            .parse_state(input)
    }

    /// Parser for valid R6RS identifiers.
    ///
    /// An identifier may begin with an alphabetic character or
    /// one of the following special characters `!`, `$`, `&`, `:`, `^`,
    /// `<`, `>`, `_`,`~`,`\`, or `?`. Subsequent characters may also include
    /// numbers or the special characters `+`, `-`, `.`, and `@`.
    ///
    /// Essentially, this parser recognizes the regular expression
    /// `[a-zA-Z!\$%:\^<>_~\\\?][a-zA-Z0-9!\$%:\^<>_~\\\?\+\-\.@]*`.
    ///
    /// For more information, consult the
    /// [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html).
    #[cfg_attr(feature = "unstable",
        stable(feature = "parser", since = "0.0.2") )]
    fn name(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        try(self.env.op())
            .or(self.env.identifier())
            .map(|n| Name(NameNode::new(n)))
            .parse_state(input)
    }

    fn sexpr_inner(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        self.parser(SchemeEnv::expr)
            .and(self.lex(many(self.parser(SchemeEnv::expr))))
            .map(|x| SExpr(SExprNode {
                        operator: Box::new(x.0),
                        operands: x.1
                    })
                )
            .parse_state(input)
    }

    fn sexpr(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        self.lex(self.parens(self.parser(SchemeEnv::sexpr_inner))
                    .or(self.brackets(self.parser(SchemeEnv::sexpr_inner)) )
                )
            .parse_state(input)
    }

    fn list(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        self.lex(self.parens(
            many(self.parser(SchemeEnv::expr))
                     .map(|x| ListConst(ListNode { elements: x }) )
            ))
            .parse_state(input)
    }

    fn constant(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        try(self.parser(SchemeEnv::number)
                .map(NumConst))
            .or(try(self.parser(SchemeEnv::character)
                    .map(CharConst)))
            .or(try(self.parser(SchemeEnv::string_const)
                    .map(StringConst)))
            .or(try(self.parser(SchemeEnv::bool_const)
                    .map(BoolConst)))
            .parse_state(input)
    }

    fn form(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        choice([ self.parser(SchemeEnv::sexpr),
                 self.parser(SchemeEnv::list),
                 self.parser(SchemeEnv::name) ])
            .parse_state(input)
    }

    /// Parses Scheme expressions.
    #[allow(unconditional_recursion)]
    fn expr(&self, input: State<I>) -> ParseResult<ExprNode, I> {
        self.lex(
            self.parser(SchemeEnv::constant)
                .or(self.parser(SchemeEnv::form) )
            )
            .parse_state(input)
    }

    fn expr_parser<'b>(&'b self) -> SchemeParser<'a, 'b, I, ExprNode> {
        self.parser(SchemeEnv::expr)
    }

}

#[cfg_attr(feature = "unstable",
    unstable(feature="parser") )]
pub fn parse(program: &str) -> Result<ExprNode, String> {
    let env = LanguageEnv::new(LanguageDef{
        ident: Identifier {
            start:   letter().or(satisfy(|c|
                // R6RS 'special initial' characters
                c == '!' || c == '$' || c == '%' || c == ':' || c == '^' ||
                c == '<' || c == '>' || c == '_' || c == '~' || c == '\\' ||
                c == '?'
            )),
            rest: alpha_num().or(satisfy(|c|
                // R6RS 'special initial' characters
                c == '!' || c == '$' || c == '%' || c == ':' || c == '^' ||
                c == '<' || c == '>' || c == '_' || c == '~' || c == '\\' ||
                c == '?' ||
                // R6RS 'special subsequent' characters
                c == '+' || c == '-' || c == '.' || c == '@'
            )),
            // this is a hack around `combine_language` behaviour; since the
            // Seax Scheme AST expects reserved words to be parsed as identifiers,
            // we put the operators here instead so they don't get parsed as idents
            reserved: ["+", "-", "/", "*", "=", ">", ">=", "<", "<=", "!="].iter()
                        .map(|x| (*x).into()).collect()
        },
        op: Identifier {
            start: satisfy( |c|
                c == '+' || c == '-' || c == '*' || c == '/' || c == '=' ||
                c == '!' || c == '>' || c == '<'),
            rest: satisfy(|c| c == '='),
            // this is a hack around `combine_language` behaviour; since the
            // Seax Scheme AST expects reserved words to be parsed as identifiers,
            // we just put the keywords here
            reserved: ["access", "define-syntax", "macro" , "and" , "delay",
            "make-environment", "begin" , "do", "named-lambda", "bkpt",
            "fluid-let", "or", "case", "if", "quasiquote", "cond",
            "in-package", "quote", "cons-stream", "lambda", "scode-quote",
            "declare", "let", "sequence", "default-object?", "let*", "set!",
            "define", "let-syntax", "the-environment", "define-integrable",
            "letrec", "unassigned?", "define-macro", "local-declare", "using-syntax",
            "define-structure", "car", "cdr", "cons", "nil", "nil?", "atom?"]
                .iter().map(|x| (*x).into()).collect()
        },
        comment_line: ";",
        comment_start: "#|",
        comment_end: "|#"

    });

    let senv = SchemeEnv{env: env};

    senv.white_space()
        .with(senv.expr_parser())
        .parse(program)
        .map_err(|e| String::from(e.description()) )
        .map(    |x| x.0 )
}

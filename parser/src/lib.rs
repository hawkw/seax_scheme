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

type ParseFn<'a, I, T> = fn (&'a SchemeEnv<'a, I>, State<I>)
                            -> ParseResult<T, I>;

#[derive(Clone)]
struct SchemeParser<'a, I, T>
where I: Stream<Item=char>,
      I: 'a {
    env: &'a SchemeEnv<'a, I>,
    parser: ParseFn<'a, I, T>
}

impl<'a, I, T> Parser for SchemeParser<'a, I, T>
where I: Stream<Item=char> {
    type Output = T;
    type Input = I;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<T, I> {
        (self.parser)(self.env, input)
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

    fn parser<T>(&'a self, parser: ParseFn<'a, I,T>)
                    -> SchemeParser<'a, I, T> {
        SchemeParser { env: self, parser: parser }
    }

    fn hex_scalar(input: State<I>) -> ParseResult<String, I> {
        satisfy(|c| c == 'x' || c == 'X')
            .with( many1(hex_digit()) )
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

        let hex_int = satisfy(|c| c == '#')
                        .with(parser(SchemeEnv::hex_scalar)
                        .map(|x| i64::from_str_radix(x.as_ref(), 16)
                                     .unwrap())
                            );

        let dec_int = optional(satisfy(|c| c == '#')
                        .and(satisfy(|c| c == 'd' || c == 'D')))
                        .with(many1::<String, _>(digit())
                                .map(|x| i64::from_str(x.as_ref()).unwrap() )
                            );

        let signed = optional(satisfy(|c| c == '-'))
                        .and(try(hex_int).or(dec_int)    );

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

    /// Parser for unsigned integer constants.
    ///
    /// This parses unssigned integer constants in decimal and hexadecimal.
    ///
    /// TODO: add support for octal
    /// TODO: add support for binary
    /// TODO: add support for R6RS exponents
    fn uint_const(&self, input: State<I>) -> ParseResult<NumNode, I> {

        let hex_uint = satisfy(|c| c == '#')
                        .with(self.parser(SchemeEnv::hex_scalar)
                        .map(|x| u64::from_str_radix(x.as_ref(), 16).unwrap()) );

        let dec_uint = many1::<String, _>(digit())
                .map(|x| u64::from_str(x.as_ref()).unwrap() );

        try(hex_uint).or(dec_uint)
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

        let newline = try(string("newline")).or(try(string("linefeed")))
                                            .map(|_| '\n');

        let tab = try(string("tab")).map(|_| '\t');

        let nul = try(string("nul")).map(|_| '\u{0000}');

        let backspace = try(string("backspace")).map(|_| '\u{0008}');

        let vtab = try(string("vtab")).map(|_| '\u{000B}');

        let page = try(string("page")).map(|_| '\u{000C}');

        let retn = try(string("return")).map(|_| '\u{000D}');

        let esc = try(string("esc")).map(|_| '\u{001B}');

        let defne = try(string("defne")).map(|_| '\u{007F}');

        let alarm = try(string("alarm")).map(|_| '\u{0007}');

        let space = try(string("space")).map(|_| '\u{0020}');

        let char_name = choice([ newline, tab, vtab, retn, nul, page, esc,
                                 defne, alarm, space, backspace ]);

        let hex_char = self.parser(SchemeEnv::hex_scalar)
                           .map(|x| std::char::from_u32(
                                    u32::from_str_radix(&x,16).unwrap()
                                ).unwrap() );

        string("#\\")
            .with(choice([char_name, hex_char, parser(any)]))
            .map(|c| CharNode { value: c})
            .parse_state(input)
    }

    fn string_const(&self, input: State<I>) -> ParseResult<StringNode, I> {

        let string_char = satisfy(|c| c != '\\' && c!= '"')
                            .or(self.env.escape_char());

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
    fn name(&self, input: State<I>) -> ParseResult<NameNode, I> {
        try(self.env.operator())
            .or(self.env.ident())
            .map(NameNode::new)
            .parse_state(input)
    }

    /// Parses Scheme expressions.
    #[allow(unconditional_recursion)]
    fn expr(&self, input: State<I>) -> ParseResult<ExprNode, I> {

        let sexpr_inner = self.parser(SchemeEnv::expr)
                .and(self.lex(many(self.parser(SchemeEnv::expr))))
                .map(|x| SExpr(SExprNode {
                        operator: box x.0,
                        operands: x.1
                    })
                );

        let sexpr = self.lex(
            self.parens(sexpr_inner).or(
            self.brackets(sexpr_inner) )
            );

        let list = self.lex(self.parens(
                    many(self.parser(SchemeEnv::expr))
                        .map(|x| ListConst(ListNode {
                                elements: x
                            })
                        )
                    )
                );

        let constant = choice([
            try(self.parser(SchemeEnv::number)
                    .map(NumConst)),
            try(self.parser(SchemeEnv::character)
                    .map(CharConst)),
            try(self.parser(SchemeEnv::string_const)
                    .map(StringConst)),
            try(self.parser(SchemeEnv::bool_const)
                    .map(BoolConst))
            ]);

        let non_constant = choice([
            sexpr, list, self.parser(SchemeEnv::name).map(Name)
        ]);

        self.lex( constant.or(non_constant) )
            .parse_state(input)
    }

}

#[cfg_attr(feature = "unstable",
    unstable(feature="parser") )]
pub fn parse(program: &str) -> Result<ExprNode, String> {
    // R6RS 'special initial' characters
    let special_initial = satisfy(|c|
        c == '!' || c == '$' || c == '%' || c == ':' || c == '^' ||
        c == '<' || c == '>' || c == '_' || c == '~' || c == '\\' ||
        c == '?'
    );
    // R6RS 'special subsequent' characters
    let special_subsequent = satisfy(|c|
        c == '+' || c == '-' || c == '.' || c == '@'
    );
    let env = LanguageEnv::new(LanguageDef{
        ident: Identifier {
            start: letter().or(special_initial),
            rest: alpha_num().or(special_initial)
                             .or(special_subsequent),
            // this is a hack around `combine_language` behaviour; since the
            // Seax Scheme AST expects reserved words to be parsed as identifiers
            reserved: ["+", "-", "/", "*", "=", ">", ">=", "<", "<=", "!="].iter()
                        .map(|x| (*x).into()).collect().iter()
        },
        op: Identifier {
            start: satisfy( |c|
                c == '+' || c == '-' || c == '*' || c == '/' || c == '=' ||
                c == '!' || c == '>' || c == '<'),
            rest: satisfy(|c| c == '='),
            // this is a hack around `combine_language` behaviour; since the
            // Seax Scheme AST expects reserved words to be parsed as identifiers
            reserved: [].iter().map(|x| (*x).into()).collect().iter()
        },
        comment_line: ";",
        comment_start: "#|",
        comment_end: "|#"

    });

    let senv = SchemeEnv{env: env};

    senv.white_space()
        .with(senv.expr())
        .parse(program)
        .map_err(|e| String::from(e.description()) )
        .map(    |x| x.0 )
}

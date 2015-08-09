use ::ast::*;
use ::ast::ExprNode::*;
use ::ast::NumNode::*;

use super::*;
use super::combine::*;
use super::combine_language::{LanguageEnv, LanguageDef, Identifier};

macro_rules! test_parse {
    ($name:ident, $string:expr,$expect:expr) => {
        #[test] fn $name () {assert_eq!(parse($string), Ok($expect)) }
    };
}

test_parse!(test_ident,
    "ident", Name(NameNode { name: "ident".to_string() })
);

test_parse!(test_ident_single_char,
    "a", Name(NameNode { name: "a".to_string() })
);

test_parse!(test_ident_special_chars,
    "ident_With\\special!Chars:~-+",
    Name(NameNode { name: "ident_With\\special!Chars:~-+".to_string() })
);

test_parse!(test_sexpr,
    "(ident arg1 arg2)",
    SExpr(SExprNode {
        operator: Box::new(Name(NameNode { name: "ident".to_string() })),
        operands: vec![
            Name(NameNode { name: "arg1".to_string() }),
            Name(NameNode { name: "arg2".to_string() })
        ]
    })
);

test_parse!(test_sexpr_square_bracket,
    "[ident arg1 arg2]",
    SExpr(SExprNode {
        operator: Box::new(Name(NameNode { name: "ident".to_string() })),
        operands: vec![
            Name(NameNode { name: "arg1".to_string() }),
            Name(NameNode { name: "arg2".to_string() })
        ]
    })
);

// signed integers
test_parse!(test_sint_positive, "1234", NumConst(IntConst(IntNode { value: 1234 }) ));
test_parse!(test_sint_positive_d, "#d1234", NumConst(IntConst(IntNode { value: 1234 }) ));
test_parse!(test_sint_positive_D, "#D1234", NumConst(IntConst(IntNode { value: 1234 }) ));
test_parse!(test_sint_negative, "-1234", NumConst(IntConst(IntNode { value: -1234 }) ));
test_parse!(test_sint_hex_x, "#x0ff", NumConst(IntConst(IntNode { value: 0x0ff }) ));
test_parse!(test_sint_hex_X, "#X0FF", NumConst(IntConst(IntNode { value: 0x0ff }) ));

// unsigned integers
test_parse!(test_uint_u, "1234u", NumConst(UIntConst(UIntNode{ value: 1234 })));
test_parse!(test_uint_U, "1234U", NumConst(UIntConst(UIntNode{ value: 1234 })));
test_parse!(test_uint_hex_x, "#x0ffu", NumConst(UIntConst(UIntNode{ value: 0x0ff })));
test_parse!(test_uint_hex_X, "#X0FFU", NumConst(UIntConst(UIntNode{ value: 0x0ff })));

// /* // Currently unsupported
// #[test]
// fn test_parse_sint_bin_upper() {
//     assert_eq!(
//         parser(number).parse("0B01"),
//         Ok((IntConst(IntNode { value: 0b01isize }), ""))
//         );
//     assert_eq!(
//         parser(number).parse("0b01"),
//         Ok((IntConst(IntNode { value: 0b01isize }), ""))
//         );
// }*/

// #[test]
// fn test_lex_float() {
//     assert_eq!(
//         parser(number).parse("1.0"),
//         Ok((FloatConst(FloatNode { value: 1.0f64 }), ""))
//         );/* // Unsupported
//     assert_eq!(
//         parser(number).parse("1f").unwrap(),
//         (FloatConst(FloatNode { value: 1.0f64 }), "")
//         );
//     assert_eq!(
//         parser(number).parse("22.2222").unwrap(),
//         (FloatConst(FloatNode { value: 22.2222f64 }), "")
//         );
//     assert_eq!(
//         parser(number).parse("22.2222f").unwrap(),
//         (FloatConst(FloatNode { value: 22.2222f64 }), "")
//         );*/
// }
//
// /// This is the parsing component of basic arithmetic
// /// integration target
// ///
// /// ```lisp
// /// (+ 10 10)
// /// ```
// #[test]
// fn test_parse_arith() {
//     assert_eq!(
//         parser(expr).parse("(+ 10 10)"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode { name: "+".to_string() }),
//                 operands: vec![
//                     NumConst(IntConst(IntNode{ value: 10 })),
//                     NumConst(IntConst(IntNode{ value: 10 }))
//                 ]
//             }),
//             ""))
//         );
// }
//
// /// This is the parsing component of the CAR integration target
// ///
// /// ```lisp
// /// (car (cons 10 (cons 20 nil)))
// /// ```
// #[test]
// fn test_parse_car() {
//     assert_eq!(
//         parser(expr).parse("(car (cons 10 (cons 20 nil)))"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode { name: "car".to_string() }),
//                 operands: vec![
//                     SExpr(SExprNode {
//                         operator: box Name(NameNode { name: "cons".to_string() }),
//                         operands: vec![
//                             NumConst(IntConst(IntNode{ value: 10 })),
//                             SExpr(SExprNode {
//                                 operator: box Name(NameNode { name: "cons".to_string() }),
//                                 operands: vec![
//                                     NumConst(IntConst(IntNode{ value: 20 })),
//                                     Name(NameNode { name: "nil".to_string() })
//                                 ]
//                             })
//                         ]
//                     })
//                 ]
//             }),
//             ""))
//         );
// }
//
//
// /// This is the parsing component of the CDR integration target
// ///
// /// ```lisp
// /// (cdr (cons 10 (cons 20 nil)))
// /// ```
// #[test]
// fn test_parse_cdr() {
//     assert_eq!(
//         parser(expr).parse("(cdr (cons 10 (cons 20 nil)))"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode { name: "cdr".to_string() }),
//                 operands: vec![
//                     SExpr(SExprNode {
//                         operator: box Name(NameNode { name: "cons".to_string() }),
//                         operands: vec![
//                             NumConst(IntConst(IntNode{ value: 10 })),
//                             SExpr(SExprNode {
//                                 operator: box Name(NameNode { name: "cons".to_string() }),
//                                 operands: vec![
//                                     NumConst(IntConst(IntNode{ value: 20 })),
//                                     Name(NameNode { name: "nil".to_string() })
//                                 ]
//                             })
//                         ]
//                     })
//                 ]
//             }),
//             ""))
//         );
// }
//
// /// This is the parsing component of nested arithmetic
// /// integration target with square brackets added
// /// (just to ensure that the parser handles them correctly)
// ///
// /// ```lisp
// /// (- 20 [+ 5 5])
// /// ```
// #[test]
// fn test_parse_nested_arith_square_bracket() {
//     assert_eq!(
//         parser(expr).parse("(- 20 [+ 5 5])"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode { name: "-".to_string() }),
//                 operands: vec![
//                     NumConst(IntConst(IntNode{ value: 20 })),
//                     SExpr(SExprNode {
//                         operator: box Name(NameNode { name: "+".to_string() }),
//                         operands: vec![
//                             NumConst(IntConst(IntNode{ value: 5 })),
//                             NumConst(IntConst(IntNode{ value: 5 }))
//                         ]
//                     })
//                 ]
//             }),
//             ""))
//         );
// }
//
// /// This is the parsing component of nested arithmetic
// /// integration target
// ///
// /// ```lisp
// /// (- 20 (+ 5 5))
// /// ```
// #[test]
// fn test_parse_nested_arith() {
//     assert_eq!(
//         parser(expr).parse("(- 20 (+ 5 5))"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode { name: "-".to_string() }),
//                 operands: vec![
//                     NumConst(IntConst(IntNode{ value: 20 })),
//                     SExpr(SExprNode {
//                         operator: box Name(NameNode { name: "+".to_string() }),
//                         operands: vec![
//                             NumConst(IntConst(IntNode{ value: 5 })),
//                             NumConst(IntConst(IntNode{ value: 5 }))
//                         ]
//                     })
//                 ]
//             }),
//             ""))
//         );
// }
//
// /// This is the parsing component of the `compile_basic_branching_1`
// /// integration target.
// ///
// /// ```lisp
// /// (if (= 0 (- 1 1)) #t #f)
// /// ``
// #[test]
// fn test_parse_basic_branching_1() {
//     assert_eq!(
//         parser(expr).parse("(if (= 0 (- 1 1)) #t #f)"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode::new("if".to_string())),
//                 operands: vec![
//                     SExpr(SExprNode{
//                         operator: box Name(NameNode::new("=".to_string())),
//                         operands: vec![
//                             NumConst(IntConst(IntNode{value: 0})),
//                             SExpr(SExprNode{
//                                 operator: box Name(NameNode::new("-".to_string())),
//                                 operands: vec![
//                                     NumConst(IntConst(IntNode{ value: 1 })),
//                                     NumConst(IntConst(IntNode{ value: 1 }))
//                                 ]
//                             })
//                         ]
//                     }),
//                     BoolConst(BoolNode{value:true}),
//                     BoolConst(BoolNode{value:false}),
//                 ]
//             }
//             ),
//             "")
//         )
//     )
// }
//
// /// This is the parsing component of the `compile_basic_branching_2`
// /// integration target.
// ///
// /// ```lisp
// /// (+ 10 (if (nil? nil) 10 20))
// /// ``
// #[test]
// fn test_parse_basic_branching_2() {
//     assert_eq!(
//         parser(expr).parse("(+ 10 (if (nil? nil) 10 20))"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode::new("+".to_string())),
//                 operands: vec![
//                     NumConst(IntConst(IntNode{value:10})),
//                     SExpr(SExprNode{
//                         operator: box Name(NameNode::new("if".to_string())),
//                         operands: vec![
//                             SExpr(SExprNode{
//                                 operator: box Name(NameNode::new("nil?".to_string())),
//                                 operands: vec![Name(NameNode::new("nil".to_string()))]
//
//                             }),
//                             NumConst(IntConst(IntNode{value:10})),
//                             NumConst(IntConst(IntNode{value:20}))
//                         ]
//                     })
//                 ]
//             }
//             ),
//             "")
//         )
//     )
// }
//
// #[test]
// fn test_lex_bool() {
//     assert_eq!(
//         parser(bool_const).parse("#t"),
//         Ok((BoolNode { value: true}, ""))
//         );
//     assert_eq!(
//         parser(bool_const).parse("#T"),
//         Ok((BoolNode { value: true}, ""))
//         );
//     assert_eq!(
//         parser(bool_const).parse("#f"),
//         Ok((BoolNode { value: false}, ""))
//         );
//     assert_eq!(
//         parser(bool_const).parse("#F"),
//         Ok((BoolNode { value: false}, ""))
//         );
// }
//
// #[test]
// fn test_lex_char() {
//     assert_eq!(
//         parser(character).parse("#\\c"),
//         Ok((CharNode { value: 'c'}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\A"),
//         Ok((CharNode { value: 'A'}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\tab"),
//         Ok((CharNode { value: '\t'}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\newline"),
//         Ok((CharNode { value: '\n'}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\nul"),
//         Ok((CharNode { value: '\u{0000}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\backspace"),
//         Ok((CharNode { value: '\u{0008}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\vtab"),
//         Ok((CharNode { value: '\u{000B}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\page"),
//         Ok((CharNode { value: '\u{000C}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\return"),
//         Ok((CharNode { value: '\u{000D}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\esc"),
//         Ok((CharNode { value: '\u{001B}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\delete"),
//         Ok((CharNode { value: '\u{007F}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\alarm"),
//         Ok((CharNode { value: '\u{0007}' }, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\linefeed"),
//         Ok((CharNode { value: '\n'}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\space"),
//         Ok((CharNode { value: ' '}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\x0020"),
//         Ok((CharNode { value: ' '}, ""))
//         );
//     assert_eq!(
//         parser(character).parse("#\\x001B"),
//         Ok((CharNode { value: '\u{001B}' }, ""))
//         );
// }
//
// #[test]
// fn test_lex_string() {
//     assert_eq!(
//         parser(string_const).parse("\"a string\""),
//         Ok((StringNode { value: "a string".to_string() }, ""))
//     );
//     assert_eq!(
//         parser(string_const).parse("\"a string with a\\ttab\""),
//         Ok((StringNode { value: "a string with a\ttab".to_string() },""))
//     );
//     assert_eq!(
//         parser(string_const).parse("\"a string with an \\\"escaped\\\" quote\""),
//         Ok((StringNode { value: "a string with an \"escaped\" quote".to_string() },""))
//     );
//     assert_eq!(
//         parser(string_const).parse("\"the\\\\worst string ever\\\"\""),
//         Ok((StringNode { value: "the\\worst string ever\"".to_string() }, ""))
//     );
// }
// /*
// #[test]
// fn test_space_sexpr() {
//  assert_eq!(parser(expr).parse("(+ 10 (if (nil? nil) 10 20) )"),
//         Ok((
//             SExpr(SExprNode {
//                 operator: box Name(NameNode::new("+".to_string())),
//                 operands: vec![
//                     NumConst(IntConst(IntNode{value:10})),
//                     SExpr(SExprNode{
//                         operator: box Name(NameNode::new("if".to_string())),
//                         operands: vec![
//                             SExpr(SExprNode{
//                                 operator: box Name(NameNode::new("nil?".to_string())),
//                                 operands: vec![Name(NameNode::new("nil".to_string()))]
//
//                             }),
//                             NumConst(IntConst(IntNode{value:10})),
//                             NumConst(IntConst(IntNode{value:20}))
//                         ]
//                     })
//                 ]
//             }
//             ),
//             "")
//         )
//     )
// }*/

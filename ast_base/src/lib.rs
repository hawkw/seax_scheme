#![crate_name = "ast_base"]
#![unstable(feature="scheme")]
#![crate_type = "lib"]
#![feature(convert)]
#![feature(box_syntax,box_patterns)]
#![feature(vec_push_all)]
#![feature(slice_patterns)]
#![feature(compile)]
#![feature(staged_api)]
#![staged_api]

use std::fmt;

/// Expression
///
/// All Seax Scheme expressions are one of the following
///
///  + Nested S-Expressions
///  + Identifiers
///  + Lists
///  + Numbers
///     - signed int
///     - unsigned int
///     - floating-point
///  + Characters
///  + Strings
///
///  TODO: implement the entire Scheme 'numeric tower'
///  TODO: macros should happen
///  TODO: figure out quasiquote somehow.
#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub enum ExprNode {
    #[stable(feature = "ast", since = "0.0.2")]
    Root(RootNode),
    #[stable(feature = "ast", since = "0.0.2")]
    SExpr(SExprNode),
    #[stable(feature = "ast", since = "0.0.2")]
    Name(NameNode),
    #[stable(feature = "ast", since = "0.0.2")]
    ListConst(ListNode),
    #[stable(feature = "ast", since = "0.0.2")]
    NumConst(NumNode),
    #[stable(feature = "ast", since = "0.0.2")]
    BoolConst(BoolNode),
    #[stable(feature = "ast", since = "0.0.2")]
    StringConst(StringNode),
    #[stable(feature = "ast", since = "0.0.2")]
    CharConst(CharNode),
}

#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub enum NumNode {
    #[stable(feature = "ast", since = "0.0.2")]
    IntConst(IntNode),
    #[stable(feature = "ast", since = "0.0.2")]
    UIntConst(UIntNode),
    #[stable(feature = "ast", since = "0.0.2")]
    FloatConst(FloatNode)
}

/// AST node for the root of a program's AST
#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct RootNode { pub exprs: Vec<ExprNode> }


/// AST node for an S-expression.
///
/// This includes function application, assignment,
/// function definition, et cetera...Scheme is not a complexl anguage.
#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct SExprNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub operator: Box<ExprNode>,
    #[stable(feature = "ast", since = "0.0.2")]
    pub operands: Vec<ExprNode>,
}

/// AST node for a list literal
#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct ListNode { pub elements: Vec<ExprNode> }


/// AST node for an identifier
#[derive(Clone, PartialEq)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct NameNode { pub name: String }

impl NameNode {
    /// Returns true if this is a keyword
    #[inline]
    #[stable(feature = "ast", since = "0.0.3")]
    fn is_kw(&self) -> bool {
        match self.name.as_ref() {
            "access" | "define-syntax" | "macro"  | "and"  | "delay"
            | "make-environment" | "begin"  | "do"| "named-lambda"
            | "bkpt" | "fluid-let" | "or" | "case" | "if" | "quasiquote"
            | "cond" | "in-package" | "quote" | "cons-stream" | "lambda"
            | "scode-quote" | "declare" | "let" | "sequence" | "default-object?"
            | "let*" | "set!" | "define" | "let-syntax" | "the-environment"
            | "define-integrable" | "letrec" | "unassigned?" | "define-macro"
            | "local-declare" | "using-syntax" | "define-structure" | "car"
            | "cdr" | "cons" | "nil" | "nil?" | "atom?" => true,
            _ => false
        }
    }
    /// Returns true if this is an arithmetic operator
    #[inline]
    #[stable(feature = "ast", since = "0.0.3")]
    fn is_arith(&self) -> bool {
      match self.name.as_ref() {
         "+" | "-" | "*" | "/" | "%" => true,
         _ => false
      }
   }
    /// Returns true if this is a comparison operator
    #[inline]
    #[stable(feature = "ast", since = "0.0.3")]
    fn is_cmp(&self) -> bool {
      match self.name.as_ref() {
         "=" | "!=" | ">" | "<" | ">=" | "<=" => true,
         _ => false
      }
   }

   #[stable(feature = "ast", since = "0.0.4")]
   pub fn new(name: String) -> Self { NameNode {name: name} }
}

/// AST node for an integer constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct IntNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub value: i64
}

/// AST node for an unsigned integer constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct UIntNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub value: u64
}

/// AST node for a floating-point constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct FloatNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub value: f64
}

/// AST node for a boolean constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct BoolNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub value: bool
}

/// AST node for a character constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct CharNode {
    #[stable(feature = "ast", since = "0.0.2")]
    pub value: char
}

/// AST node for a  string constant
#[derive(Clone, PartialEq,Debug)]
#[stable(feature = "ast", since = "0.0.2")]
pub struct StringNode { pub value: String }

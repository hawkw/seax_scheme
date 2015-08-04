#![crate_name = "seax_scheme"]
#![unstable(feature="scheme")]
#![crate_type = "lib"]
#![feature(convert)]
#![feature(box_syntax,box_patterns)]
#![feature(vec_push_all)]
#![feature(slice_patterns)]
#![feature(compile)]
#![feature(staged_api)]
#![staged_api]

//! Library for compiling Scheme programs to Seax SVM bytecode.
//!
//! This Scheme implementation is based on the Scheme programming
//! language described in the
//! [Revised<sup>6</sup> Report on Scheme](http://www.r6rs.org/)
//! (R<sup>6</sup>RS). Any cases in which Seax Scheme differs from
//! R<sup>6</sup>RS Scheme are documented in the RustDoc for the
//! Seax Scheme library and in the Seax Scheme language manual.
//! Do note, however, that some inconsistencies between Seax Scheme
//! and R<sup>6</sup>RS may be the result of unimplemented features in
//! Seax Scheme.

#[macro_use] extern crate seax_util as seax;
#[macro_use] extern crate log;

/// Contains the Scheme abstract syntax tree (AST).
///
/// The AST stores the semantic structure of a parsed Scheme
/// program, and is responsible for compiling those programs
/// to SVM bytecode instructions, performing semantic analysis
/// (as necessary), and (eventually) for optimizing programs.
#[unstable(feature = "ast")]
pub mod parser;

/// Contains the Scheme parser.
///
/// This parser is based on the
/// [Scheme grammar](final/html/r6rs/r6rs-Z-H-7.html) given in the
/// [Revised<sup>6</sup> Report on Scheme](http://www.r6rs.org/)
/// (R<sup>6</sup>RS).
/// Any deviations from the R6RS standard, especially those with an impact
/// on the valid programs accepted by the parser, will be noted in the
/// parser's RustDoc.
#[unstable(feature="parser")]
pub mod ast;

use ast::*;

use seax::cell::SVMCell;
use seax::cell::Atom::*;
use seax::cell::Inst::*;
use seax::cell::SVMCell::*;

use seax::list::{List,Stack};
use seax::list::{Cons,Nil};

use seax::compiler_tools::ForkTable;
use seax::compiler_tools::ast::{INDENT,ASTNode};
use seax::compiler_tools::{SymTable, CompileResult, Index, Scope};

use std::fmt;
use std::fmt::Write;
use std::iter::FromIterator;
use std::convert::Into;
use std::hash::Hash;

/// Compile a Scheme program into a list of SVM cells (a control stack)
///
/// # Arguments
///
///  + `program` - a string containing a Scheme program or line
///
/// # Return Value
///
///  + A `Result` containing either a `List` of `SVMCells` if the program
///    was compiled successfully, or a `String` with any error messages that
///    occured during compilation
///
/// TODO: Should this return a list of errors instead?
#[unstable(feature="compile")]
pub fn compile(program: &str) -> Result<List<SVMCell>, String> {
    parser::parse(program)
        .and_then(|tree: ExprNode     | {
            debug!("parsed:\n{:?}",tree);
            tree.compile(&ForkTable::new()) })
        .map(     |prog: Vec<SVMCell> | {
            debug!("compiled: {:?}",prog);
            let result = List::from_iter(prog);
            debug!("control stack: {:?}", result);
            result
             })
}

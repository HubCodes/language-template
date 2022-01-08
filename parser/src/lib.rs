mod state;

#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate lazy_static;

use state::State;
use lang::ast::*;
use lalrpop_util::{ParseError, lexer};

lalrpop_mod!(grammar);

pub fn parse(code: &str) -> Result<Program, ParseError<usize, lexer::Token, &str>> {
    let mut state = State::new();
    grammar::StmtParser::new().parse(&mut state, code).map(|stmt| Program { stmt })
}

#[cfg(test)]
mod tests {
    use super::state::State;
    use super::grammar;
    use lang::ast::*;
    use std::collections::HashMap;

    lazy_static! {
        static ref TERM_PARSER: grammar::TermParser = grammar::TermParser::new();
        static ref EXPR_PARSER: grammar::ExprParser = grammar::ExprParser::new();
        static ref STMT_PARSER: grammar::StmtParser = grammar::StmtParser::new();
    }

    #[test]
    fn positive_int() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "42").unwrap();
        assert_eq!(Term::Num(Num::Int(42)), parse_result);
    }

    #[test]
    #[should_panic]
    fn negative_int() {
        let mut state = State::new();
        TERM_PARSER.parse(&mut state, "-42").unwrap();
    }

    #[test]
    fn too_big_int_into_double() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "12345678912345678912345").unwrap();
        assert_eq!(Term::Num(Num::Double(12345678912345678912345.)), parse_result);
    }

    #[test]
    fn positive_double() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "42.24").unwrap();
        assert_eq!(Term::Num(Num::Double(42.24)), parse_result);
    }

    #[test]
    fn double_with_exp() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "42.2e-3").unwrap();
        assert_eq!(Term::Num(Num::Double(42.2e-3)), parse_result);
    }

    #[test]
    fn string_literal_singlequote() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "'Hello, world!'").unwrap();
        assert_eq!(Term::Str("Hello, world!".to_string()), parse_result);
    }

    #[test]
    fn string_literal_doublequote() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "\"Hello, world!\"").unwrap();
        assert_eq!(Term::Str("Hello, world!".to_string()), parse_result);
    }

    #[test]
    fn string_literal_empty() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "''").unwrap();
        assert_eq!(Term::Str("".to_string()), parse_result);
    }

    #[test]
    fn string_literal_one_char() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "'1'").unwrap();
        assert_eq!(Term::Str("1".to_string()), parse_result);
    }

    #[test]
    fn symbol() {
        let mut state = State::new();
        let parse_result = TERM_PARSER.parse(&mut state, "name").unwrap();
        assert_eq!(Term::Symbol(Symbol::new(0, "name")), parse_result);
    }

    #[test]
    fn two_symbols_share_same_state() {
        let mut state = State::new();
        let first_parse_result = TERM_PARSER.parse(&mut state, "name1").unwrap();
        let second_parse_result = TERM_PARSER.parse(&mut state, "name2").unwrap();
        let first_expected = Term::Symbol(Symbol::new(0, "name1"));
        let second_expected = Term::Symbol(Symbol::new(1, "name2"));
        assert_eq!(first_expected, first_parse_result);
        assert_eq!(second_expected, second_parse_result);
    }

    #[test]
    fn postfix_expr_indexing() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "a[1]").unwrap();
        let target_symbol = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let target_index = Expr::Term(Term::Num(Num::Int(1)));
        let expected = Expr::Index(Box::new(target_symbol), Box::new(target_index));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn postfix_expr_indexing_recursive() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "(a[1])[2]").unwrap();
        let target_symbol = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let target_index_inner = Expr::Term(Term::Num(Num::Int(1)));
        let target_index_outer = Expr::Term(Term::Num(Num::Int(2)));
        let expected_inner = Expr::Index(Box::new(target_symbol), Box::new(target_index_inner));
        let expected = Expr::Index(Box::new(expected_inner), Box::new(target_index_outer));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn func_call_1_arg() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "a(1)").unwrap();
        let target_symbol = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let target_expr = Expr::Term(Term::Num(Num::Int(1)));
        let expected = Expr::Call(Box::new(target_symbol), vec![target_expr]);
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn func_call_no_arg() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "func()").unwrap();
        let target_symbol = Expr::Term(Term::Symbol(Symbol::new(0, "func")));
        let expected = Expr::Call(Box::new(target_symbol), vec![]);
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn func_call_2_args() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "add(1, 2)").unwrap();
        let target_symbol = Expr::Term(Term::Symbol(Symbol::new(0, "add")));
        let num1 = Expr::Term(Term::Num(Num::Int(1)));
        let num2 = Expr::Term(Term::Num(Num::Int(2)));
        let expected = Expr::Call(Box::new(target_symbol), vec![num1, num2]);
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn member_access() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo.bar").unwrap();
        let target_symbol_left = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let target_symbol_right = Symbol::new(1, "bar");
        let expected = Expr::Member(Box::new(target_symbol_left), target_symbol_right.clone());
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn typeof_expr() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "typeof foo").unwrap();
        let target_expr = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let expected = Expr::Typeof(Box::new(target_expr));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn multiply() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "1 * foo").unwrap();
        let lhs = Expr::Term(Term::Num(Num::Int(1)));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let expected = Expr::Binop(Binop::Mul, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn divide() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo / 4.2").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Double(4.2)));
        let expected = Expr::Binop(Binop::Div, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn remainder() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "bar % 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "bar")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Mod, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn add() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "a + 2").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let rhs = Expr::Term(Term::Num(Num::Int(2)));
        let expected = Expr::Binop(Binop::Add, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn sub() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "1.05 - foo").unwrap();
        let lhs = Expr::Term(Term::Num(Num::Double(1.05)));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let expected = Expr::Binop(Binop::Sub, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn shift_left() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "a << 2").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let rhs = Expr::Term(Term::Num(Num::Int(2)));
        let expected = Expr::Binop(Binop::Shl, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn shift_right() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "a >> 2").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "a")));
        let rhs = Expr::Term(Term::Num(Num::Int(2)));
        let expected = Expr::Binop(Binop::Shr, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn less_than() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo < 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Lt, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn greater_than() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo > 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Gt, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn less_than_eq() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo <= 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Lte, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn greater_than_eq() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo >= 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Gte, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn eq() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo == 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Eq, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn not_eq() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo != 3").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Num(Num::Int(3)));
        let expected = Expr::Binop(Binop::Neq, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn bitwise_and() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo & bar").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Expr::Binop(Binop::BitAnd, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn bitwise_xor() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo ^ bar").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Expr::Binop(Binop::Xor, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn bitwise_or() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo | bar").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Expr::Binop(Binop::BitOr, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn logical_and() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo && bar").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Expr::Binop(Binop::And, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn logical_or() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo || bar").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Expr::Binop(Binop::Or, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn assign() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "foo = bar + 2").unwrap();
        let lhs = Expr::Term(Term::Symbol(Symbol::new(0, "foo")));
        let rhs = Expr::Binop(
            Binop::Add,
            Box::new(Expr::Term(Term::Symbol(Symbol::new(1, "bar")))),
            Box::new(Expr::Term(Term::Num(Num::Int(2))))
        );
        let expected = Expr::Binop(Binop::Assign, Box::new(lhs), Box::new(rhs));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn variable_define() {
        let mut state = State::new();
        let parse_result = STMT_PARSER.parse(&mut state, "let foo = bar;").unwrap();
        let symbol = Symbol::new(0, "foo");
        let init_expr = Expr::Term(Term::Symbol(Symbol::new(1, "bar")));
        let expected = Stmt::VarDef(symbol, Some(init_expr));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn empty_object_literal() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "{}").unwrap();
        let expected = Expr::Term(Term::Obj(Obj::empty()));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn one_key_value_object_literal() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "{a:1}").unwrap();
        let mut kv_map: HashMap<String, Expr> = HashMap::new();
        kv_map.insert("a".to_string(), Expr::Term(Term::Num(Num::Int(1))));
        let expected = Expr::Term(Term::Obj(Obj { kv: kv_map }));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn two_keys_object_literal() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, r#"{a:1,"b":2}"#).unwrap();
        let mut kv_map: HashMap<String, Expr> = HashMap::new();
        kv_map.insert("a".to_string(), Expr::Term(Term::Num(Num::Int(1))));
        kv_map.insert("b".to_string(), Expr::Term(Term::Num(Num::Int(2))));
        let expected = Expr::Term(Term::Obj(Obj { kv: kv_map }));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn normal_stmt() {
        let mut state = State::new();
        let parse_result = STMT_PARSER.parse(&mut state, "1;").unwrap();
        let expected = Stmt::Expr(Expr::Term(Term::Num(Num::Int(1))));
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn if_stmt() {
        let mut state = State::new();
        let parse_result = STMT_PARSER.parse(&mut state, "if(1){2;}").unwrap();
        let expected = Stmt::If(
            Expr::Term(Term::Num(Num::Int(1))),
            Box::new(Stmt::Block(vec![Stmt::Expr(Expr::Term(Term::Num(Num::Int(2))))])),
            None
        );
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn if_block_empty() {
        let mut state = State::new();
        let parse_result = STMT_PARSER.parse(&mut state, "if(1){}").unwrap();
        let expected = Stmt::If(
            Expr::Term(Term::Num(Num::Int(1))),
            Box::new(Stmt::Block(vec![])),
            None
        );
        assert_eq!(expected, parse_result);
    }

    #[test]
    fn lambda_expr() {
        let mut state = State::new();
        let parse_result = EXPR_PARSER.parse(&mut state, "function(a){1;}").unwrap();
        let expected = Expr::Lambda(
            vec![Expr::Term(Term::Symbol(Symbol::new(0, "a")))],
            Box::new(Stmt::Block(vec![Stmt::Expr(Expr::Term(Term::Num(Num::Int(1))))]))
        );
        assert_eq!(expected, parse_result);
    }
}

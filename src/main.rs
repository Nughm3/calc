use logos::Logos;
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Logos)]
enum Token {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("!")]
    Exclamation,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[regex(r"\d+", |s| s.slice().parse::<u32>().unwrap())]
    Num(u32),
    #[error]
    #[regex(r"[ \n\r\t]+", logos::skip)]
    Error,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Literal(i64),
}

fn lexer(input: &str) -> Vec<Token> {
    Token::lexer(input).collect()
}

fn parse(tokens: &[Token]) -> Expr {
    use Token::*;
    let mut lex = tokens.into_iter().copied().peekable();

    fn expr(lex: &mut Peekable<impl Iterator<Item = Token>>, power: u8) -> Expr {
        let mut lhs = match lex.next().expect("unexpected EOF") {
            Num(x) => Expr::Literal(x as i64),
            op @ Plus | op @ Minus => {
                let pow = prefix_power(op);
                let child = expr(lex, pow);
                Expr::Unary(op, Box::new(child))
            }
            LeftParen => {
                let inner = expr(lex, 0);
                assert_eq!(lex.next(), Some(RightParen));
                inner
            }
            err => panic!("expected start of expression, got: {err:?}"),
        };

        loop {
            let op = match lex.peek() {
                Some(Plus | Minus | Star | Slash | Exclamation) => *lex.peek().unwrap(),
                _ => break,
            };

            if let Some((l, r)) = infix_power(op) {
                if l < power {
                    break;
                }

                lex.next().unwrap();
                let rhs = expr(lex, r);

                lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
            } else if let Some(pow) = postfix_power(op) {
                if pow < power {
                    break;
                }

                lex.next().unwrap();
                lhs = Expr::Unary(op, Box::new(lhs));
            }
        }

        lhs
    }

    expr(&mut lex, 0)
}

fn prefix_power(op: Token) -> u8 {
    match op {
        Token::Plus | Token::Minus => 5,
        err => panic!("invalid prefix op: {err:?}"),
    }
}

fn infix_power(op: Token) -> Option<(u8, u8)> {
    Some(match op {
        Token::Star | Token::Slash => (3, 4),
        Token::Plus | Token::Minus => (1, 2),
        _ => return None,
    })
}

fn postfix_power(op: Token) -> Option<u8> {
    match op {
        Token::Exclamation => Some(7),
        _ => None,
    }
}

impl Expr {
    fn eval(&self) -> i64 {
        match self {
            Expr::Binary(lhs, op, rhs) => {
                let (lhs, rhs) = (lhs.eval(), rhs.eval());
                match op {
                    Token::Plus => lhs + rhs,
                    Token::Minus => lhs - rhs,
                    Token::Star => lhs * rhs,
                    Token::Slash => lhs / rhs,
                    err => panic!("invalid binary op: {err:?}"),
                }
            }
            Expr::Unary(op, child) => {
                let child = child.eval();
                match op {
                    Token::Plus => child.abs(),
                    Token::Minus => -child,
                    Token::Exclamation => (1..=child).reduce(|acc, x| acc * x).unwrap(),
                    err => panic!("invalid unary op: {err:?}"),
                }
            }
            Expr::Literal(x) => *x,
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    dbg!(&input);

    let tokens = lexer(&input);
    dbg!(&tokens);

    let expr = parse(&tokens);
    dbg!(&expr);

    let result = expr.eval();
    dbg!(result);
}

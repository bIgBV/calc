use std::{
    cmp,
    collections::HashMap,
    fmt::{self},
    hash::Hash,
    io::{self, BufRead},
    sync::OnceLock,
};

fn main() {
    for line in io::stdin().lock().lines() {
        let line = line.unwrap();
        let mut parser = Parser::new(line);
        let expr = expression(&mut parser, State::new());
        println!("{}", expr)
    }
}

static PARSE_RULES: OnceLock<HashMap<TokenType, ParseRule>> = OnceLock::new();

/// A function pointer to a compiler function.
type ParseFn = Option<fn(&mut Parser, state: State) -> Expr>;

/// Holds the current state of the parser.
#[derive(Debug)]
struct State {
    /// The previous token that the parser saw
    previous: Token,

    /// The left side of an infix expression is held here if applicable
    left: Option<Expr>,
}

impl State {
    fn new() -> Self {
        State {
            previous: Token::new("", TokenType::Eof),
            left: None,
        }
    }

    fn with_token(previous: Token) -> Self {
        State {
            previous,
            left: None,
        }
    }
}

/// A grouping of parsing functions for a given token type along with it's associated precedence
#[derive(Debug)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

impl ParseRule {
    // Simple helper to make calling the parsing functions easier
    fn call_prefix(&self, parser: &mut Parser, state: State) -> Expr {
        self.prefix.expect(&format!(
            "No prefix rule mapped for operator: {:?}",
            state.previous
        ))(parser, state)
    }

    fn call_infix(&self, parser: &mut Parser, state: State) -> Expr {
        self.infix.expect(&format!(
            "No infix rule mapped for operator: {:?}",
            state.previous
        ))(parser, state)
    }
}

/// The core parser mapping
///
/// This function sets up the mapping of [`TokenType`] -> [`ParseRule`]
fn parse_rule(operator: &TokenType) -> &'static ParseRule {
    PARSE_RULES
        .get_or_init(|| {
            let mut map = HashMap::new();
            map.insert(
                TokenType::Add,
                ParseRule {
                    prefix: Some(unary),
                    infix: Some(binary),
                    precedence: Precedence::Term,
                },
            );
            map.insert(
                TokenType::Sub,
                ParseRule {
                    prefix: Some(unary),
                    infix: Some(binary),
                    precedence: Precedence::Term,
                },
            );
            map.insert(
                TokenType::Mul,
                ParseRule {
                    prefix: None,
                    infix: Some(binary),
                    precedence: Precedence::Factor,
                },
            );
            map.insert(
                TokenType::Div,
                ParseRule {
                    prefix: None,
                    infix: Some(binary),
                    precedence: Precedence::Factor,
                },
            );
            map.insert(
                TokenType::Ident,
                ParseRule {
                    prefix: Some(ident),
                    infix: None,
                    precedence: Precedence::None,
                },
            );
            map.insert(
                TokenType::LeftParen,
                ParseRule {
                    prefix: Some(grouping),
                    infix: None,
                    precedence: Precedence::None,
                },
            );
            map.insert(
                TokenType::Eof,
                ParseRule {
                    prefix: None,
                    infix: None,
                    precedence: Precedence::None,
                },
            );
            map.insert(
                TokenType::RightParen,
                ParseRule {
                    prefix: None,
                    infix: None,
                    precedence: Precedence::None,
                },
            );
            map
        })
        .get(&operator)
        .expect(&format!(
            "No parser rule mapping found for operator: {:?}",
            operator
        ))
}

/// For example, say the compiler is sitting on a chunk of code like:
/// -a * b + c If we call parsePrecedence(PREC_ASSIGNMENT), then it will parse the
/// entire expression because + has higher precedence than assignment.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Precedence {
    None,
    Term,   // + -
    Factor, // * /
    Unary,  // -
}

impl cmp::PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        (*self as u8).partial_cmp(&(*other as u8))
    }
}

/// Parses any expression defined in the language
fn expression(parser: &mut Parser, _state: State) -> Expr {
    // We simply parse the lowest precedence level, which subsumes all of the higher-precedence expressions too
    parser.parse_expression(Precedence::None)
}

/// Parses a grouping
///
/// ```
/// grouping := ( expr )
/// ```
fn grouping(parser: &mut Parser, state: State) -> Expr {
    let expr = expression(parser, state);
    // Make sure to get the right paren
    parser.consume_token(TokenType::RightParen);
    expr
}

/// Parses a binary expression
///
/// ```
/// binaryExpr := expr <op> expr
/// ```
fn binary(parser: &mut Parser, state: State) -> Expr {
    // FIgure out the parse rule associated with the operator
    let operator_rule = parse_rule(&state.previous.kind);

    // Use that to parse the rest of the expression
    let right = parser.parse_expression(operator_rule.precedence);

    Expr::In(Box::new(InfixExpr {
        left: state
            .left
            .expect("Binary parser called without left expression in state"),
        right,
        op: state.previous.lexeme,
    }))
}

/// Parses a unary expression
///
/// ```
/// unary := <op> expr
/// ```
fn unary(parser: &mut Parser, state: State) -> Expr {
    // Parse the operand
    let right = parser.parse_expression(Precedence::Unary);

    Expr::Pre(Box::new(PrefixExpr {
        op: state.previous.lexeme,
        expr: right,
    }))
}

/// Parses an identifier in the language
///
/// ```
/// ident := <alphnumeric word>
/// ```
fn ident(_parser: &mut Parser, state: State) -> Expr {
    match state.previous.kind {
        TokenType::Ident => Expr::Atom(state.previous.lexeme),
        _ => panic!(
            "ident parser called with non ident token: {:?}",
            state.previous
        ),
    }
}

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
}

impl Parser {
    fn new(input: impl Into<String>) -> Self {
        Parser {
            lexer: Lexer::new(input.into()),
        }
    }

    /// The core of the pratt parser.
    ///
    /// This works on the idea that we always parse the prefix expression
    /// associated with a token, and then proceed to parse the rest of the
    /// expression _only_ if the precedence associated with the next token is
    /// greater than that of the current token.
    ///
    /// This ensures that the operator associativity is properly set up
    /// according the rules of our language -- which themselves are set up in
    /// the [`PARSE_RULES`] table
    pub fn parse_expression(&mut self, precedence: Precedence) -> Expr {
        let token = self.consume();

        let rule = parse_rule(&token.kind);
        let mut left = rule.call_prefix(self, State::with_token(token));

        // While the precedence of the next token is lower than the current token's precedence,
        // keep parsing the infix expression
        while precedence < parse_rule(&self.lookahead().kind).precedence {
            // Make sure to consume the next token
            let token = self.consume();
            let infix_rule = parse_rule(&token.kind);
            let mut state = State::with_token(token);

            // Update the state with the left expression for parsing the infix expression.
            state.left = Some(left);
            left = infix_rule.call_infix(self, state);
        }

        left
    }

    /// Consume the next token
    fn consume(&mut self) -> Token {
        self.lexer.pop()
    }

    /// Consume the token of the given [`TokenType`]
    fn consume_token(&mut self, kind: TokenType) -> Token {
        let next = self.lookahead();
        if next.kind != kind {
            panic!(
                "Next token {:?} not matching the expected kind: {:?}",
                next, kind
            );
        }

        self.consume()
    }

    /// Peek at the next token
    fn lookahead(&mut self) -> Token {
        self.lexer.peek()
    }
}

#[derive(Debug)]
/// Represents an Expression in our little language
enum Expr {
    Atom(String),
    Pre(Box<PrefixExpr>),
    In(Box<InfixExpr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Atom(ident) => write!(f, "{}", ident),
            Expr::Pre(pre) => write!(f, "{}", pre),
            Expr::In(infix) => write!(f, "{}", infix),
        }
    }
}

#[derive(Debug)]
struct PrefixExpr {
    expr: Expr,
    op: String,
}

impl fmt::Display for PrefixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.op, self.expr)
    }
}

#[derive(Debug)]
struct InfixExpr {
    left: Expr,
    right: Expr,
    op: String,
}

impl fmt::Display for InfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum TokenType {
    Add,
    Sub,
    Mul,
    Div,
    LeftParen,
    RightParen,
    Ident,
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    lexeme: String,
    kind: TokenType,
}

impl Token {
    fn new(lexeme: &str, kind: TokenType) -> Self {
        Self {
            lexeme: lexeme.to_string(),
            kind,
        }
    }
}

#[derive(Debug)]
struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut tokens = vec![];

        for ident in input.split_ascii_whitespace() {
            let token = match ident {
                "+" => Token::new("+", TokenType::Add),
                "-" => Token::new("-", TokenType::Sub),
                "*" => Token::new("*", TokenType::Mul),
                "/" => Token::new("/", TokenType::Div),
                "(" => Token::new("(", TokenType::LeftParen),
                ")" => Token::new(")", TokenType::RightParen),
                _ => Token::new(ident, TokenType::Ident),
            };
            tokens.push(token)
        }
        tokens.reverse();
        Lexer { tokens }
    }

    pub fn pop(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token {
            lexeme: "".to_string(),
            kind: TokenType::Eof,
        })
    }

    pub fn peek(&self) -> Token {
        if let Some(val) = self.tokens.last() {
            // makes our lives easier
            val.clone()
        } else {
            Token {
                lexeme: "".to_string(),
                kind: TokenType::Eof,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn arithmetic_expressions() {
        // Unary
        let mut parser = Parser::new("- 1");
        let expr = expression(&mut parser, State::new());

        assert_eq!(format!("{}", expr), "(- 1)");

        // Binary
        let mut parser = Parser::new("1 + 2");
        let expr = expression(&mut parser, State::new());

        assert_eq!(format!("{}", expr), "(1 + 2)");

        // Grouping
        let mut parser = Parser::new("1 + 2 + (3 + 4)");
        let expr = expression(&mut parser, State::new());

        assert_eq!(format!("{}", expr), "(((1 + 2) + (3) + 4))");

        // Factors
        let mut parser = Parser::new("1 + 2 / 4 * 5");
        let expr = expression(&mut parser, State::new());

        assert_eq!(format!("{}", expr), "(1 + ((2 / 4) * 5))");
    }
}

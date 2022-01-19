use logos::{Lexer, Logos};

#[derive(Debug, Clone)]
pub enum Type {
    Int(TokenData<()>),
    Float(TokenData<()>),
    String(TokenData<()>),
    Char(TokenData<()>),
    Bool(TokenData<()>),
}

impl Type {
    pub fn parse(token: &Token) -> Type {
        match token {
            Token::TypInt(d) => Type::Int(d.clone()),
            Token::TypFloat(d) => Type::Float(d.clone()),
            Token::TypString(d) => Type::String(d.clone()),
            Token::TypChar(d) => Type::Char(d.clone()),
            Token::TypBool(d) => Type::Bool(d.clone()),
            _ => panic!("Invalid type"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(TokenData<i32>),
    Float(TokenData<f64>),
    String(TokenData<String>),
    Char(TokenData<char>),
    Bool(TokenData<bool>),
}

impl Literal {
    pub fn to_string(&self) -> String {
        match self {
            Literal::Int(i) => i.data.to_string(),
            Literal::Float(f) => f.data.to_string(),
            Literal::String(s) => s.data.clone(),
            Literal::Char(c) => c.data.to_string(),
            Literal::Bool(b) => b.data.to_string(),
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            Literal::Int(d) => Type::Int(TokenData {
                line: d.line,
                column: d.column,
                length: d.length,
                data: (),
            }),
            Literal::Float(d) => Type::Float(TokenData {
                line: d.line,
                column: d.column,
                length: d.length,
                data: (),
            }),
            Literal::String(d) => Type::String(TokenData {
                line: d.line,
                column: d.column,
                length: d.length,
                data: (),
            }),
            Literal::Char(d) => Type::Char(TokenData {
                line: d.line,
                column: d.column,
                length: d.length,
                data: (),
            }),
            Literal::Bool(d) => Type::Bool(TokenData {
                line: d.line,
                column: d.column,
                length: d.length,
                data: (),
            }),
        }
    }

    pub fn parse(token: &Token) -> Literal {
        match token {
            Token::Int(i) => Literal::Int(i.clone()),
            Token::Float(f) => Literal::Float(f.clone()),
            Token::String(s) => Literal::String(s.clone()),
            Token::Char(c) => Literal::Char(c.clone()),
            Token::Bool(b) => Literal::Bool(b.clone()),
            _ => panic!("Invalid literal"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Plus(TokenData<()>),
    Minus(TokenData<()>),
    Mult(TokenData<()>),
    Div(TokenData<()>),
    Eq(TokenData<()>),
    Neq(TokenData<()>),
}

impl Op {
    pub fn priority(&self) -> i32 {
        match self {
            Op::Plus(_) => 1,
            Op::Minus(_) => 1,
            Op::Mult(_) => 2,
            Op::Div(_) => 2,
            Op::Eq(_) => 3,
            Op::Neq(_) => 3,
        }
    }

    pub fn parse(token: Token) -> Op {
        match token {
            Token::Plus(a) => Op::Plus(a),
            Token::Minus(a) => Op::Minus(a),
            Token::Mult(a) => Op::Mult(a),
            Token::Div(a) => Op::Div(a),
            Token::Eq(a) => Op::Eq(a),
            Token::Neq(a) => Op::Neq(a),
            _ => panic!("Invalid operator"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Parenthesis(Box<Expr>),
    FunCall(Ident, Vec<Expr>),
    BinOp(Op, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Vec<Expr>),
    While(Box<Expr>, Vec<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub info: TokenData<String>,
    pub type_: Option<Type>,
}

impl Ident {
    pub fn from_token(token_data: TokenData<String>) -> Ident {
        Ident {
            info: token_data,
            type_: None,
        }
    }
}

impl Expr {}

pub enum Defn {
    Expr(Expr),
    VarDecl(Ident, Expr),
    Assign(Ident, Expr),
    FunDef(Ident, Vec<(String, Option<Type>)>, Vec<Expr>),
}

impl Defn {}

pub struct Program {
    pub definitions: Vec<Defn>,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData<T> {
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub data: T,
}

pub fn token_data(lex: &mut Lexer<Token>) -> TokenData<()> {
    lex.extras.pos.column += lex.slice().len();
    let line = lex.extras.pos.line;
    let column = lex.extras.pos.column;
    let length = lex.slice().len();

    TokenData {
        line,
        column,
        length,
        data: (),
    }
}

#[derive(Default)]
pub struct Extras {
    pos: Position,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras=Extras)]
pub enum Token {
    #[regex(r"[ \t\f]", |lex| {
        lex.extras.pos.column += lex.slice().len();
        logos::Skip
    })]
    #[regex( r"\n", |lex| {
        lex.extras.pos.line += 1;
        lex.extras.pos.column = 0;

        logos::Skip
    })]
    #[error]
    Error,
    // literals
    // int literal
    #[regex(r"\d+", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().parse::<i32>().unwrap()}
    })]
    Int(TokenData<i32>),

    // float literal
    #[regex(r"\d+\.\d+", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().parse::<f64>().unwrap()}
    })]
    Float(TokenData<f64>),

    // string literal
    #[regex(r#""[^"]*""#, |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().to_string()}
    })]
    String(TokenData<String>),

    // char literal
    #[regex(r"'[^']'", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().chars().nth(1).unwrap()}
    })]
    Char(TokenData<char>),

    // bool literal
    #[regex(r"true|false", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().parse::<bool>().unwrap()}
    })]
    Bool(TokenData<bool>),

    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().to_string()}
    })]
    Ident(TokenData<String>),

    // operators
    #[token("+", token_data)]
    Plus(TokenData<()>),
    #[token("-", token_data)]
    Minus(TokenData<()>),
    #[token("*", token_data)]
    Mult(TokenData<()>),
    #[token("/", token_data)]
    Div(TokenData<()>),
    #[token("==", token_data)]
    Eq(TokenData<()>),
    #[token("!=", token_data)]
    Neq(TokenData<()>),

    // delimiters
    #[token("(", token_data)]
    LParen(TokenData<()>),
    #[token(")", token_data)]
    RParen(TokenData<()>),
    #[token("{", token_data)]
    LBrace(TokenData<()>),
    #[token("}", token_data)]
    RBrace(TokenData<()>),
    #[token("[", token_data)]
    LBracket(TokenData<()>),
    #[token("]", token_data)]
    RBracket(TokenData<()>),

    #[token(":", token_data)]
    Colon(TokenData<()>),

    #[token(",", token_data)]
    Comma(TokenData<()>),

    #[token("=", token_data)]
    Assign(TokenData<()>),

    // keywords
    #[token("if", token_data)]
    If(TokenData<()>),

    #[token("while", token_data)]
    While(TokenData<()>),

    #[token("return", token_data)]
    Return(TokenData<()>),

    #[token("let", token_data)]
    Let(TokenData<()>),

    #[token("fn", token_data)]
    Fn(TokenData<()>),

    #[token("mut", token_data)]
    Mut(TokenData<()>),
    // primitive types
    #[token("int", token_data)]
    TypInt(TokenData<()>),
    #[token("float", token_data)]
    TypFloat(TokenData<()>),
    #[token("string", token_data)]
    TypString(TokenData<()>),
    #[token("char", token_data)]
    TypChar(TokenData<()>),
    #[token("bool", token_data)]
    TypBool(TokenData<()>),
}

struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    // peaking at the next token
    peek: Option<Token>,
}

struct ParserError {
    line: usize,
    column: usize,
    length: usize,
    message: String,
}

impl ParserError {
    fn new(lexer: &mut Lexer<Token>, message: String) -> ParserError {
        ParserError {
            line: lexer.extras.pos.line,
            column: lexer.extras.pos.column,
            length: lexer.slice().len(),
            message,
        }
    }

    fn to_string(&self) -> String {
        format!(
            "{}:{}:{} {}",
            self.line, self.column, self.length, self.message
        )
    }
}

impl Parser<'_> {
    pub fn new(input: &str) -> Parser<'_> {
        Parser {
            lexer: Token::lexer(input),
            peek: None,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.peek.is_some() {
            let token = self.peek.take();
            return token;
        }
        self.lexer.next()
    }

    pub fn peek(&mut self) -> Option<Token> {
        if self.peek.is_some() {
            return self.peek.clone();
        }
        self.peek = self.lexer.next();
        self.peek.clone()
    }

    pub fn parse_type_decl(&mut self) -> Option<Type> {
        // check if type is going to be supplied
        let token = self.peek()?;

        match token {
            Token::Colon(_) => {}
            _ => return None,
        }
        // consume the colon
        self.next();

        // parse the type
        let token = self.next()?;

        match token {
            Token::TypInt(a) => Some(Type::Int(a)),
            Token::TypFloat(a) => Some(Type::Float(a)),
            Token::TypString(a) => Some(Type::String(a)),
            Token::TypChar(a) => Some(Type::Char(a)),
            Token::TypBool(a) => Some(Type::Bool(a)),
            _ => {
                let error = ParserError::new(&mut self.lexer, "Expected a type".to_string());
                panic!("{}", error.to_string());
            }
        }
    }

    pub fn parse_literal(&mut self) -> Option<Expr> {
        let token = self.peek();
        let literal = match token {
            Some(Token::Int(a)) => Some(Literal::Int(a)),
            Some(Token::Float(a)) => Some(Literal::Float(a)),
            Some(Token::String(a)) => Some(Literal::String(a)),
            Some(Token::Char(a)) => Some(Literal::Char(a)),
            Some(Token::Bool(a)) => Some(Literal::Bool(a)),
            _ => None,
        };

        if literal.is_some() {
            self.next();
            return Some(Expr::Literal(literal?));
        }

        None
    }

    pub fn parse_op(&mut self) -> Option<Op> {
        let token = self.peek();

        let op = match token {
            Some(Token::Plus(a)) => Some(Op::Plus(a)),
            Some(Token::Minus(a)) => Some(Op::Minus(a)),
            Some(Token::Mult(a)) => Some(Op::Mult(a)),
            Some(Token::Div(a)) => Some(Op::Div(a)),
            Some(Token::Eq(a)) => Some(Op::Eq(a)),
            Some(Token::Neq(a)) => Some(Op::Neq(a)),
            _ => None,
        };

        if op.is_some() {
            self.next();
            return op;
        }

        None
    }

    pub fn parse_func_call(&mut self, ident_info: TokenData<String>) -> Option<Expr> {
        let token = self.peek();

        match token {
            Some(Token::LParen(_)) => {
                self.next();
                let mut args = Vec::new();

                loop {
                    let arg = self.parse_expr();

                    if arg.is_none() {
                        let closing_paren = self.next()?;

                        match closing_paren {
                            Token::RParen(_) => {
                                break;
                            }
                            _ => {
                                let error = ParserError::new(
                                    &mut self.lexer,
                                    "Expected closing parenthesis".to_string(),
                                );
                                panic!("{}", error.to_string());
                            }
                        }
                    }

                    args.push(arg.unwrap());

                    let token = self.next()?;

                    match token {
                        Token::RParen(_) => {
                            break;
                        }
                        Token::Comma(_) => {}
                        _ => {
                            let error = ParserError::new(
                                &mut self.lexer,
                                "Expected comma or closing parenthesis".to_string(),
                            );
                            panic!("{}", error.to_string());
                        }
                    }
                }

                return Some(Expr::FunCall(Ident::from_token(ident_info), args));
            }

            _ => None,
        }
    }

    pub fn parse_ident(&mut self) -> Option<Ident> {
        let token = self.peek();

        match token {
            Some(Token::Ident(a)) => {
                self.next();
                Some(Ident::from_token(a))
            }
            _ => None,
        }
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        let literal = self.parse_literal();

        if literal.is_some() {
            let op = self.parse_op();

            return if op.is_some() {
                let op = op.unwrap();
                let right = self.parse_expr()?;
                Some(Expr::BinOp(op, Box::new(literal.unwrap()), Box::new(right)))
            } else {
                Some(literal.unwrap())
            };
        }

        let ident = self.peek();

        match ident {
            Some(Token::Ident(ident_info)) => {
                self.next();
                // check if the next token is a operator
                let op = self.parse_op();

                if op.is_some() {
                    let op = op.unwrap();
                    let right = self.parse_expr()?;
                    return Some(Expr::BinOp(
                        op,
                        Box::new(Expr::Ident(Ident::from_token(ident_info))),
                        Box::new(right),
                    ));
                }

                // check if its a function call

                let func_call = self.parse_func_call(ident_info.clone());

                if func_call.is_none() {
                    return Some(Expr::Ident(Ident::from_token(ident_info)));
                }

                if op.is_some() {
                    let op = op.unwrap();
                    let right = self.parse_expr()?;
                    return Some(Expr::BinOp(op, Box::new(func_call?), Box::new(right)));
                }

                return Some(func_call?);
            }
            _ => {
                //its ok, it can still be an opening paren
            }
        }

        // check if its an opening paren
        let token = self.peek()?;

        match token {
            Token::LParen(_) => {
                self.next();
                let left = self.parse_expr()?;

                let op = self.parse_op();

                if op.is_some() {
                    let op = op.unwrap();
                    let right = self.parse_expr()?;

                    // check if the next token is a closing paren
                    let closing_paren = self.next()?;

                    match closing_paren {
                        Token::RParen(_) => {
                            return Some(Expr::Parenthesis(Box::new(Expr::BinOp(
                                op,
                                Box::new(left),
                                Box::new(right),
                            ))));
                        }
                        _ => {
                            let error = ParserError::new(
                                &mut self.lexer,
                                "Expected closing parenthesis".to_string(),
                            );
                            panic!("{}", error.to_string());
                        }
                    }
                }

                let closing_paren = self.next()?;

                match closing_paren {
                    Token::RParen(_) => {
                        return Some(Expr::Parenthesis(Box::new(left)));
                    }
                    _ => {
                        let error = ParserError::new(
                            &mut self.lexer,
                            "Expected closing parenthesis".to_string(),
                        );
                        panic!("{}", error.to_string());
                    }
                }
            }

            _ => {
                // not expression found so return none
                return None;
                // let error = ParserError::new(
                //     &mut self.lexer,
                //     "Expected expression".to_string(),
                // );
                // panic!("{}", error.to_string());
            }
        }
    }
}

pub fn lex(input: &str) {
    let mut parser = Parser::new(input);

    println!("{:#?}", parser.parse_expr());

    // match parser.parse_expr(){
    //     Some(expr) => {
    //         println!("{:?}", expr);
    //     }
    //     None => {
    //         println!("No expression found");
    //     }
    // }
}

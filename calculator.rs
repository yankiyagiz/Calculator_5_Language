use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]

enum Token {

    Number(f64),
    Variable(String),
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
}

impl Token {

    fn precedence(&self) -> u8 {

        match self {

            Token::Plus | Token::Minus => 1,
            Token::Multiply | Token::Divide => 2,
            _ => 0,
        }
    }
}

fn tokenize(expr: &str) -> Result<Vec<Token>, String> {

    let mut tokens = Vec::new();
    let mut chars = expr.chars().peekable();
    
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }
        if c.is_alphabetic() {
            let mut var = String::new();
            var.push(c);
            chars.next();
            while let Some(&ch) = chars.peek() {
                if ch.is_alphanumeric() || ch == '_' {
                    var.push(ch);
                    chars.next();
                } else {
                    break;
                }
            }
            tokens.push(Token::Variable(var));
        } else if c.is_ascii_digit() || c == '.' {
            let mut num_str = String::new();
            let mut has_dot = false;
            while let Some(&ch) = chars.peek() {
                if ch == '.' {
                    if has_dot {
                        return Err("Invalid number with multiple dots".to_string());
                    }
                    has_dot = true;
                    num_str.push(ch);
                    chars.next();
                } else if ch.is_ascii_digit() {
                    num_str.push(ch);
                    chars.next();
                } else {
                    break;
                }
            }
            if num_str.is_empty() {
                return Err("Unexpected character".to_string());
            }
            match num_str.parse::<f64>() {
                Ok(num) => tokens.push(Token::Number(num)),
                Err(_) => return Err(format!("Invalid number '{}'", num_str)),
            }
        } else {
            match c {
                '+' => tokens.push(Token::Plus),
                '-' => tokens.push(Token::Minus),
                '*' => tokens.push(Token::Multiply),
                '/' => tokens.push(Token::Divide),
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                _ => return Err(format!("Unexpected character '{}'", c)),
            }
            chars.next();
        }
    }
    Ok(tokens)
}

fn shunting_yard(tokens: &[Token]) -> Result<Vec<Token>, String> {
    let mut output = Vec::new();
    let mut stack = Vec::new();
    let mut expect_operand = true;

    for token in tokens {
        match token {
            Token::Minus if expect_operand => {
                output.push(Token::Number(0.0));
                stack.push(Token::Minus);
                expect_operand = true;
                continue;
            }
            _ => (),
        }

        match token {
            Token::Number(_) | Token::Variable(_) => {
                output.push(token.clone());
                expect_operand = false;
            }
            Token::LParen => {
                stack.push(token.clone());
                expect_operand = true;
            }
            Token::RParen => {
                let mut found_lparen = false;
                while let Some(top) = stack.pop() {
                    if top == Token::LParen {
                        found_lparen = true;
                        break;
                    }
                    output.push(top);
                }
                if !found_lparen {
                    return Err("Mismatched parentheses".to_string());
                }
                expect_operand = false;
            }

            Token::Plus | Token::Minus | Token::Multiply | Token::Divide => {

                while let Some(top) = stack.last() {

                    if top == &Token::LParen {
                        break;
                    }

                    if top.precedence() >= token.precedence() {

                        let op = stack.pop().unwrap();
                        output.push(op);

                    } else {

                        break;
                    }
                }
                stack.push(token.clone());
                expect_operand = true;
            }
            
        }
    }

    while let Some(op) = stack.pop() {

        if op == Token::LParen || op == Token::RParen {

            return Err("Mismatched parentheses".to_string());
        }
        output.push(op);
    }

    Ok(output)
}

fn evaluate_postfix(postfix: &[Token], variables: &HashMap<String, f64>) -> Result<f64, String> {

    let mut stack = Vec::new();
    for token in postfix {

        match token {

            Token::Number(n) => stack.push(*n),
            Token::Variable(var) => {

                if let Some(val) = variables.get(var) {
                    stack.push(*val);
                } else {
                    return Err(format!("not defined '{}'", var));
                }
            }
            Token::Plus => {
                let b = stack.pop().ok_or("Missing operand for +")?;
                let a = stack.pop().ok_or("Missing operand for +")?;
                stack.push(a + b);
            }
            Token::Minus => {
                let b = stack.pop().ok_or("Missing operand for -")?;
                let a = stack.pop().ok_or("Missing operand for -")?;
                stack.push(a - b);
            }
            Token::Multiply => {
                let b = stack.pop().ok_or("Missing operand for *")?;
                let a = stack.pop().ok_or("Missing operand for *")?;
                stack.push(a * b);
            }
            Token::Divide => {
                let b = stack.pop().ok_or("Missing operand for /")?;
                let a = stack.pop().ok_or("Missing operand for /")?;
                if b == 0.0 {
                    return Err("Division by zero".to_string());
                }
                stack.push(a / b);
            }
            _ => return Err("Invalid token in postfix expression".to_string()),
        }
    }
    if stack.len() != 1 {

        return Err("Invalid expression".to_string());
    }
    Ok(stack[0])
}

fn valid_variable_name(name: &str) -> bool {

    let mut chars = name.chars();

    if let Some(c) = chars.next() {

        if !c.is_alphabetic() {

            return false;
        }
        for c in chars {

            if !c.is_alphanumeric() && c != '_' {
                return false;
            }
        }
        true
    } else {
        false
    }
}

fn process_input(input: &str, variables: &mut HashMap<String, f64>) -> Result<Option<f64>, String> {

    if let Some(equal_pos) = input.find('=') {

        let var_part = input[..equal_pos].trim();
        let expr_part = input[equal_pos + 1..].trim();

        if !valid_variable_name(var_part) {

            return Err(format!("Invalid variable name '{}'", var_part));
        }

        let tokens = tokenize(expr_part)?;
        let postfix = shunting_yard(&tokens)?;
        let value = evaluate_postfix(&postfix, variables)?;

        variables.insert(var_part.to_string(), value);
        Ok(None)

    } else {

        let tokens = tokenize(input)?;
        let postfix = shunting_yard(&tokens)?;
        let value = evaluate_postfix(&postfix, variables)?;

        Ok(Some(value))
    }
}

fn main() {

    let mut variables = HashMap::new();
    loop {

        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();

        match io::stdin().read_line(&mut input) {

            Ok(_) => {

                let input = input.trim();
                if input.is_empty() {
                    continue;
                }
                match process_input(input, &mut variables) {
                    Ok(Some(val)) => println!("{}", val),
                    Ok(None) => {}
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            
            Err(e) => eprintln!("Error reading input: {}", e),
        }
    }
}
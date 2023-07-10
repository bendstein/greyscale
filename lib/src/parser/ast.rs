use std::rc::Rc;

use crate::location::Location;

#[derive(Debug)]
pub struct AST {
    pub statements: Vec<Node>
}

impl AST {
    pub fn new(statements: Vec<Node>) -> Self {
        Self {
            statements
        }
    }

    pub fn debug_string(&self, program: Rc<Vec<&str>>) -> String {
        let debug_strings: Vec<String> = self.statements.iter()
            .map(|node| node.debug_string(4, Rc::clone(&program)))
            .collect();

        format!("Parse Tree:\n{}", debug_strings.join("\n"))
    }
}

#[derive(Debug)]
pub enum Node {
    Expression(Box<expression::ExprNode>),
    Statement(Box<statement::StmtNode>)
}

impl Node {
    pub fn name(&self) -> String {
        match self {
            Node::Expression(expr) => format!("Expression ({})", expr.name()),
            Node::Statement(stmt) => format!("Statement ({})", stmt.name()),
        }
    }

    pub fn location(&self) -> Location {
        match self {
            Node::Expression(expr) => expr.location(),
            Node::Statement(stmt) => stmt.location()
        }
    }

    pub fn debug_string(&self, indent: usize, program: Rc<Vec<&str>>) -> String {
        match self {
            Node::Expression(expr) => expr.debug_string(indent, program),
            Node::Statement(stmt) => stmt.debug_string(indent, program)
        }
    }
}

#[derive(Debug)]
pub enum LiteralType {
    Void,
    Null,
    Boolean(bool),
    String(String),
    Double(f64),
    Integer(i64)
}

impl LiteralType {
    pub fn name(&self) -> String {
        match self {
            LiteralType::Void => String::from("Void"),
            LiteralType::Null => String::from("Null"),
            LiteralType::Boolean(_) => String::from("Boolean"),
            LiteralType::String(_) => String::from("String"),
            LiteralType::Double(_) => String::from("Double"),
            LiteralType::Integer(_) => String::from("Integer"),
        }
    }

    pub fn debug_string(&self, indent: usize) -> String {
        match self {
            LiteralType::Void | LiteralType::Null => self.name(),
            LiteralType::Boolean(v) => format!("{:indent$}|---- {}({})", "", self.name(), v, indent = indent - 4),
            LiteralType::String(v) => format!("{:indent$}|---- {}({})", "", self.name(), v, indent = indent - 4),
            LiteralType::Double(v) => format!("{:indent$}|---- {}({})", "", self.name(), v, indent = indent - 4),
            LiteralType::Integer(v) => format!("{:indent$}|---- {}({})", "", self.name(), v, indent = indent - 4),
        }
    }
}

pub mod expression {
    use std::rc::Rc;

    use crate::location::Location;
    use crate::token::Token;
    use super::LiteralType;
    use super::statement;

    #[derive(Debug)]
    pub enum ExprNode {
        Binary(Binary, Location),
        Unary(Unary, Location),
        Assignment(Assignment, Location),
        Literal(Literal, Location),
        InterpolatedString(InterpolatedString, Location),
        Identifier(Identifier, Location),
        Function(Function, Location),
        Call(Call, Location)
    }

    impl ExprNode {
        pub fn name(&self) -> String {
            match self {
                Self::Binary(_, _) => String::from("Binary"),
                Self::Unary(_, _) => String::from("Unary"),
                Self::Assignment(_, _) => String::from("Assignment"),
                Self::Literal(lit, _) => format!("{} Literal", lit.value.name()),
                Self::InterpolatedString(_, _) => String::from("Interpolated String"),
                Self::Identifier(_, _) => String::from("Identifier"),
                Self::Function(_, _) => String::from("Function"),
                Self::Call(_, _) => String::from("Call")
            }
        }

        pub fn location(&self) -> Location {
            match self {
                Self::Binary(_, l) => *l,
                Self::Unary(_, l) => *l,
                Self::Assignment(_, l) => *l,
                Self::Literal(_, l) => *l,
                Self::InterpolatedString(_, l) => *l,
                Self::Identifier(_, l) => *l,
                Self::Function(_, l) => *l,
                Self::Call(_, l) => *l
            }
        }

        pub fn debug_string(&self, indent: usize, program: Rc<Vec<&str>>) -> String {
            match self {
                ExprNode::Binary(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let lhs = expr.left.debug_string(indent + 6, Rc::clone(&program));
                    let rhs_strings: Vec<String> = expr.right.iter()
                        .map(|rhs| {
                            let op_string = format!("{:indent$}|---- {}", "", rhs.op.token_type().as_program_string(&program), indent = indent + 2);
                            let expr_string = rhs.right.debug_string(indent + 6, Rc::clone(&program));
                            format!("{op_string}\n{expr_string}")
                        })
                        .collect();

                    format!("{initial}\n{lhs}\n{}", rhs_strings.join("\n"))
                },
                ExprNode::Unary(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let op_strings: Vec<String> = expr.ops.iter()
                        .map(|op| format!("{:indent$}|---- {}", "", op.token_type().as_program_string(&program), indent = indent + 2))
                        .collect();
                    let rhs = expr.expr.debug_string(indent + 6, program);

                    format!("{initial}\n{}\n{rhs}", op_strings.join("\n"))
                },
                ExprNode::Assignment(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let id = format!("{:indent$}|---- {}", "", expr.id.token_type().as_program_string(&program), indent = indent + 2);
                    let op = format!("{:indent$}|---- {}", "", expr.assignment_type.token_type().as_program_string(&program), indent = indent + 2);
                    let expr = expr.assignment.debug_string(indent + 6, program);

                    format!("{initial}\n{id}\n{op}\n{expr}")
                },
                ExprNode::Literal(expr, _) => {
                    expr.value.debug_string(indent)
                },
                ExprNode::InterpolatedString(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let segment_strings: Vec<String> = expr.segments.iter()
                        .map(|segment| segment.debug_string(indent + 6, Rc::clone(&program)))
                        .collect();

                    format!("{initial}\n{}", segment_strings.join("\n"))
                },
                ExprNode::Identifier(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    format!("{initial}\n{:indent$}|---- {}", "", expr.id.token_type().as_program_string(&program), indent = indent + 2)
                },
                ExprNode::Function(_expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    initial
                },
                ExprNode::Call(expr, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    let callable_str = expr.callable.debug_string(indent + 6, Rc::clone(&program));
                    let args_strings: Vec<String> = expr.calls.iter()
                        .map(|args| {
                            let arg_strings: Vec<String> = args.iter()
                                .map(|arg| arg.debug_string(indent + 12, Rc::clone(&program)))
                                .collect();

                            format!("{:indent$}|---- Args:\n{}", "", arg_strings.join("\n"), indent = indent + 2)
                        })
                        .collect();

                    format!("{initial}\n{callable_str}\n{}", args_strings.join("\n"))
                },
            }
        }
    }

    #[derive(Debug)]
    pub struct Binary {
        pub left: Box<ExprNode>, 
        pub right: Vec<BinaryRHS>
    }

    #[derive(Debug)]
    pub struct BinaryRHS {
        pub op: Token,
        pub right: Box<ExprNode>
    }

    #[derive(Debug)]
    pub struct Unary { 
        pub ops: Vec<Token>,
        pub expr: Box<ExprNode> 
    }

    #[derive(Debug)]
    pub struct Assignment {
        pub id: Token, 
        pub assignment_type: Token, 
        pub assignment: Box<ExprNode>
    }

    #[derive(Debug)]
    pub struct Literal {
        pub value: LiteralType
    }

    #[derive(Debug)]
    pub struct InterpolatedString {
        pub segments: Vec<ExprNode>
    }

    #[derive(Debug)]
    pub struct Identifier {
        pub id: Token
    }

    #[derive(Debug)]
    pub struct Function {
        pub args: Vec<Token>, 
        pub body: Box<statement::StmtNode>
    }

    #[derive(Debug)]
    pub struct Call {
        pub callable: Box<ExprNode>,
        pub calls: Vec<Vec<ExprNode>>
    }
}

pub mod statement {
    use std::rc::Rc;

    use crate::{token::Token, location::Location};
    use super::expression::ExprNode;

    #[derive(Debug)]
    pub enum StmtNode {
        Block(Block, Location, Location),
        Conditional(Conditional, Location, Location),
        Keyword(Keyword, Location, Location),
        Declaration(Declaration, Location, Location),
        Expression(Expression, Location, Location),
        For(For, Location, Location),
        While(While, Location, Location),
        Loop(Loop, Location, Location),
        Return(Return, Location, Location),
        Print(Print, Location, Location)
    }

    impl StmtNode {
        pub fn name(&self) -> String {
            match self {
                StmtNode::Block(_, _, _) => String::from("Block"),
                StmtNode::Conditional(_, _, _) => String::from("Conditional"),
                StmtNode::Keyword(_, _, _) => String::from("Keyword"),
                StmtNode::Declaration(_, _, _) => String::from("Declaration"),
                StmtNode::Expression(_, _, _) => String::from("Expression"),
                StmtNode::For(_, _, _) => String::from("For"),
                StmtNode::While(_, _, _) => String::from("While"),
                StmtNode::Loop(_, _, _) => String::from("Loop"),
                StmtNode::Return(_, _, _) => String::from("Return"),
                StmtNode::Print(_, _, _) => String::from("Print"),
            }
        }

        pub fn location(&self) -> Location {
            match self {
                Self::Block(_, l, _) => *l,
                Self::Conditional(_, l, _) => *l,
                Self::Keyword(_, l, _) => *l,
                Self::Declaration(_, l, _) => *l,
                Self::Expression(_, l, _) => *l,
                Self::For(_, l, _) => *l,
                Self::While(_, l, _) => *l,
                Self::Loop(_, l, _) => *l,
                Self::Return(_, l, _) => *l,
                Self::Print(_, l, _) => *l
            }
        }

        pub fn end_location(&self) -> Location {
            match self {
                Self::Block(_, _, l) => *l,
                Self::Conditional(_, _, l) => *l,
                Self::Keyword(_, _, l) => *l,
                Self::Declaration(_, _, l) => *l,
                Self::Expression(_, _, l) => *l,
                Self::For(_, _, l) => *l,
                Self::While(_, _, l) => *l,
                Self::Loop(_, _, l) => *l,
                Self::Return(_, _, l) => *l,
                Self::Print(_, _, l) => *l
            }
        }
  
        pub fn debug_string(&self, indent: usize, program: Rc<Vec<&str>>) -> String {
            match self {
                StmtNode::Block(stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    let stmts_strings: Vec<String> = stmt.statements.iter()
                        .map(|stmt| stmt.debug_string(indent + 6, Rc::clone(&program)))
                        .collect();

                    format!("{initial}\n{}", stmts_strings.join("\n"))
                },
                StmtNode::Conditional(_stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    initial
                },
                StmtNode::Keyword(_stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    
                    initial
                },
                StmtNode::Declaration(stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let id = format!("{:indent$}|---- {}", "", stmt.id.token_type().as_program_string(&program), indent = indent + 2);

                    if let Some(assign) = &stmt.assignment {
                        let assign_string = assign.debug_string(indent + 6, program);
                        format!("{initial}\n{id}\n{assign_string}")
                    }
                    else {
                        format!("{initial}\n{id}")
                    }
                },
                StmtNode::Expression(stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let expr = stmt.expression.debug_string(indent + 6, program);
                    
                    format!("{initial}\n{expr}")
                },
                StmtNode::For(_stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    initial
                },
                StmtNode::While(_stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    initial
                },
                StmtNode::Loop(_stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    initial
                },
                StmtNode::Return(stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);

                    if let Some(expr) = &stmt.expression {
                        let expr_string = expr.debug_string(indent + 4, program);

                        format!("{initial}\n{expr_string}")
                    }
                    else {
                         initial
                    }
                },
                StmtNode::Print(stmt, _, _) => {
                    let initial = format!("{:indent$}|---- {}", "", self.name(), indent = indent - 4);
                    let expr = stmt.expression.debug_string(indent + 6, program);
                    
                    format!("{initial}\n{expr}")
                },
            }
        }
    }

    #[derive(Debug)]
    pub struct ConditionalBranch {
        pub condition: ExprNode,
        pub body: StmtNode
    }

    #[derive(Debug)]
    pub struct Block {
        pub statements: Vec<StmtNode>
    }
    
    #[derive(Debug)]
    pub struct Conditional {
        pub branches: Vec<ConditionalBranch>, 
        pub branch_else: Option<Box<StmtNode>>
    }
    
    #[derive(Debug)]
    pub struct Keyword {
        pub keyword: Token
    }
    
    #[derive(Debug)]
    pub struct Declaration {
        pub id: Token, 
        pub assignment: Option<Box<ExprNode>>
    }
    
    #[derive(Debug)]
    pub struct Expression {
        pub expression: Box<ExprNode>
    }

    #[derive(Debug)]
    pub struct For {
        pub declaration: Option<Box<StmtNode>>, 
        pub condition: Option<Box<ExprNode>>, 
        pub action: Option<Box<ExprNode>>, 
        pub body: Box<StmtNode>
    }

    #[derive(Debug)]
    pub struct While {
        pub condition: Option<Box<ExprNode>>, 
        pub body: Box<StmtNode>
    }

    #[derive(Debug)]
    pub struct Loop {
        pub body: Box<StmtNode>
    }

    #[derive(Debug)]
    pub struct Return {
        pub expression: Option<Box<ExprNode>>
    }

    #[derive(Debug)]
    pub struct Print {
        pub expression: Box<ExprNode>
    }
}
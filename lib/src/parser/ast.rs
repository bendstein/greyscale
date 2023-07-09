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
}

pub mod expression {
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
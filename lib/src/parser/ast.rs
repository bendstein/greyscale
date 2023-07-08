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
    use crate::token::Token;
    use super::LiteralType;
    use super::statement;

    #[derive(Debug)]
    pub enum ExprNode {
        Binary(Binary),
        Unary(Unary),
        Assignment(Assignment),
        Literal(Literal),
        InterpolatedString(InterpolatedString),
        Identifier(Identifier),
        Function(Function),
        Call(Call)
    }

    impl ExprNode {
        pub fn name(&self) -> String {
            match self {
                Self::Binary(_) => String::from("Binary"),
                Self::Unary(_) => String::from("Unary"),
                Self::Assignment(_) => String::from("Assignment"),
                Self::Literal(lit) => format!("{} Literal", lit.value.name()),
                Self::InterpolatedString(_) => String::from("Interpolated String"),
                Self::Identifier(_) => String::from("Identifier"),
                Self::Function(_) => String::from("Function"),
                Self::Call(_) => String::from("Call")
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
    use crate::token::Token;
    use super::expression::ExprNode;

    #[derive(Debug)]
    pub enum StmtNode {
        Block(Block),
        Conditional(Conditional),
        Keyword(Keyword),
        Declaration(Declaration),
        Expression(Expression),
        For(For),
        While(While),
        Loop(Loop),
        Return(Return),
        Print(Print)
    }

    impl StmtNode {
        pub fn name(&self) -> String {
            match self {
                StmtNode::Block(_) => String::from("Block"),
                StmtNode::Conditional(_) => String::from("Conditional"),
                StmtNode::Keyword(_) => String::from("Keyword"),
                StmtNode::Declaration(_) => String::from("Declaration"),
                StmtNode::Expression(_) => String::from("Expression"),
                StmtNode::For(_) => String::from("For"),
                StmtNode::While(_) => String::from("While"),
                StmtNode::Loop(_) => String::from("Loop"),
                StmtNode::Return(_) => String::from("Return"),
                StmtNode::Print(_) => String::from("Print"),
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
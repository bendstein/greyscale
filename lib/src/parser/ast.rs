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

#[derive(Debug)]
pub enum LiteralType {
    Void,
    Null,
    Boolean(bool),
    String(String),
    Double(f64),
    Integer(i64)
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
        Return(Return)
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
}
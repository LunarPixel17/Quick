package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"unicode"
)

// VERSION information
const VERSION = "0.2.0"

// Token types
type TokenType int

const (
	TOKEN_IDENTIFIER TokenType = iota
	TOKEN_NUMBER
	TOKEN_STRING
	TOKEN_KEYWORD
	TOKEN_OPERATOR
	TOKEN_LPAREN
	TOKEN_RPAREN
	TOKEN_LBRACE
	TOKEN_RBRACE
	TOKEN_LBRACKET
	TOKEN_RBRACKET
	TOKEN_SEMICOLON
	TOKEN_COMMA
	TOKEN_DOT
	TOKEN_COLON
	TOKEN_EOF
)

// Keywords in Quick language
var keywords = map[string]bool{
	"func":     true,
	"var":      true,
	"const":    true,
	"if":       true,
	"else":     true,
	"for":      true,
	"while":    true,
	"return":   true,
	"break":    true,
	"continue": true,
	"import":   true,
	"package":  true,
	"app":      true,
	"lib":      true,
	"print":    true,
	"true":     true,
	"false":    true,
	"null":     true,
	"int":      true,
	"float":    true,
	"string":   true,
	"bool":     true,
	"struct":   true,
	"interface":true,
}

// Token struct
type Token struct {
	Type    TokenType
	Value   string
	Line    int
	Column  int
}

// Lexer struct
type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           byte
	line         int
	column       int
}

// Creates a new Lexer
func NewLexer(input string) *Lexer {
	l := &Lexer{input: input, line: 1, column: 0}
	l.readChar()
	return l
}

// Read the next character
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
	l.column++
	
	// Track line numbers
	if l.ch == '\n' {
		l.line++
		l.column = 0
	}
}

// Peek at the next character
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// Skip whitespace
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// Skip comments
func (l *Lexer) skipComment() {
	// Single line comments
	if l.ch == '/' && l.peekChar() == '/' {
		for l.ch != '\n' && l.ch != 0 {
			l.readChar()
		}
		if l.ch != 0 {
			l.readChar() // Skip the newline
		}
	}
	
	// Multi-line comments
	if l.ch == '/' && l.peekChar() == '*' {
		l.readChar() // Skip /
		l.readChar() // Skip *
		
		for !(l.ch == '*' && l.peekChar() == '/') && l.ch != 0 {
			l.readChar()
		}
		
		if l.ch != 0 {
			l.readChar() // Skip *
			l.readChar() // Skip /
		}
	}
}

// Read an identifier
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) || (l.position > position && isDigit(l.ch)) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// Read a number
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	
	// Handle decimal numbers
	if l.ch == '.' && isDigit(l.peekChar()) {
		l.readChar()
		for isDigit(l.ch) {
			l.readChar()
		}
	}
	
	return l.input[position:l.position]
}

// Read a string
func (l *Lexer) readString() string {
	l.readChar() // Skip the opening quote
	position := l.position
	
	for l.ch != '"' && l.ch != 0 {
		if l.ch == '\\' && l.peekChar() == '"' {
			l.readChar() // Skip the backslash
		}
		l.readChar()
	}
	
	str := l.input[position:l.position]
	l.readChar() // Skip the closing quote
	return str
}

// Get the next token
func (l *Lexer) NextToken() Token {
	var tok Token
	
	l.skipWhitespace()
	
	// Skip comments
	if (l.ch == '/' && l.peekChar() == '/') || (l.ch == '/' && l.peekChar() == '*') {
		l.skipComment()
		return l.NextToken()
	}
	
	// Store current position for error reporting
	line := l.line
	column := l.column
	
	switch l.ch {
	case '(':
		tok = Token{TOKEN_LPAREN, "(", line, column}
	case ')':
		tok = Token{TOKEN_RPAREN, ")", line, column}
	case '{':
		tok = Token{TOKEN_LBRACE, "{", line, column}
	case '}':
		tok = Token{TOKEN_RBRACE, "}", line, column}
	case '[':
		tok = Token{TOKEN_LBRACKET, "[", line, column}
	case ']':
		tok = Token{TOKEN_RBRACKET, "]", line, column}
	case ';':
		tok = Token{TOKEN_SEMICOLON, ";", line, column}
	case ',':
		tok = Token{TOKEN_COMMA, ",", line, column}
	case '.':
		tok = Token{TOKEN_DOT, ".", line, column}
	case ':':
		tok = Token{TOKEN_COLON, ":", line, column}
	case '+':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "+=", line, column}
		} else if l.peekChar() == '+' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "++", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "+", line, column}
		}
	case '-':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "-=", line, column}
		} else if l.peekChar() == '-' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "--", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "-", line, column}
		}
	case '*':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "*=", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "*", line, column}
		}
	case '/':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "/=", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "/", line, column}
		}
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "==", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "=", line, column}
		}
	case '!':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "!=", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "!", line, column}
		}
	case '<':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "<=", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "<", line, column}
		}
	case '>':
		if l.peekChar() == '=' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, ">=", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, ">", line, column}
		}
	case '&':
		if l.peekChar() == '&' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "&&", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "&", line, column}
		}
	case '|':
		if l.peekChar() == '|' {
			l.readChar()
			tok = Token{TOKEN_OPERATOR, "||", line, column}
		} else {
			tok = Token{TOKEN_OPERATOR, "|", line, column}
		}
	case '"':
		tok = Token{TOKEN_STRING, l.readString(), line, column}
	case 0:
		tok = Token{TOKEN_EOF, "", line, column}
	default:
		if isLetter(l.ch) {
			identifier := l.readIdentifier()
			if keywords[identifier] {
				tok = Token{TOKEN_KEYWORD, identifier, line, column}
			} else {
				tok = Token{TOKEN_IDENTIFIER, identifier, line, column}
			}
			return tok
		} else if isDigit(l.ch) {
			tok = Token{TOKEN_NUMBER, l.readNumber(), line, column}
			return tok
		} else {
			tok = Token{TOKEN_OPERATOR, string(l.ch), line, column}
		}
	}
	
	l.readChar()
	return tok
}

// Helper functions
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// AST Node interface
type Node interface {
	TokenLiteral() string
	String() string
}

// Statement interface
type Statement interface {
	Node
	statementNode()
}

// Expression interface
type Expression interface {
	Node
	expressionNode()
}

// Program struct - root node of every AST
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// Identifier expression
type Identifier struct {
	Token Token
	Value string
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) TokenLiteral() string { return i.Token.Value }
func (i *Identifier) String() string { return i.Value }

// LetStatement for variable declarations
type LetStatement struct {
	Token Token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode() {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Value }
func (ls *LetStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

// ReturnStatement
type ReturnStatement struct {
	Token       Token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Value }
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer
	out.WriteString(rs.TokenLiteral() + " ")
	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}
	out.WriteString(";")
	return out.String()
}

// ExpressionStatement
type ExpressionStatement struct {
	Token      Token
	Expression Expression
}

func (es *ExpressionStatement) statementNode() {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Value }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// IntegerLiteral
type IntegerLiteral struct {
	Token Token
	Value int64
}

func (il *IntegerLiteral) expressionNode() {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Value }
func (il *IntegerLiteral) String() string { return il.Token.Value }

// StringLiteral
type StringLiteral struct {
	Token Token
	Value string
}

func (sl *StringLiteral) expressionNode() {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Value }
func (sl *StringLiteral) String() string { return "\"" + sl.Value + "\"" }

// PrefixExpression
type PrefixExpression struct {
	Token    Token
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) expressionNode() {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Value }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	return out.String()
}

// InfixExpression
type InfixExpression struct {
	Token    Token
	Left     Expression
	Operator string
	Right    Expression
}

func (ie *InfixExpression) expressionNode() {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Value }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	return out.String()
}

// Parser struct
type Parser struct {
	l              *Lexer
	curToken       Token
	peekToken      Token
	errors         []string
	prefixParseFns map[TokenType]prefixParseFn
	infixParseFns  map[TokenType]infixParseFn
}

type (
	prefixParseFn func() Expression
	infixParseFn  func(Expression) Expression
)

// Precedence levels
const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(X)
	INDEX       // array[index]
)

var precedences = map[string]int{
	"==": EQUALS,
	"!=": EQUALS,
	"<":  LESSGREATER,
	">":  LESSGREATER,
	"<=": LESSGREATER,
	">=": LESSGREATER,
	"+":  SUM,
	"-":  SUM,
	"*":  PRODUCT,
	"/":  PRODUCT,
	"(":  CALL,
	"[":  INDEX,
}

// Create a new Parser
func NewParser(l *Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}
	
	// Initialize prefix parse functions
	p.prefixParseFns = make(map[TokenType]prefixParseFn)
	p.registerPrefix(TOKEN_IDENTIFIER, p.parseIdentifier)
	p.registerPrefix(TOKEN_NUMBER, p.parseIntegerLiteral)
	p.registerPrefix(TOKEN_STRING, p.parseStringLiteral)
	
	// Initialize infix parse functions
	p.infixParseFns = make(map[TokenType]infixParseFn)
	
	// Read two tokens to initialize curToken and peekToken
	p.nextToken()
	p.nextToken()
	
	return p
}

func (p *Parser) registerPrefix(tokenType TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// Move to the next token
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// Parse expression
func (p *Parser) parseIdentifier() Expression {
	return &Identifier{Token: p.curToken, Value: p.curToken.Value}
}

// Parse integer literal
func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteral{Token: p.curToken}
	
	// For simplicity, not handling errors here
	var value int64 = 0
	for _, ch := range p.curToken.Value {
		if unicode.IsDigit(ch) {
			value = value*10 + int64(ch-'0')
		}
	}
	
	lit.Value = value
	return lit
}

// Parse string literal
func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.curToken, Value: p.curToken.Value}
}

// Compiler struct
type Compiler struct {
	parser     *Parser
	output     string
	tempDir    string
	buildCache map[string]string
}

// Create a new compiler
func NewCompiler(parser *Parser) *Compiler {
	tempDir, _ := ioutil.TempDir("", "quick-compiler")
	return &Compiler{
		parser:     parser,
		tempDir:    tempDir,
		buildCache: make(map[string]string),
	}
}

// Compile Quick source to Go
func (c *Compiler) compileToGo(ast *Program) string {
	var out bytes.Buffer
	
	// Generate basic Go code header
	out.WriteString("// Generated by Quick compiler v" + VERSION + "\n")
	out.WriteString("package main\n\n")
	out.WriteString("import (\n")
	out.WriteString("\t\"fmt\"\n")
	out.WriteString(")\n\n")
	
	// TODO: Implement full code generation
	out.WriteString("func main() {\n")
	out.WriteString("\tfmt.Println(\"Compiled Quick Program\")\n")
	out.WriteString("}\n")
	
	return out.String()
}

// Compile Quick to executable
func (c *Compiler) Compile(filename string, outfile string) error {
	// Read the Quick source file
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to read source file: %v", err)
	}
	
	// Create lexer and parser
	lexer := NewLexer(string(source))
	parser := NewParser(lexer)
	
	// TODO: Parse and build AST
	// ast := parser.ParseProgram()
	
	// For now, just generate a simple Go program
	goSource := `// Generated by Quick compiler
package main

import (
	"fmt"
)

func main() {
	fmt.Println("Hello from Quick compiled program!")
}
`
	
	// Write Go source to temp file
	goFile := filepath.Join(c.tempDir, "output.go")
	err = ioutil.WriteFile(goFile, []byte(goSource), 0644)
	if err != nil {
		return fmt.Errorf("failed to write Go source: %v", err)
	}
	
	// Compile Go to executable
	cmd := exec.Command("go", "build", "-o", outfile, goFile)
	err = cmd.Run()
	if err != nil {
		return fmt.Errorf("go compilation failed: %v", err)
	}
	
	fmt.Printf("Successfully compiled %s to %s\n", filename, outfile)
	return nil
}

// Clean up temp files
func (c *Compiler) Cleanup() {
	os.RemoveAll(c.tempDir)
}

// Command-line interface
func main() {
	fmt.Println("Quick Programming Language v" + VERSION)
	fmt.Println("Copyright (C) Xbitlet, Quick Lang")
	
	if len(os.Args) < 2 {
		printUsage()
		return
	}
	
	switch os.Args[1] {
	case "run":
		if len(os.Args) < 3 {
			fmt.Println("Error: Missing source file")
			printUsage()
			return
		}
		runFile(os.Args[2])
		
	case "build":
		if len(os.Args) < 3 {
			fmt.Println("Error: Missing source file")
			printUsage()
			return
		}
		
		outfile := "a.out"
		if len(os.Args) >= 4 {
			outfile = os.Args[3]
		}
		
		buildFile(os.Args[2], outfile)
		
	case "tokens":
		if len(os.Args) < 3 {
			fmt.Println("Error: Missing source file")
			printUsage()
			return
		}
		printTokens(os.Args[2])
		
	case "help":
		printUsage()
		
	case "version":
		fmt.Println("Version:", VERSION)
		
	default:
		fmt.Println("Unknown command:", os.Args[1])
		printUsage()
	}
}

// Print usage information
func printUsage() {
	fmt.Println("\nUsage:")
	fmt.Println("  quick run <file>              Run a Quick program")
	fmt.Println("  quick build <file> [outfile]  Compile a Quick program")
	fmt.Println("  quick tokens <file>           Show tokens from a Quick file")
	fmt.Println("  quick help                    Show this help message")
	fmt.Println("  quick version                 Show version information")
}

// Run a Quick program
func runFile(filename string) {
	// Create a temp executable name
	tempExe := filepath.Join(os.TempDir(), "quick_temp_"+strings.Replace(filepath.Base(filename), filepath.Ext(filename), "", 1))
	if runtime := getRuntime(); runtime == "windows" {
		tempExe += ".exe"
	}
	
	// Build the file
	err := buildFile(filename, tempExe)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	
	// Run the executable
	cmd := exec.Command(tempExe)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	
	err = cmd.Run()
	if err != nil {
		fmt.Printf("Error running program: %v\n", err)
	}
	
	// Clean up
	os.Remove(tempExe)
}

// Build a Quick program
func buildFile(filename string, outfile string) error {
	lexer := NewLexer("") // Placeholder
	parser := NewParser(lexer)
	compiler := NewCompiler(parser)
	defer compiler.Cleanup()
	
	return compiler.Compile(filename, outfile)
}

// Print tokens from a source file
func printTokens(filename string) {
	// Read the source file
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}
	
	// Create lexer
	lexer := NewLexer(string(source))
	
	// Print all tokens
	fmt.Println("Tokens in", filename)
	fmt.Println("--------------------")
	for {
		token := lexer.NextToken()
		fmt.Printf("%-15s %s (Line %d, Col %d)\n", 
			tokenTypeString(token.Type), 
			token.Value,
			token.Line,
			token.Column)
		
		if token.Type == TOKEN_EOF {
			break
		}
	}
}

// Get string representation of token type
func tokenTypeString(tt TokenType) string {
	switch tt {
	case TOKEN_IDENTIFIER:
		return "IDENTIFIER"
	case TOKEN_NUMBER:
		return "NUMBER"
	case TOKEN_STRING:
		return "STRING"
	case TOKEN_KEYWORD:
		return "KEYWORD"
	case TOKEN_OPERATOR:
		return "OPERATOR"
	case TOKEN_LPAREN:
		return "LPAREN"
	case TOKEN_RPAREN:
		return "RPAREN"
	case TOKEN_LBRACE:
		return "LBRACE"
	case TOKEN_RBRACE:
		return "RBRACE"
	case TOKEN_LBRACKET:
		return "LBRACKET"
	case TOKEN_RBRACKET:
		return "RBRACKET"
	case TOKEN_SEMICOLON:
		return "SEMICOLON"
	case TOKEN_COMMA:
		return "COMMA"
	case TOKEN_DOT:
		return "DOT"
	case TOKEN_COLON:
		return "COLON"
	case TOKEN_EOF:
		return "EOF"
	default:
		return "UNKNOWN"
	}
}

// Helper function to get runtime OS
func getRuntime() string {
	if os.PathSeparator == '\\' {
		return "windows"
	}
	return "unix"
}

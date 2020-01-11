BEGIN, END, DOT, ID, ASSIGN, SEMI, INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = (
'BEGIN', 'END', 'DOT', 'ID', 'ASSIGN', 'SEMI', 'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', 'LPAREN', 'RPAREN', 'EOF')

class Token(object):

	def __init__(self, type, value):

		self.type = type
		self.value = value

	def __str__(self):

		return f'Token({self.type}, {repr(self.value)})'

	def __repr__(self):

		return self.__str__()


RESERVED_KEYWORDS = {
	'BEGIN' : Token(BEGIN, 'BEGIN'),
	'END'	: Token(END, 'END'),
}

class Lexer(object):

	def __init__(self, text):

		self.text = text
		self.pos = 0
		self.currentChar = self.text[self.pos]

	def error(self):

		raise Exception('Invalid Character!')

	def advance(self):

		self.pos += 1
		if self.pos < len(self.text):
			self.currentChar = self.text[self.pos]
		else:
			self.currentChar = None

	def skipSpace(self):

		while self.currentChar is not None and self.currentChar.isspace():
			self.advance()

	def integer(self):

		number = ''
		while self.currentChar is not None and self.currentChar.isdigit():
			number += self.currentChar
			self.advance()
		return int(number)

	def peek(self):

		peekPos = self.pos + 1
		return self.text[peekPos] if peekPos < len(self.text) else None

	def _id(self):

		result = ''
		while self.currentChar is not None and self.currentChar.isalnum():
			result += self.currentChar
			self.advance()

		token = RESERVED_KEYWORDS.get(result, Token(ID, result))
		return token

	def getNextToken(self):

		while self.currentChar is not None:

			if self.currentChar.isspace():
				self.skipSpace()
				continue

			if self.currentChar.isdigit():
				return Token(INTEGER, self.integer())

			if self.currentChar.isalpha():
				return self._id()

			if self.currentChar == ':' and self.peek() == '=':
				self.advance()
				self.advance()
				return Token(ASSIGN, ':=')

			if self.currentChar == ';':
				self.advance()
				return Token(SEMI, ';')

			if self.currentChar == '.':
				self.advance()
				return Token(DOT, '.')

			if self.currentChar == '+':
				self.advance()
				return Token(PLUS, '+')

			if self.currentChar == '-':
				self.advance()
				return Token(MINUS, '-')

			if self.currentChar == '*':
				self.advance()
				return Token(MUL, '*')

			if self.currentChar == '/':
				self.advance()
				return Token(DIV, '/')

			if self.currentChar == '(':
				self.advance()
				return Token(LPAREN, '(')

			if self.currentChar == ')':
				self.advance()
				return Token(RPAREN, ')')

			self.error()

		return Token(EOF, None)

class AST(object):

	pass

class Compound(AST):

	def __init__(self):

		self.children = []

class Assign(AST):

	def __init__(self, left, op, right):

		self.left = left
		self.token = self.op = op
		self.right = right

class Var(AST):

	def __init__(self, token):

		self.token = token
		self.value = self.token.value

class Num(AST):

	def __init__(self, token):

		self.token = token
		self.value = self.token.value

class NoOp(AST):

	pass

class UnaryOp(AST):

	def __init__(self, op, expr):

		self.op = op
		self.expr = expr

class BinOp(AST):

	def __init__(self, left, op, right):

		self.left = left
		self.token = self.op = op
		self.right = right

class Parser(object):

	def __init__(self, lexer):

		self.lexer = lexer
		self.currentToken = self.lexer.getNextToken()

	def error(self):

		raise Exception('Invalid Syntax')

	def eatToken(self, tokenType):

		if self.currentToken.type == tokenType:
			self.currentToken = self.lexer.getNextToken()
		else:
			self.error()

	def program(self):

		node = self.compoundStatement()
		self.eatToken(DOT)
		return node

	def compoundStatement(self):

		self.eatToken(BEGIN)
		nodes = self.statementList()
		self.eatToken(END)

		root = Compound()

		for node in nodes:
			root.children.append(node)

		return root

	def statementList(self):

		node = self.statement()
		result = [node]

		while self.currentToken.type == SEMI:

			self.eatToken(SEMI)
			result.append(self.statement())

		if self.currentToken.type == ID:
			self.error()

		return result

	def statement(self):

		if self.currentToken.type == BEGIN:
			node = self.compoundStatement()

		elif self.currentToken.type == ID:
			node = self.assignmentStatement()

		else:
			node = self.empty()

		return node

	def assignmentStatement(self):

		left = self.variable()
		token = self.currentToken
		self.eatToken(ASSIGN)
		right = self.expr()
		node = Assign(left, token, right)
		return node

	def variable(self):

		node = Var(self.currentToken)
		self.eatToken(ID)
		return node

	def empty(self):

		return NoOp()

	def factor(self):

		token = self.currentToken

		if token.type == PLUS:
			self.eatToken(PLUS)
			return UnaryOp(op=token, expr=self.factor())

		elif token.type == MINUS:
			self.eatToken(MINUS)
			return UnaryOp(op=token, expr=self.factor())

		elif token.type == INTEGER:
			self.eatToken(INTEGER)
			return Num(token)

		elif token.type == LPAREN:
			self.eatToken(LPAREN)
			node = self.expr()
			self.eatToken(RPAREN)
			return node

		else:
			node = self.variable()
			return node

	def term(self):

		node = self.factor()

		while self.currentToken.type in (MUL, DIV):

			token = self.currentToken

			if token.type == MUL:
				self.eatToken(MUL)

			elif token.type == DIV:
				self.eatToken(DIV)

			node = BinOp(left=node, op=token, right=self.factor())

		return node

	def expr(self):

		node = self.term()

		while self.currentToken.type in (PLUS, MINUS):

			token = self.currentToken

			if token.type == PLUS:
				self.eatToken(PLUS)

			elif token.type == MINUS:
				self.eatToken(MINUS)

			node = BinOp(left=node, op=token, right=self.term())

		return node

	def parse(self):

		node = self.program()
		if self.currentToken.type != EOF:
			self.error()
		return node

class NodeVisitor(object):

	def visit(self, node):

		methodName = 'visit_' + type(node).__name__
		visitor = getattr(self, methodName, self.genericVisit)
		return visitor(node)

	def genericVisit(self, node):

		raise Exception(f'No visit_{type(node).__name__} found!')

class Interpreter(NodeVisitor):

	def __init__(self, parser):

		self.parser = parser
		self.GLOBAL_SCOPE = {}

	def visit_Compound(self, node):

		for child in node.children:
			self.visit(child)

	def visit_Assign(self, node):

		varName = node.left.value
		self.GLOBAL_SCOPE[varName] = self.visit(node.right)

	def visit_Var(self, node):

		varName = node.value
		varValue = self.GLOBAL_SCOPE.get(varName)

		if varValue is None:
			raise NameError(repr(varName))
		else:
			return varValue

	def visit_NoOp(self, node):

		pass

	def visit_Num(self, node):

		return node.value

	def visit_UnaryOp(self, node):

		if node.op.type == PLUS:
			return self.visit(node.expr)

		elif node.op.type == MINUS:
			return -self.visit(node.expr)

	def visit_BinOp(self, node):

		if node.op.type == PLUS:
			return self.visit(node.left) + self.visit(node.right)

		elif node.op.type == MINUS:
			return self.visit(node.left) - self.visit(node.right)

		elif node.op.type == MUL:
			return self.visit(node.left) * self.visit(node.right)

		elif node.op.type == DIV:
			return self.visit(node.left) / self.visit(node.right)

	def interpret(self):

		tree = self.parser.parse()
		return self.visit(tree)

def main():

	print('\n## Simple Arithmetic Interpreter v1 ##\n')

	while True:

		try:
			text = input('\nin>> ')
		except EOFError:
			break
		if not text:
			continue

		lexer = Lexer(text)
		parser = Parser(lexer)
		interpreter = Interpreter(parser)

		print('out>', interpreter.interpret())

if __name__ == '__main__':
	main()

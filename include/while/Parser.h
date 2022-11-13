//modified to own while language


//===- Parser.h - Toy Language Parser -------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the parser for the Toy language. It processes the Token
// provided by the Lexer and returns an AST.
//
//===----------------------------------------------------------------------===//

#ifndef TOY_PARSER_H
#define TOY_PARSER_H

#include "while/AST.h"
#include "while/Lexer.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <utility>
#include <vector>
#include <set>
#include <optional>


namespace while_lang {


	std::vector<std::pair<Token, int>> precedence_map{
		{Token::tok_plus, 20},
		{Token::tok_minus, 20},
		{Token::tok_multiply, 40},
		{Token::tok_and, 40},
	};

	std::set<Token> binary_op_set{
		Token::tok_plus,Token::tok_minus, Token::tok_multiply, Token::tok_and
	};
	/// This is a simple recursive parser for the Toy language. It produces a well
	/// formed AST from a stream of Token supplied by the Lexer. No semantic checks
	/// or symbol resolution is performed. For example, variables are referenced by
	/// string and the code could reference an undeclared variable and the parsing
	/// succeeds.
	class Parser {
	public:
		/// Create a Parser for the supplied lexer.
		Parser(Lexer& lexer) : lexer(lexer) {}

		/// Parse a full Module. A module is a list of function definitions.
		ParsedValue<ModuleAST> parseModule() {
			lexer.getNextToken(); // prime the lexer

			// Parse functions one at a time and accumulate in this vector.
			std::vector<ParsedValue<StatementAST>> functions;
			while (auto f = parseSatement()) {
				functions.push_back(std::move(f));
				if (lexer.getCurToken() == tok_eof)
					break;
			}
			// If we didn't reach EOF, there was an error during parsing
			if (lexer.getCurToken() != tok_eof)
				return parseError<ModuleAST>("nothing", "at end of module");

			ModuleAST tmp(std::move(functions));
			return ParsedValue<ModuleAST>(ModuleAST(std::move(tmp)));
		}

	private:
		Lexer& lexer;

		/// Parse a return statement.
		/// return :== return ; | return expr ;
		ParsedValue<ReturnExprAST> parseReturn() {
			auto loc = lexer.getLastLocation();
			lexer.consume(tok_return);

			// return takes an optional argument
			if (lexer.getCurToken() != ';') {
				ParsedValue<ExprAST> expr = parseExpression();
				if (!expr)
					return std::move(expr);
				return ParsedValue<ReturnExprAST>(new ReturnExprAST(std::move(loc), std::move(expr)));
			}
			return ParsedValue<ReturnExprAST>(new ReturnExprAST(std::move(loc), ParsedValue<ExprAST>::error())); //XX JS make it void type
		}

		/// Parse a literal number.
		/// numberexpr ::= number
		ParsedValue<ExprAST> parseNumberExpr() {
			auto loc = lexer.getLastLocation();
			auto result =
				ParsedValue<ExprAST> ( new NumberExprAST(std::move(loc), lexer.getValue()));
			lexer.consume(tok_number);
			return std::move(result);
		}

		

		/// parenexpr ::= '(' expression ')'
		ParsedValue<ExprAST> parseParenExpr() {
			lexer.getNextToken(); // eat (.
			auto v = parseExpression();
			if (!v)
				return ParsedValue<ExprAST>::error();

			if (lexer.getCurToken() != ')')
				return parseError<ExprAST>(")", "to close expression with parentheses");
			lexer.consume(Token(')'));
			return v;
		}

		/// identifierexpr
		///   ::= identifier
		///   ::= identifier '(' expression ')'
		ParsedValue<ExprAST> parseIdentifierExpr() {
			std::string name(lexer.getId());

			auto loc = lexer.getLastLocation();
			lexer.getNextToken(); // eat identifier.

			if (lexer.getCurToken() != '(') // Simple variable ref.
				return ParsedValue<VariableExprAST>(new VariableExprAST(std::move(loc), name));

			// This is a function call.
			lexer.consume(Token('('));
			std::vector<ParsedValue<ExprAST>> args;
			if (lexer.getCurToken() != ')') {
				while (true) {
					if (auto arg = parseExpression())
						args.push_back(std::move(arg));
					else
						return ParsedValue<ExprAST>::error();

					if (lexer.getCurToken() == ')')
						break;

					if (lexer.getCurToken() != ',')
						return parseError<ExprAST>(", or )", "in argument list");
					lexer.getNextToken();
				}
			}
			lexer.consume(Token(')'));

			// It can be a builtin call to print
			if (name == "print") {
				if (args.size() != 1)
					return parseError<ExprAST>("<single arg>", "as argument to print()");

				return ParsedValue<PrintExprAST>(new PrintExprAST(std::move(loc), std::move(args[0])));
			}

			// Call to a user-defined function
			return ParsedValue<CallExprAST>(new CallExprAST(std::move(loc), name, std::move(args)));
		}

		/// primary
		///   ::= identifierexpr
		///   ::= numberexpr
		///   ::= parenexpr
		///   ::= tensorliteral
		ParsedValue<ExprAST> parsePrimary() {
			switch (lexer.getCurToken()) {
			default:
				llvm::errs() << "unknown token '" << lexer.getCurToken()
					<< "' when expecting an expression\n";
				return ParsedValue<ExprAST>::error();
			case tok_identifier:
				return parseIdentifierExpr();
			case tok_number:
				return parseNumberExpr();
			case '(':
				return parseParenExpr();	
			case Token::tok_not:
				lexer.consume(tok_not);
				return ExprAST::create_not(parseExpression());
			case Token::tok_true:
				lexer.consume(tok_true);
				return ExprAST::create_true();
			case Token::tok_false:
				lexer.consume(tok_false);
				return ExprAST::create_false();
			}
		}


		bool currentTokensBinaryOp() {
			return binary_op_set.count(lexer.getCurToken()) > 0;
		}
		/// Recursively parse the right hand side of a binary expression, the ExprPrec
		/// argument indicates the precedence of the current binary operator.
		///
		/// binoprhs ::= ('binop'? primary)*
		ParsedValue<ExprAST> parseBinOpRHS(int exprPrec, ParsedValue<ExprAST> lhs) {
			// If this is a binop, find its precedence.
			while (currentTokensBinaryOp()) {
				int tokPrec = getTokPrecedence();

				// If this is a binop that binds at least as tightly as the current binop,
				// consume it, otherwise we are done.
				if (tokPrec < exprPrec)
					return lhs;

				// Okay, we know this is a binop.
				int binOp = lexer.getCurToken();
				lexer.consume(Token(binOp));
				auto loc = lexer.getLastLocation();

				// Parse the primary expression after the binary operator.
				auto rhs = parsePrimary();
				if (!rhs)
					return parseError<ExprAST>("expression", "to complete binary operator");

				// If BinOp binds less tightly with rhs than the operator after rhs, let
				// the pending operator take rhs as its lhs.
				int nextPrec = getTokPrecedence();
				if (tokPrec < nextPrec) {
					rhs = parseBinOpRHS(nextPrec, std::move(rhs));
					if (!rhs)
						return parseError<ExprAST>("expression", "to complete binary operator");
				}

				// Merge lhs/RHS.
				lhs = ParsedValue<BinaryExprAST>( new BinaryExprAST(
					std::move(loc), getAstOp(binOp), std::move(lhs), std::move(rhs)
				));
			}
			return lhs;
		}

		/// expression::= primary binop rhs
		ParsedValue<ExprAST> parseExpression() {
			auto lhs = parsePrimary();
			if (!lhs)
				return parseError<ExprAST>("expression", "parsing an expression");;

			return parseBinOpRHS(0, std::move(lhs));
		}

		/// type ::= < shape_list >
		/// shape_list ::= num | num , shape_list
		ParsedValue<VarType> parseType() {
			if (lexer.getCurToken() != '<')
				return parseError<VarType>("<", "to begin type");
			lexer.getNextToken(); // eat <

			auto type = ParsedValue<VarType>::error();

			while (lexer.getCurToken() == tok_number) {
				type->shape.push_back(lexer.getValue());
				lexer.getNextToken();
				if (lexer.getCurToken() == ',')
					lexer.getNextToken();
			}

			if (lexer.getCurToken() != '>')
				return parseError<VarType>(">", "to end type");
			lexer.getNextToken(); // eat >
			return type;
		}

		/// Parse a variable declaration, it starts with a `var` keyword followed by
		/// and identifier and an optional type (shape specification) before the
		/// initializer.
		/// decl ::= var identifier [ type ] = expr
		ParsedValue<VarDeclExprAST> parseDeclaration() {
			if (lexer.getCurToken() != tok_var)
				return parseError<VarDeclExprAST>("var", "to begin declaration");
			auto loc = lexer.getLastLocation();
			lexer.getNextToken(); // eat var

			if (lexer.getCurToken() != tok_identifier)
				return parseError<VarDeclExprAST>("identified",
					"after 'var' declaration");
			std::string id(lexer.getId());
			lexer.getNextToken(); // eat id

			ParsedValue<VarType> type = ParsedValue<VarType>::error(); // Type is optional, it can be inferred
			if (lexer.getCurToken() == '<') {
				type = parseType();
				if (!type)
					return nullptr;
			}

			if (!type)
				type = ParsedValue<VarType>::error();
			lexer.consume(Token('='));
			auto expr = parseExpression();
			return ParsedValue<VarDeclExprAST>(new VarDeclExprAST(
				std::move(loc), std::move(id), std::move(*type), std::move(expr)
			));
		}

		/// Parse a block: a list of expression separated by semicolons and wrapped in
		/// curly braces.
		///
		/// block ::= { expression_list }
		/// expression_list ::= block_expr ; expression_list
		/// block_expr ::= decl | "return" | expr
		ParsedValue<ExprASTList> parseBlock() {
			if (lexer.getCurToken() != '{')
				return parseError<ExprASTList>("{", "to begin block");
			lexer.consume(Token('{'));

			ExprASTList exprList;

			// Ignore empty expressions: swallow sequences of semicolons.
			while (lexer.getCurToken() == ';')
				lexer.consume(Token(';'));

			while (lexer.getCurToken() != '}' && lexer.getCurToken() != tok_eof) {
				if (lexer.getCurToken() == tok_var) {
					// Variable declaration
					auto varDecl = parseDeclaration();
					if (!varDecl)
						return ParsedValue<ExprASTList>::error();
					exprList.push_back(std::move(varDecl));
				}
				else if (lexer.getCurToken() == tok_return) {
					// Return statement
					auto ret = parseReturn();
					if (!ret)
						return ParsedValue<ExprASTList>::error();
					exprList.push_back(std::move(ret));
				}
				else {
					// General expression
					auto expr = parseExpression();
					if (!expr)
						return ParsedValue<ExprASTList>::error();
					exprList.push_back(std::move(expr));
				}
				// Ensure that elements are separated by a semicolon.
				if (lexer.getCurToken() != ';')
					return parseError<ExprASTList>(";", "after expression");

				// Ignore empty expressions: swallow sequences of semicolons.
				while (lexer.getCurToken() == ';')
					lexer.consume(Token(';'));
			}

			if (lexer.getCurToken() != '}')
				return parseError<ExprASTList>("}", "to close block");

			lexer.consume(Token('}'));
			return std::move(ParsedValue<ExprASTList>(std::move(exprList)));
		}

		/// prototype ::= def id '(' decl_list ')'
		/// decl_list ::= identifier | identifier, decl_list
		ParsedValue<PrototypeAST> parsePrototype() {
			auto loc = lexer.getLastLocation();

			if (lexer.getCurToken() != tok_def)
				return parseError<PrototypeAST>("def", "in prototype");
			lexer.consume(tok_def);

			if (lexer.getCurToken() != tok_identifier)
				return parseError<PrototypeAST>("function name", "in prototype");

			std::string fnName(lexer.getId());
			lexer.consume(tok_identifier);

			if (lexer.getCurToken() != '(')
				return parseError<PrototypeAST>("(", "in prototype");
			lexer.consume(Token('('));

			std::vector<ParsedValue<VariableExprAST>> args;
			if (lexer.getCurToken() != ')') {
				do {
					std::string name(lexer.getId());
					auto loc = lexer.getLastLocation();
					lexer.consume(tok_identifier);
					auto decl = ParsedValue<VariableExprAST>(new VariableExprAST(
						std::move(loc), name)	);
					args.push_back(std::move(decl));
					if (lexer.getCurToken() != ',')
						break;
					lexer.consume(Token(','));
					if (lexer.getCurToken() != tok_identifier)
						return parseError<PrototypeAST>(
							"identifier", "after ',' in function parameter list");
				} while (true);
			}
			if (lexer.getCurToken() != ')')
				return parseError<PrototypeAST>(")", "to end function prototype");

			// success.
			lexer.consume(Token(')'));
			return ParsedValue<PrototypeAST>(new PrototypeAST(
				std::move(loc), fnName, std::move(args)
			));
		}

		ParsedValue<StatementAST> parseIfSatement() {
			auto loc = lexer.getLastLocation();

			ParsedValue<ExprAST> condition = parseExpression();

			switch (lexer.getCurToken()) {
			case Token::tok_then:
				lexer.consume(tok_then);
				break;
			default:
				return parseError<StatementAST>("then", "parsing a if statement");
			}

			ParsedValue<StatementAST> true_branch = parseSatement();

			switch (lexer.getCurToken()) {
			case Token::tok_else:
				lexer.consume(tok_else);
				break;
			default:
				return parseError<StatementAST>("else", "parsing a if statement");
			}
			ParsedValue<StatementAST> false_branch = parseSatement();

			return StatementAST::create_if(loc, std::move(*condition), std::move(*true_branch), std::move(*false_branch));
		}
		ParsedValue<StatementAST> parseWhileSatement() {
			auto loc = lexer.getLastLocation();

			ParsedValue<ExprAST> condition = parseExpression();

			switch (lexer.getCurToken()) {
			case Token::tok_do:
				lexer.consume(tok_do);
				break;
			default:
				return parseError<StatementAST>("do", "parsing a while statement");
			}

			ParsedValue<StatementAST> true_branch = parseSatement();

			return StatementAST::create_while(loc, std::move(*condition), std::move(*true_branch));
		}

		ParsedValue<StatementAST> parseVarDeclSatement() {
			auto loc = lexer.getLastLocation();

			std::string var_name(lexer.getId());
			lexer.consume(tok_identifier);
		
			switch (lexer.getCurToken()) {
			case Token::tok_asign:
				lexer.consume(tok_asign);
				break;
			default:
				return parseError<StatementAST>(":=", "parsing a variable declare statement");
			}

			ParsedValue<ExprAST> value = parseExpression();

			return StatementAST::create_var_decl(loc, std::move(var_name), std::move(*value));
		}


		ParsedValue<StatementAST> parseAssigmentSatement() {
			auto loc = lexer.getLastLocation();

			std::string var_name(lexer.getId());
			lexer.consume(tok_identifier);

			switch (lexer.getCurToken()) {
			case Token::tok_asign:
				lexer.consume(tok_asign);
				break;
			default:
				return parseError<StatementAST>(":=", "parsing an assigment statement");
			}

			ParsedValue<ExprAST> value = parseExpression();

			return StatementAST::create_assigment(loc, std::move(var_name), std::move(*value));
		}

		//parse a while statement
		ParsedValue<StatementAST> parseSatement() {

			
			auto loc = lexer.getLastLocation();

			switch (lexer.getCurToken()) {
			case Token::tok_if:
				lexer.consume(tok_if);
				return parseIfSatement();
				break;
			case Token::tok_while:
				lexer.consume(tok_while);
				return parseWhileSatement();
				break;
			case Token::tok_var:
				lexer.consume(tok_var);
				return parseVarDeclSatement();
				break;
			case Token::tok_identifier:
				// consumed in the parse method -- lexer.consume(tok_identifier);
				return parseAssigmentSatement();
				break;
			default:
				return parseError<StatementAST>("A Statement", "parsing a statement");
			}
		}
		/// Parse a function definition, we expect a prototype initiated with the
		/// `def` keyword, followed by a block containing a list of expressions.
		///
		/// definition ::= prototype block
		ParsedValue<FunctionAST> parseDefinition() {
			auto proto = parsePrototype();
			if (!proto)
				return nullptr;

			if (auto block = parseBlock())
				return ParsedValue<FunctionAST>(new FunctionAST(
					std::move(proto), std::move(block)
				));
			return nullptr;
		}

		/// Get the precedence of the pending binary operator token.
		int getTokPrecedence() {
			if (!isascii(lexer.getCurToken()))
				return -1;
			for (const auto& el : precedence_map) {
				if (lexer.getCurToken() == el.first) {
					return el.second;
				}
			}
			// 1 is lowest precedence.
			switch (static_cast<char>(lexer.getCurToken())) {
			case '-':
				return 20;
			case '+':
				return 20;
			case '*':
				return 40;
			default:
				return -1;
			}
		}

		/// Helper function to signal errors while parsing, it takes an argument
		/// indicating the expected token and another argument giving more context.
		/// Location is retrieved from the lexer to enrich the error message.
		template <typename R, typename T, typename U = const char*>
		ParsedValue<R> parseError(T&& expected, U&& context = "") {
			auto curToken = lexer.getCurToken();
			llvm::errs() << "Parse error (" << lexer.getLastLocation().line << ", "
				<< lexer.getLastLocation().col << "): expected '" << expected
				<< "' " << context << " but has Token " << curToken;
			if (isprint(curToken))
				llvm::errs() << " '" << (char)curToken << "'";
			llvm::errs() << "\n";
			return ParsedValue<R>::error();
		}
	};

} // namespace toy

#endif // TOY_PARSER_H

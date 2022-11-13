//modified to own while language


//===- AST.h - Node definition for the Toy AST ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the AST for the Toy language. It is optimized for
// simplicity, not efficiency. The AST forms a tree structure where each node
// references its children using std::unique_ptr<>.
//
//===----------------------------------------------------------------------===//

#ifndef TOY_AST_H
#define TOY_AST_H

#include "while/Lexer.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include <utility>
#include <vector>
#include <ostream>
#include <while/ast_statement.h>

namespace while_lang {

	/// A variable type with shape information.
	struct VarType {
		std::vector<int64_t> shape;
	};

	/// Expression class for numeric literals like "1.0".
	class NumberExprAST : public ExprAST {
		double val;

	public:
		NumberExprAST(Location loc, double val)
			: ExprAST(Expr_Num, std::move(loc)), val(val) {}

		double getValue() { return val; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Num; }
	};

	/// Expression class for a literal value.
	class LiteralExprAST : public ExprAST {
		std::vector<std::unique_ptr<ExprAST>> values;
		std::vector<int64_t> dims;

	public:
		LiteralExprAST(Location loc, std::vector<std::unique_ptr<ExprAST>> values,
			std::vector<int64_t> dims)
			: ExprAST(Expr_Literal, std::move(loc)), values(std::move(values)),
			dims(std::move(dims)) {}

		llvm::ArrayRef<std::unique_ptr<ExprAST>> getValues() { return values; }
		llvm::ArrayRef<int64_t> getDims() { return dims; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Literal; }
	};

	/// Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string name;

	public:
		VariableExprAST(Location loc, llvm::StringRef name)
			: ExprAST(Expr_Var, std::move(loc)), name(name) {}

		llvm::StringRef getName() { return name; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Var; }
	};

	/// Expression class for defining a variable.
	class VarDeclExprAST : public ExprAST {
		std::string name;
		VarType type;
		ParsedValue<ExprAST> initVal;

	public:
		VarDeclExprAST(Location loc, llvm::StringRef name, VarType type, ParsedValue<ExprAST> initVal)
			: ExprAST(Expr_VarDecl, std::move(loc)), name(name),
			type(std::move(type)), initVal(std::move(initVal)) {}

		llvm::StringRef getName() { return name; }
		const ParsedValue<ExprAST>& getInitVal() { return initVal; }
		const VarType& getType() { return type; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_VarDecl; }
	};

	/// Expression class for a return operator.
	class ReturnExprAST : public ExprAST {
		ParsedValue<ExprAST> expr;

	public:
		ReturnExprAST(Location&& loc, ParsedValue<ExprAST>&& expr)
			: ExprAST(Expr_Return, std::move(loc)), expr(std::move(expr)) {}

		const ParsedValue<ExprAST>& getExpr() {
			return expr;
		}

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Return; }
	};

	/// Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		ExprASTOp op;
		ParsedValue<ExprAST> lhs, rhs;

	public:
		ExprASTOp getOp() { return op; }
		const ParsedValue<ExprAST>& getLHS() { return lhs; }
		const ParsedValue<ExprAST>& getRHS() { return rhs; }

		BinaryExprAST(Location loc, ExprASTOp op_, ParsedValue<ExprAST>&& lhs_, ParsedValue<ExprAST>&& rhs_)
			: ExprAST(Expr_BinOp, std::move(loc)), op(op_), lhs(std::move(lhs_)),
			rhs(std::move(rhs_)) {}

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_BinOp; }
	};

	/// Expression class for function calls.
	class CallExprAST : public ExprAST {
		std::string callee;
		std::vector<ParsedValue<ExprAST>> args;

	public:
		CallExprAST(Location loc, const std::string& callee,
			std::vector<ParsedValue<ExprAST>>&& args)
			: ExprAST(Expr_Call, std::move(loc)), callee(callee),
			args(std::move(args)) {}

		llvm::StringRef getCallee() { return callee; }
		llvm::ArrayRef<ParsedValue<ExprAST>> getArgs() { return args; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Call; }
	};

	/// Expression class for builtin print calls.
	class PrintExprAST : public ExprAST {
		ParsedValue<ExprAST> arg;

	public:
		PrintExprAST(Location loc, ParsedValue<ExprAST> arg)
			: ExprAST(Expr_Print, std::move(loc)), arg(std::move(arg)) {}

		const ParsedValue<ExprAST>& getArg() { return arg; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Print; }
	};

	/// This class represents the "prototype" for a function, which captures its
	/// name, and its argument names (thus implicitly the number of arguments the
	/// function takes).
	class PrototypeAST {
		Location location;
		std::string name;
		std::vector<ParsedValue<VariableExprAST>> args;

	public:
		PrototypeAST(Location location, const std::string& name,
			std::vector<ParsedValue<VariableExprAST>> args)
			: location(std::move(location)), name(name), args(std::move(args)) {}

		const Location& loc() { return location; }
		llvm::StringRef getName() const { return name; }
		llvm::ArrayRef<ParsedValue<VariableExprAST>> getArgs() { return args; }
	};

	/// This class represents a function definition itself.
	class FunctionAST {
		ParsedValue<PrototypeAST> proto;
		ParsedValue<ExprASTList> body;

	public:
		FunctionAST(ParsedValue<PrototypeAST> proto,
			ParsedValue<ExprASTList> body)
			: proto(std::move(proto)), body(std::move(body)) {}
		const ParsedValue<PrototypeAST>& getProto() { return proto; }
		const ParsedValue<ExprASTList>& getBody() { return body; }
	};

	
	

	

	/// This class represents a list of functions to be processed together
	class ModuleAST {
		std::vector<ParsedValue<StatementAST>> statements;

	public:
		ModuleAST(std::vector<ParsedValue<StatementAST>>&& functions)
			: statements(std::move(functions)) {}

		auto begin() { return statements.begin(); }
		auto end() { return statements.end(); }
	};

	void dump(ModuleAST&);


	
} // namespace toy
llvm::raw_ostream& operator << (llvm::raw_ostream& out, while_lang::ExprASTOp d);

#endif // TOY_AST_H

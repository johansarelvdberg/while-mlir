#pragma once

#include <while/Lexer.h>
#include <optional>
#include <while/parsed_value.h>
#include <while/Lexer.h>
#include <optional>
#include <while/parsed_value.h>

namespace while_lang {
	enum class ExprASTOp {
		undefined_,
		plus_,
		minus_,
		not_,
		and_,
		or_,
		imply_,
		equal_
	};

	ExprASTOp getAstOp(char c);

	/// Base class for all expression nodes.
	class ExprAST {
	public:
		enum ExprASTKind {
			Expr_VarDecl,
			Expr_Return,
			Expr_Num,
			Expr_Literal,
			Expr_Var,
			Expr_BinOp,
			Expr_UniOp,
			Expr_Call,
			Expr_Print,
			Expr_Bool
		};



		ExprAST(ExprASTKind kind, Location location)
			: kind(kind), location(std::move(location)) {}
		virtual ~ExprAST() = default;

		ExprASTKind getKind() const { return kind; }

		const Location& loc() { return location; }

		static ParsedValue<ExprAST> create_true();
		static ParsedValue<ExprAST> create_false();
		static ParsedValue<ExprAST> create_not(ParsedValue<ExprAST> exp);
	private:
		const ExprASTKind kind;
		Location location;
	};

	/// A block-list of expressions.
	using ExprASTList = std::vector<ParsedValue<ExprAST>>;

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

	//bool value
	class BoolExprAST : public ExprAST {
		bool val;

	public:
		BoolExprAST(Location loc, bool val_)
			: ExprAST(Expr_Bool, loc), val(val_) {}

		bool getValue() { return val; }

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_Bool; }
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

	/// Expression class for a binary operator. 
	class UniarityExprAST : public ExprAST {
		ExprASTOp op;
		ParsedValue<ExprAST> parameter;

	public:
		ExprASTOp getOp() { return op; }
		const ParsedValue<ExprAST>& getParameter() { return parameter; }

		UniarityExprAST(Location loc, ExprASTOp op_, ParsedValue<ExprAST>&& parameter_)
			: ExprAST(Expr_UniOp, std::move(loc)), op(op_), parameter(std::move(parameter_)) {}

		/// LLVM style RTTI
		static bool classof(const ExprAST* c) { return c->getKind() == Expr_UniOp; }
	};


}

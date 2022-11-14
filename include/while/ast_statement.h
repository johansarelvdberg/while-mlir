#pragma once

#include<memory>
#include<while/ast_expression.h>
#include <string>
#include <optional>
#include <while/parsed_value.h>

namespace while_lang {

	enum class StatementType {
		ast_if,
		ast_while,
		ast_assignment,
		ast_decl
	};


	class StatementAST_Impl {
	public:
		StatementAST_Impl() = default;
	};

	

	class StatementAST {
		ParsedValue<StatementAST_Impl> current;
	public:
		const StatementType type;
		const Location location;

		StatementAST() = delete;


		template<typename T>
		T& get_current() { return *current; }

		static ParsedValue<StatementAST> create_if(Location location_, ParsedValue<ExprAST> conditions, ParsedValue<StatementAST> true_block, ParsedValue<StatementAST> false_block);
		static ParsedValue<StatementAST> create_while(Location location_, ParsedValue<ExprAST> conditions, ParsedValue<StatementAST> true_block);
		static ParsedValue<StatementAST> create_var_decl(Location location_, std::string identifier, ParsedValue<ExprAST> value);
		static ParsedValue<StatementAST> create_assigment(Location location_, std::string identifier, ParsedValue<ExprAST> value);
	protected:
		StatementAST(Location location_, StatementType type_, ParsedValue<StatementAST_Impl> current_)
			: location(std::move(location_)), type(type_), current(std::move(current_)) {}
	};

	class StatementASTIf : public StatementAST_Impl {
	public:
		const ParsedValue<ExprAST> condition;
		const ParsedValue<StatementAST> true_block;
		const ParsedValue<StatementAST> false_block;

		StatementASTIf(ParsedValue<ExprAST> condition_, ParsedValue<StatementAST> true_block_, ParsedValue<StatementAST> false_block_) :
			condition(std::move(condition_)), true_block(std::move(true_block_)), false_block(std::move(false_block_)) {};
	};

	class StatementASTWhile : public StatementAST_Impl {
	public:
		const ParsedValue<ExprAST> condition;
		const ParsedValue<StatementAST> true_block;

		StatementASTWhile(ParsedValue<ExprAST> condition_, ParsedValue<StatementAST> true_block_) :
			condition(std::move(condition_)), true_block(std::move(true_block_)) {};
	};

	class StatementASTDecl : public StatementAST_Impl {
	public:
		const std::string variable;
		const ParsedValue<ExprAST> expression;
	
		StatementASTDecl(std::string variable_, ParsedValue<ExprAST> expression_) :
			variable(std::move(variable_)), expression(std::move(expression_)) {};
	};
}
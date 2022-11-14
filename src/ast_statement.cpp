#include <while/ast_statement.h>

using namespace while_lang;

ParsedValue<StatementAST> StatementAST::create_if(Location location_, ParsedValue<ExprAST> conditions, ParsedValue<StatementAST> true_block, ParsedValue<StatementAST> false_block) {
	StatementASTIf* state_if = new StatementASTIf(std::move(conditions), std::move(true_block), std::move(false_block));
	return ParsedValue<StatementAST>(new StatementAST(location_, StatementType::ast_if, state_if));
}

ParsedValue<StatementAST> StatementAST::create_while(Location location_, ParsedValue<ExprAST> conditions, ParsedValue<StatementAST> true_block) {
	StatementASTWhile* state_while = new StatementASTWhile(std::move(conditions), std::move(true_block));
	return std::move(ParsedValue<StatementAST>(new StatementAST(location_, StatementType::ast_while, state_while)));
}
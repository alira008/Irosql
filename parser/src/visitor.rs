use lexer::Span;

use crate::ast::{
    ArithmeticOperator, ArithmeticOperatorKind, CaseCondition, CommonTableExpression,
    CommonTableExpressionStatement, ComparisonOperator, ComparisonOperatorKind, DataType,
    DataTypeSize, Expression, ExpressionList, FetchArg, FunctionName, GroupByClause, HavingClause,
    InsertStatement, Join, JoinCondition, JoinType, Keyword, KeywordKind, Literal, LocalVariable,
    NextOrFirst, NumericSize, OffsetArg, OffsetFetchClause, OrderByArg, OrderByClause, OverClause,
    ProcedureParameter, ProcedureParameterName, Query, RowOrRows, RowsOrRange, SelectItem,
    SelectStatement, Statement, Symbol, SymbolKind, TableArg, TableSource, Top, UnaryOperator,
    UnaryOperatorKind, Union, WhereClause, WindowFrame, WindowFrameBound,
};

pub trait Visitor: Sized {
    type Result: VisitorResult;

    fn visit_query(&mut self, query: &Query) -> Self::Result {
        walk_query(self, query)
    }
    fn visit_expression(&mut self, expr: &Expression) -> Self::Result {
        walk_expression(self, expr)
    }
    fn visit_statement(&mut self, stmt: &Statement) -> Self::Result {
        walk_statement(self, stmt)
    }
    fn visit_insert_statement(&mut self, stmt: &InsertStatement) -> Self::Result {
        walk_insert_statement(self, stmt)
    }
    fn visit_union(&mut self, union: &Union) -> Self::Result {
        walk_union(self, union)
    }
    fn visit_select_statement(&mut self, stmt: &SelectStatement) -> Self::Result {
        walk_select_statement(self, stmt)
    }
    fn visit_common_table_expression_statement(
        &mut self,
        stmt: &CommonTableExpressionStatement,
    ) -> Self::Result {
        walk_common_table_expression_statement(self, stmt)
    }

    fn visit_symbol(&mut self, symbol: &Symbol) -> Self::Result {
        walk_symbol(self, symbol)
    }
    fn visit_symbol_kind(&mut self, _: SymbolKind) -> Self::Result {
        Self::Result::output()
    }
    fn visit_select_item_wild_card(&mut self) -> Self::Result {
        Self::Result::output()
    }
    fn visit_asterisk(&mut self) -> Self::Result {
        Self::Result::output()
    }
    fn visit_span(&mut self, _: &Span) -> Self::Result {
        Self::Result::output()
    }
    fn visit_literal(&mut self, literal: &Literal) -> Self::Result {
        walk_literal(self, literal)
    }
    fn visit_comparison_operator(&mut self, op: &ComparisonOperator) -> Self::Result {
        walk_comparison_operator(self, op)
    }
    fn visit_comparison_operator_kind(&mut self, _: ComparisonOperatorKind) -> Self::Result {
        Self::Result::output()
    }
    fn visit_arithmetic_operator(&mut self, op: &ArithmeticOperator) -> Self::Result {
        walk_arithmetic_operator(self, op)
    }
    fn visit_arithmetic_operator_kind(&mut self, _: ArithmeticOperatorKind) -> Self::Result {
        Self::Result::output()
    }
    fn visit_unary_operator(&mut self, op: &UnaryOperator) -> Self::Result {
        walk_unary_operator(self, op)
    }
    fn visit_unary_operator_kind(&mut self, _: UnaryOperatorKind) -> Self::Result {
        Self::Result::output()
    }
    fn visit_keyword(&mut self, keyword: &Keyword) -> Self::Result {
        walk_keyword(self, keyword)
    }
    fn visit_keyword_kind(&mut self, _: KeywordKind) -> Self::Result {
        Self::Result::output()
    }
    fn visit_data_type(&mut self, data_type: &DataType) -> Self::Result {
        walk_data_type(self, data_type)
    }
    fn visit_data_type_size(&mut self, data_type_size: &DataTypeSize) -> Self::Result {
        walk_symbol(self, &data_type_size.left_paren);
        walk_symbol(self, &data_type_size.right_paren);
        Self::Result::output()
    }
    fn visit_data_type_numeric_size(&mut self, ns: &NumericSize) -> Self::Result {
        walk_symbol(self, &ns.left_paren);
        walk_symbol(self, &ns.right_paren);
        Self::Result::output()
    }
    fn visit_top_clause(&mut self, top_clause: &Top) -> Self::Result {
        walk_top_clause(self, top_clause)
    }
    fn visit_select_item(&mut self, select_item: &SelectItem) -> Self::Result {
        walk_select_item(self, select_item)
    }
    fn visit_table_clause(&mut self, table_clause: &TableArg) -> Self::Result {
        walk_table_clause(self, table_clause)
    }
    fn visit_where_clause(&mut self, where_clause: &WhereClause) -> Self::Result {
        walk_where_clause(self, where_clause)
    }
    fn visit_group_by_clause(&mut self, group_by_clause: &GroupByClause) -> Self::Result {
        walk_group_by_clause(self, group_by_clause)
    }
    fn visit_having_clause(&mut self, having_clause: &HavingClause) -> Self::Result {
        walk_having_clause(self, having_clause)
    }
    fn visit_order_by_clause(&mut self, order_by_clause: &OrderByClause) -> Self::Result {
        walk_order_by_clause(self, order_by_clause)
    }
    fn visit_table_source(&mut self, table_source: &TableSource) -> Self::Result {
        walk_table_source(self, table_source)
    }
    fn visit_table_join(&mut self, table_join: &Join) -> Self::Result {
        walk_table_join(self, table_join)
    }
    fn visit_table_join_condition(&mut self, table_join_condition: &JoinCondition) -> Self::Result {
        walk_table_join_condition(self, table_join_condition)
    }
    fn visit_table_join_type(&mut self, _: &JoinType) -> Self::Result {
        Self::Result::output()
    }
    fn visit_order_by_arg(&mut self, order_by_arg: &OrderByArg) -> Self::Result {
        walk_order_by_arg(self, order_by_arg)
    }
    fn visit_order_by_offset_fetch_clause(
        &mut self,
        offset_fetch_clause: &OffsetFetchClause,
    ) -> Self::Result {
        walk_order_by_offset_fetch_clause(self, offset_fetch_clause)
    }
    fn visit_order_by_offset_arg(&mut self, offset_arg: &OffsetArg) -> Self::Result {
        walk_order_by_offset_arg(self, offset_arg)
    }
    fn visit_order_by_fetch_arg(&mut self, fetch_arg: &FetchArg) -> Self::Result {
        walk_order_by_fetch_arg(self, fetch_arg)
    }
    fn visit_row_or_rows(&mut self, _: RowOrRows) -> Self::Result {
        Self::Result::output()
    }
    fn visit_rows_or_range(&mut self, _: RowsOrRange) -> Self::Result {
        Self::Result::output()
    }
    fn visit_first_or_next(&mut self, _: NextOrFirst) -> Self::Result {
        Self::Result::output()
    }
    fn visit_function_name(&mut self, fn_name: &FunctionName) -> Self::Result {
        walk_function_name(self, fn_name)
    }
    fn visit_function_over_clause(&mut self, over_clause: &OverClause) -> Self::Result {
        walk_function_over_clause(self, over_clause)
    }
    fn visit_function_over_clause_window_frame(
        &mut self,
        window_frame: &WindowFrame,
    ) -> Self::Result {
        walk_function_over_clause_window_frame(self, window_frame)
    }
    fn visit_function_over_clause_window_frame_bound(
        &mut self,
        window_frame_bound: &WindowFrameBound,
    ) -> Self::Result {
        match window_frame_bound {
            WindowFrameBound::CurrentRow
            | WindowFrameBound::UnboundedPreceding
            | WindowFrameBound::UnboundedFollowing => Self::Result::output(),
            WindowFrameBound::Preceding(e) | WindowFrameBound::Following(e) => {
                self.visit_expression(&e)
            }
        }
    }
    fn visit_case_condition(&mut self, case_condition: &CaseCondition) -> Self::Result {
        walk_case_condition(self, case_condition)
    }
    fn visit_common_table_expression(&mut self, cte: &CommonTableExpression) -> Self::Result {
        walk_common_table_expression(self, cte)
    }
    fn visit_expression_list(&mut self, list: &ExpressionList) -> Self::Result {
        walk_expression_list(self, list)
    }
    fn visit_execute_statement_procedure_parameter(
        &mut self,
        param: &ProcedureParameter,
    ) -> Self::Result {
        walk_execute_statement_procedure_parameter(self, param)
    }
    fn visit_execute_statement_procedure_parameter_name(
        &mut self,
        name: &ProcedureParameterName,
    ) -> Self::Result {
        walk_execute_statement_procedure_parameter_name(self, name)
    }
    fn visit_local_variable(&mut self, local_variable: &LocalVariable) -> Self::Result {
        walk_local_variable(self, local_variable)
    }
}

pub trait VisitorResult {
    fn output() -> Self;
}

impl VisitorResult for () {
    fn output() -> Self {
        ()
    }
}

#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for element in $list.iter() {
            $visitor.$method(element);
        }
    };
}

#[macro_export]
macro_rules! walk_opt_list {
    ($visitor: expr, $method: ident, $opt: expr) => {
        if let Some(o) = $opt {
            for element in o.iter() {
                $visitor.$method(element);
            }
        }
    };
}

#[macro_export]
macro_rules! walk_opt {
    ($visitor: expr, $method: ident, $opt: expr) => {
        if let Some(o) = $opt {
            $visitor.$method(o);
        }
    };
}

impl VisitorResult for String {
    fn output() -> Self {
        Self::default()
    }
}

pub fn walk_query<V: Visitor>(visitor: &mut V, query: &Query) -> V::Result {
    walk_list!(visitor, visit_statement, query.statements);
    V::Result::output()
}

pub fn walk_expression<V: Visitor>(visitor: &mut V, expression: &Expression) -> V::Result {
    match expression {
        Expression::Asterisk(s) => {
            visitor.visit_symbol(s);
            visitor.visit_asterisk()
        }
        Expression::Identifier(l)
        | Expression::QuotedIdentifier(l)
        | Expression::StringLiteral(l)
        | Expression::NumberLiteral(l)
        | Expression::LocalVariable(l) => visitor.visit_literal(l),
        Expression::Keyword(k) => visitor.visit_keyword(&k),
        Expression::Compound(e) => {
            walk_list!(visitor, visit_expression, e);
            V::Result::output()
        }
        Expression::Arithmetic {
            operator,
            left,
            right,
        } => {
            visitor.visit_arithmetic_operator(operator);
            visitor.visit_expression(left);
            visitor.visit_expression(right)
        }
        Expression::And {
            and_kw,
            left,
            right,
        } => {
            visitor.visit_keyword(and_kw);
            visitor.visit_expression(left);
            visitor.visit_expression(right)
        }
        Expression::Or { or_kw, left, right } => {
            visitor.visit_keyword(or_kw);
            visitor.visit_expression(left);
            visitor.visit_expression(right)
        }
        Expression::Comparison {
            operator,
            left,
            right,
        } => {
            visitor.visit_comparison_operator(operator);
            visitor.visit_expression(left);
            visitor.visit_expression(right)
        }
        Expression::Unary { operator, right } => {
            visitor.visit_unary_operator(operator);
            visitor.visit_expression(right)
        }
        Expression::Function {
            name,
            left_paren,
            args,
            right_paren,
            over,
        } => {
            visitor.visit_function_name(name);
            visitor.visit_symbol(left_paren);
            walk_opt_list!(visitor, visit_expression, args);
            visitor.visit_symbol(right_paren);
            walk_opt!(visitor, visit_function_over_clause, over);

            V::Result::output()
        }
        Expression::Cast {
            cast_kw,
            left_paren,
            expression,
            as_kw,
            data_type,
            right_paren,
        } => {
            visitor.visit_keyword(cast_kw);
            visitor.visit_symbol(left_paren);
            visitor.visit_expression(expression);
            visitor.visit_keyword(as_kw);
            visitor.visit_data_type(data_type);
            visitor.visit_symbol(right_paren)
        }
        Expression::InExpressionList {
            test_expression,
            in_kw,
            not_kw,
            left_paren,
            list,
            right_paren,
        } => {
            visitor.visit_expression(test_expression);
            visitor.visit_keyword(in_kw);
            walk_opt!(visitor, visit_keyword, not_kw);
            visitor.visit_symbol(left_paren);
            walk_list!(visitor, visit_expression, list);
            visitor.visit_symbol(right_paren);

            V::Result::output()
        }
        Expression::InSubquery {
            test_expression,
            in_kw,
            not_kw,
            subquery,
        } => {
            visitor.visit_expression(test_expression);
            visitor.visit_keyword(in_kw);
            walk_opt!(visitor, visit_keyword, not_kw);
            visitor.visit_expression(subquery)
        }
        Expression::Subquery {
            left_paren,
            select_statement,
            right_paren,
        } => {
            visitor.visit_symbol(left_paren);
            visitor.visit_select_statement(select_statement);
            visitor.visit_symbol(right_paren)
        }
        Expression::Between {
            test_expression,
            not_kw,
            between_kw,
            begin,
            and_kw,
            end,
        } => {
            visitor.visit_expression(test_expression);
            walk_opt!(visitor, visit_keyword, not_kw);
            visitor.visit_keyword(between_kw);
            visitor.visit_expression(begin);
            visitor.visit_keyword(and_kw);
            visitor.visit_expression(end)
        }
        Expression::Not { not_kw, expression } => {
            visitor.visit_keyword(not_kw);
            visitor.visit_expression(expression)
        }
        Expression::Exists {
            exists_kw,
            subquery,
        } => {
            visitor.visit_keyword(exists_kw);
            visitor.visit_expression(subquery)
        }
        Expression::All {
            all_kw,
            scalar_expression,
            comparison_op,
            subquery,
        } => {
            visitor.visit_keyword(all_kw);
            visitor.visit_expression(scalar_expression);
            visitor.visit_comparison_operator(comparison_op);
            visitor.visit_expression(subquery)
        }
        Expression::Some {
            some_kw,
            scalar_expression,
            comparison_op,
            subquery,
        } => {
            visitor.visit_keyword(some_kw);
            visitor.visit_expression(scalar_expression);
            visitor.visit_comparison_operator(comparison_op);
            visitor.visit_expression(subquery)
        }
        Expression::Any {
            any_kw,
            scalar_expression,
            comparison_op,
            subquery,
        } => {
            visitor.visit_keyword(any_kw);
            visitor.visit_expression(scalar_expression);
            visitor.visit_comparison_operator(comparison_op);
            visitor.visit_expression(subquery)
        }
        Expression::Like {
            match_expression,
            not_kw,
            like_kw,
            pattern,
        } => {
            visitor.visit_expression(match_expression);
            walk_opt!(visitor, visit_keyword, not_kw);
            visitor.visit_keyword(like_kw);
            visitor.visit_expression(pattern)
        }
        Expression::SimpleCase {
            case_kw,
            input_expression,
            conditions,
            end_kw,
        } => {
            visitor.visit_keyword(case_kw);
            visitor.visit_expression(input_expression);
            walk_list!(visitor, visit_case_condition, conditions);
            visitor.visit_keyword(end_kw)
        }
        Expression::SearchedCase {
            case_kw,
            conditions,
            end_kw,
        } => {
            visitor.visit_keyword(case_kw);
            walk_list!(visitor, visit_case_condition, conditions);
            visitor.visit_keyword(end_kw)
        }
    }
}

pub fn walk_union<V: Visitor>(visitor: &mut V, union: &Union) -> V::Result {
    visitor.visit_keyword(&union.union_kw);
    walk_opt!(visitor, visit_keyword, &union.all_kw);
    visitor.visit_select_statement(&union.select)
}

pub fn walk_statement<V: Visitor>(visitor: &mut V, stmt: &Statement) -> V::Result {
    match stmt {
        Statement::Select(s) => visitor.visit_select_statement(s),
        Statement::Insert(i) => visitor.visit_insert_statement(i),
        Statement::Update(_) => V::Result::output(),
        Statement::Delete(_) => V::Result::output(),
        Statement::CTE {
            with_kw,
            ctes,
            statement,
        } => {
            visitor.visit_keyword(with_kw);
            walk_list!(visitor, visit_common_table_expression, ctes);
            visitor.visit_common_table_expression_statement(statement)
        }
        Statement::Declare {
            declare_kw,
            variables,
        } => {
            visitor.visit_keyword(declare_kw);
            walk_list!(visitor, visit_local_variable, variables);
            V::Result::output()
        }
        Statement::SetLocalVariable {
            set_kw,
            name,
            equal_sign,
            value,
        } => {
            visitor.visit_keyword(set_kw);
            visitor.visit_expression(name);
            visitor.visit_symbol(equal_sign);
            visitor.visit_expression(value)
        }
        Statement::Execute {
            exec_kw,
            procedure_name,
            parameters,
        } => {
            visitor.visit_keyword(exec_kw);
            visitor.visit_expression(procedure_name);
            walk_list!(
                visitor,
                visit_execute_statement_procedure_parameter,
                parameters
            );
            V::Result::output()
        }
        Statement::Union { select, unions } => {
            visitor.visit_select_statement(select);
            walk_list!(visitor, visit_union, unions);
            V::Result::output()
        }
    }
}

pub fn walk_insert_statement<V: Visitor>(visitor: &mut V, stmt: &InsertStatement) -> V::Result {
    match stmt {
        InsertStatement::Values {
            insert_kw,
            into_kw,
            object,
            columns,
            values_kw,
            values,
        } => {
            visitor.visit_keyword(insert_kw);
            walk_opt!(visitor, visit_keyword, into_kw);
            visitor.visit_expression(object);
            walk_opt!(visitor, visit_expression_list, columns);
            visitor.visit_keyword(values_kw);
            visitor.visit_expression_list(values);
            V::Result::output()
        }
        InsertStatement::Table {
            insert_kw,
            into_kw,
            object,
            select_kw,
            top,
            columns,
            table,
            where_clause,
        } => {
            visitor.visit_keyword(insert_kw);
            walk_opt!(visitor, visit_keyword, into_kw);
            visitor.visit_expression(object);
            visitor.visit_keyword(select_kw);
            walk_opt!(visitor, visit_top_clause, top);
            walk_list!(visitor, visit_expression, columns);
            visitor.visit_table_clause(table);
            walk_opt!(visitor, visit_where_clause, where_clause);
            V::Result::output()
        }
    }
}

pub fn walk_select_statement<V: Visitor>(visitor: &mut V, stmt: &SelectStatement) -> V::Result {
    visitor.visit_keyword(&stmt.select);
    walk_opt!(visitor, visit_keyword, &stmt.distinct);
    walk_opt!(visitor, visit_keyword, &stmt.all);
    walk_opt!(visitor, visit_top_clause, &stmt.top);
    walk_list!(visitor, visit_select_item, &stmt.columns);
    walk_opt!(visitor, visit_table_clause, &stmt.table);
    walk_opt!(visitor, visit_where_clause, &stmt.where_clause);
    walk_opt!(visitor, visit_group_by_clause, &stmt.group_by);
    walk_opt!(visitor, visit_having_clause, &stmt.having);
    walk_opt!(visitor, visit_order_by_clause, &stmt.order_by);
    V::Result::output()
}

pub fn walk_common_table_expression_statement<V: Visitor>(
    visitor: &mut V,
    stmt: &CommonTableExpressionStatement,
) -> V::Result {
    match stmt {
        CommonTableExpressionStatement::Select(s) => visitor.visit_select_statement(s),
        CommonTableExpressionStatement::Insert(i) => visitor.visit_insert_statement(i),
        // CommonTableExpressionStatement::Update(u) => todo!(),
        // CommonTableExpressionStatement::Delete(d) => todo!(),
    }
}

pub fn walk_symbol<V: Visitor>(visitor: &mut V, symbol: &Symbol) -> V::Result {
    visitor.visit_span(&symbol.location)
}

pub fn walk_literal<V: Visitor>(visitor: &mut V, literal: &Literal) -> V::Result {
    visitor.visit_span(&literal.location)
}

pub fn walk_comparison_operator<V: Visitor>(visitor: &mut V, op: &ComparisonOperator) -> V::Result {
    visitor.visit_span(&op.location);
    visitor.visit_comparison_operator_kind(op.kind)
}

pub fn walk_arithmetic_operator<V: Visitor>(visitor: &mut V, op: &ArithmeticOperator) -> V::Result {
    visitor.visit_span(&op.location);
    visitor.visit_arithmetic_operator_kind(op.kind)
}

pub fn walk_unary_operator<V: Visitor>(visitor: &mut V, op: &UnaryOperator) -> V::Result {
    visitor.visit_unary_operator(op);
    visitor.visit_span(&op.location);
    visitor.visit_unary_operator_kind(op.kind)
}

pub fn walk_keyword<V: Visitor>(visitor: &mut V, keyword: &Keyword) -> V::Result {
    visitor.visit_span(&keyword.location);
    visitor.visit_keyword_kind(keyword.kind)
}

pub fn walk_data_type<V: Visitor>(visitor: &mut V, data_type: &DataType) -> V::Result {
    match data_type {
        DataType::Int(k)
        | DataType::BigInt(k)
        | DataType::TinyInt(k)
        | DataType::SmallInt(k)
        | DataType::Datetime(k)
        | DataType::Time(k)
        | DataType::Real(k)
        | DataType::Date(k)
        | DataType::Bit(k) => visitor.visit_keyword(&k),
        DataType::Decimal(k, ns) | DataType::Numeric(k, ns) => {
            visitor.visit_keyword(&k);
            walk_opt!(visitor, visit_data_type_numeric_size, &ns);
            V::Result::output()
        }
        DataType::Float(k, s) | DataType::Varchar(k, s) => {
            visitor.visit_keyword(&k);
            walk_opt!(visitor, visit_data_type_size, s);
            V::Result::output()
        }
    }
}

pub fn walk_top_clause<V: Visitor>(visitor: &mut V, top_clause: &Top) -> V::Result {
    visitor.visit_keyword(&top_clause.top);
    walk_opt_list!(visitor, visit_keyword, &top_clause.with_ties);
    walk_opt!(visitor, visit_keyword, &top_clause.percent);
    visitor.visit_expression(&top_clause.quantity)
}

pub fn walk_select_item<V: Visitor>(visitor: &mut V, select_item: &SelectItem) -> V::Result {
    match select_item {
        SelectItem::Wildcard(s) => {
            visitor.visit_symbol(s);
            visitor.visit_select_item_wild_card()
        }
        SelectItem::Unnamed(e) => visitor.visit_expression(e),
        SelectItem::WithAlias {
            expression,
            as_kw,
            alias,
        }
        | SelectItem::WildcardWithAlias {
            expression,
            as_kw,
            alias,
        } => {
            visitor.visit_expression(expression);
            walk_opt!(visitor, visit_keyword, as_kw);
            visitor.visit_expression(alias)
        }
        SelectItem::ReverseAliasAssign { alias, expression } => {
            visitor.visit_expression(alias);
            visitor.visit_expression(expression)
        }
    }
}

pub fn walk_table_clause<V: Visitor>(visitor: &mut V, table_clause: &TableArg) -> V::Result {
    visitor.visit_keyword(&table_clause.from);
    visitor.visit_table_source(&table_clause.table)
}

pub fn walk_where_clause<V: Visitor>(visitor: &mut V, where_clause: &WhereClause) -> V::Result {
    visitor.visit_keyword(&where_clause.where_kw);
    visitor.visit_expression(&where_clause.expression)
}

pub fn walk_group_by_clause<V: Visitor>(
    visitor: &mut V,
    group_by_clause: &GroupByClause,
) -> V::Result {
    walk_list!(visitor, visit_keyword, &group_by_clause.group_by_kws);
    walk_list!(visitor, visit_expression, &group_by_clause.expressions);
    V::Result::output()
}

pub fn walk_having_clause<V: Visitor>(visitor: &mut V, having_clause: &HavingClause) -> V::Result {
    visitor.visit_having_clause(having_clause);
    visitor.visit_keyword(&having_clause.having_kw);
    visitor.visit_expression(&having_clause.expression)
}

pub fn walk_order_by_clause<V: Visitor>(
    visitor: &mut V,
    order_by_clause: &OrderByClause,
) -> V::Result {
    walk_list!(visitor, visit_keyword, &order_by_clause.order_by_kws);
    walk_list!(visitor, visit_order_by_arg, &order_by_clause.expressions);
    walk_opt!(
        visitor,
        visit_order_by_offset_fetch_clause,
        &order_by_clause.offset_fetch_clause
    );

    V::Result::output()
}

pub fn walk_table_source<V: Visitor>(visitor: &mut V, table_source: &TableSource) -> V::Result {
    match table_source {
        TableSource::Table { name, alias } => {
            visitor.visit_expression(name);
            walk_opt!(visitor, visit_expression, alias);
        }
        TableSource::Derived { query, alias } => {
            visitor.visit_expression(query);
            visitor.visit_expression(alias);
        }
        TableSource::TableValuedFunction { function, alias } => {
            visitor.visit_expression(function);
            walk_opt!(visitor, visit_expression, alias);
        }
    }

    V::Result::output()
}

pub fn walk_table_join<V: Visitor>(visitor: &mut V, table_join: &Join) -> V::Result {
    walk_list!(visitor, visit_keyword, &table_join.join);
    visitor.visit_table_join_type(&table_join.join_type);
    visitor.visit_table_source(&table_join.table);
    walk_opt!(visitor, visit_table_join_condition, &table_join.condition);

    V::Result::output()
}

pub fn walk_table_join_condition<V: Visitor>(
    visitor: &mut V,
    table_join_condition: &JoinCondition,
) -> V::Result {
    visitor.visit_keyword(&table_join_condition.on_kw);
    visitor.visit_expression(&table_join_condition.condition);

    V::Result::output()
}

pub fn walk_order_by_arg<V: Visitor>(visitor: &mut V, order_by_arg: &OrderByArg) -> V::Result {
    visitor.visit_expression(&order_by_arg.column);
    walk_opt!(visitor, visit_keyword, &order_by_arg.order_kw);

    V::Result::output()
}

pub fn walk_order_by_offset_fetch_clause<V: Visitor>(
    visitor: &mut V,
    offset_fetch_clause: &OffsetFetchClause,
) -> V::Result {
    visitor.visit_order_by_offset_arg(&offset_fetch_clause.offset);
    walk_opt!(
        visitor,
        visit_order_by_fetch_arg,
        &offset_fetch_clause.fetch
    );

    V::Result::output()
}

pub fn walk_order_by_offset_arg<V: Visitor>(visitor: &mut V, offset_arg: &OffsetArg) -> V::Result {
    visitor.visit_keyword(&offset_arg.offset_kw);
    visitor.visit_expression(&offset_arg.value);
    visitor.visit_keyword(&offset_arg.row_or_rows_kw);
    visitor.visit_row_or_rows(offset_arg.row)
}

pub fn walk_order_by_fetch_arg<V: Visitor>(visitor: &mut V, fetch_arg: &FetchArg) -> V::Result {
    visitor.visit_keyword(&fetch_arg.fetch_kw);
    visitor.visit_expression(&fetch_arg.value);
    visitor.visit_keyword(&fetch_arg.first_or_next_kw);
    visitor.visit_first_or_next(fetch_arg.first);
    visitor.visit_keyword(&fetch_arg.row_or_rows_kw);
    visitor.visit_row_or_rows(fetch_arg.row);
    visitor.visit_keyword(&fetch_arg.only_kw)
}

pub fn walk_function_name<V: Visitor>(visitor: &mut V, fn_name: &FunctionName) -> V::Result {
    match fn_name {
        FunctionName::Builtin(k) => visitor.visit_keyword(&k),
        FunctionName::User(e) => visitor.visit_expression(&e),
    }
}

pub fn walk_function_over_clause<V: Visitor>(
    visitor: &mut V,
    over_clause: &OverClause,
) -> V::Result {
    visitor.visit_keyword(&over_clause.over_kw);
    visitor.visit_symbol(&over_clause.left_paren);
    walk_opt_list!(visitor, visit_keyword, &over_clause.partition_by_kws);
    walk_list!(visitor, visit_expression, &over_clause.partition_by);
    walk_opt_list!(visitor, visit_keyword, &over_clause.order_by_kws);
    walk_list!(visitor, visit_order_by_arg, &over_clause.order_by);
    walk_opt!(
        visitor,
        visit_function_over_clause_window_frame,
        &over_clause.window_frame
    );
    visitor.visit_symbol(&over_clause.right_paren);

    V::Result::output()
}

pub fn walk_function_over_clause_window_frame<V: Visitor>(
    visitor: &mut V,
    window_frame: &WindowFrame,
) -> V::Result {
    visitor.visit_rows_or_range(window_frame.rows_or_range);
    visitor.visit_keyword(&window_frame.rows_or_range_kw);
    walk_list!(visitor, visit_keyword, &window_frame.start_bound_keywords);
    visitor.visit_function_over_clause_window_frame_bound(&window_frame.start);
    walk_opt!(visitor, visit_keyword, &window_frame.between_kw);
    walk_opt!(visitor, visit_keyword, &window_frame.and_kw);
    walk_opt_list!(visitor, visit_keyword, &window_frame.end_bound_keywords);
    walk_opt!(
        visitor,
        visit_function_over_clause_window_frame_bound,
        &window_frame.end
    );

    V::Result::output()
}

pub fn walk_case_condition<V: Visitor>(
    visitor: &mut V,
    case_condition: &CaseCondition,
) -> V::Result {
    match case_condition {
        CaseCondition::WhenCondition {
            when_kw,
            when_expression,
            then_kw,
            result_expression,
        } => {
            visitor.visit_keyword(when_kw);
            visitor.visit_expression(when_expression);
            visitor.visit_keyword(then_kw);
            visitor.visit_expression(result_expression);
        }
        CaseCondition::ElseCondition {
            else_kw,
            result_expression,
        } => {
            visitor.visit_keyword(else_kw);
            visitor.visit_expression(result_expression);
        }
    };

    V::Result::output()
}

pub fn walk_common_table_expression<V: Visitor>(
    visitor: &mut V,
    cte: &CommonTableExpression,
) -> V::Result {
    visitor.visit_expression(&cte.name);
    walk_opt!(visitor, visit_expression_list, &cte.columns);
    visitor.visit_keyword(&cte.as_kw);
    visitor.visit_select_statement(&cte.query)
}

pub fn walk_expression_list<V: Visitor>(visitor: &mut V, list: &ExpressionList) -> V::Result {
    visitor.visit_symbol(&list.left_paren);
    walk_list!(visitor, visit_expression, &list.items);
    visitor.visit_symbol(&list.right_paren)
}

pub fn walk_execute_statement_procedure_parameter<V: Visitor>(
    visitor: &mut V,
    param: &ProcedureParameter,
) -> V::Result {
    if let Some(name) = &param.name {
        visitor.visit_execute_statement_procedure_parameter_name(&name.0);
        visitor.visit_symbol(&name.1);
    }
    visitor.visit_expression(&param.value)
}

pub fn walk_execute_statement_procedure_parameter_name<V: Visitor>(
    visitor: &mut V,
    name: &ProcedureParameterName,
) -> V::Result {
    visitor.visit_span(&name.location)
}

pub fn walk_local_variable<V: Visitor>(
    visitor: &mut V,
    local_variable: &LocalVariable,
) -> V::Result {
    visitor.visit_expression(&local_variable.name);
    visitor.visit_data_type(&local_variable.data_type);
    if let Some(value) = &local_variable.value {
        visitor.visit_symbol(&value.0);
        visitor.visit_expression(&value.1);
    }

    V::Result::output()
}

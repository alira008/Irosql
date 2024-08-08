use sql_lexer::Span;

use crate::ast::{
    ArithmeticOperator, ArithmeticOperatorKind, CaseCondition, CommonTableExpression,
    CommonTableExpressionStatement, ComparisonOperator, ComparisonOperatorKind, DataType,
    Expression, FetchArg, FunctionName, HavingClause, InsertStatement, Join, JoinType, Keyword,
    KeywordKind, Literal, LocalVariable, NextOrFirst, NumericSize, OffsetArg, OffsetFetchClause,
    OrderByArg, OrderByClause, OverClause, ProcedureParameter, ProcedureParameterName, Query,
    RowOrRows, RowsOrRange, SelectItem, SelectStatement, Statement, TableArg, TableSource, Top,
    UnaryOperator, UnaryOperatorKind, WhereClause, WindowFrame, WindowFrameBound,
};

pub trait Visitor {
    type Result: VisitorResult;

    fn visit_query(&mut self, query: &Query) -> Self::Result;
    fn visit_expression(&mut self, expr: &Expression) -> Self::Result;
    fn visit_statement(&mut self, stmt: &Statement) -> Self::Result;
    fn visit_insert_statement(&mut self, stmt: &InsertStatement) -> Self::Result;
    fn visit_select_statement(&mut self, stmt: &SelectStatement) -> Self::Result;
    fn visit_common_table_expression_statement(
        &mut self,
        stmt: &CommonTableExpressionStatement,
    ) -> Self::Result;

    fn visit_literal(&mut self, literal: &Literal) -> Self::Result;
    fn visit_comparison_operator(&mut self, op: &ComparisonOperator) -> Self::Result;
    fn visit_comparison_operator_kind(&mut self, op: &ComparisonOperatorKind) -> Self::Result;
    fn visit_arithmetic_operator(&mut self, op: &ArithmeticOperator) -> Self::Result;
    fn visit_arithmetic_operator_kind(&mut self, op: &ArithmeticOperatorKind) -> Self::Result;
    fn visit_unary_operator(&mut self, op: &UnaryOperator) -> Self::Result;
    fn visit_unary_operator_kind(&mut self, op: &UnaryOperatorKind) -> Self::Result;
    fn visit_keyword(&mut self, keyword: &Keyword) -> Self::Result;
    fn visit_keyword_kind(&mut self, keyword_kind: KeywordKind) -> Self::Result;
    fn visit_data_type(&mut self, data_type: &DataType) -> Self::Result;
    fn visit_data_type_numeric_size(&mut self, numeric_size: &NumericSize) -> Self::Result;
    fn visit_top_clause(&mut self, top_clause: &Top) -> Self::Result;
    fn visit_select_item(&mut self, select_item: &SelectItem) -> Self::Result;
    fn visit_table_clause(&mut self, table_clause: &TableArg) -> Self::Result;
    fn visit_where_clause(&mut self, where_clause: &WhereClause) -> Self::Result;
    fn visit_having_clause(&mut self, having_clause: &HavingClause) -> Self::Result;
    fn visit_order_by_clause(&mut self, order_by_clause: &OrderByClause) -> Self::Result;
    fn visit_table_source(&mut self, table_source: &TableSource) -> Self::Result;
    fn visit_table_join(&mut self, table_join: &Join) -> Self::Result;
    fn visit_table_join_type(&mut self, table_join_type: &JoinType) -> Self::Result;
    fn visit_order_by_arg(&mut self, order_by_arg: &OrderByArg) -> Self::Result;
    fn visit_order_by_offset_fetch_clause(
        &mut self,
        offset_fetch_clause: &OffsetFetchClause,
    ) -> Self::Result;
    fn visit_order_by_offset_arg(&mut self, offset_arg: &OffsetArg) -> Self::Result;
    fn visit_order_by_fetch_arg(&mut self, fetch_arg: &FetchArg) -> Self::Result;
    fn visit_row_or_rows(&mut self, row_or_rows: RowOrRows) -> Self::Result;
    fn visit_rows_or_range(&mut self, rows_or_range: RowsOrRange) -> Self::Result;
    fn visit_first_or_next(&mut self, first_or_next: NextOrFirst) -> Self::Result;
    fn visit_function_name(&mut self, fn_name: &FunctionName) -> Self::Result;
    fn visit_function_over_clause(&mut self, over_clause: &OverClause) -> Self::Result;
    fn visit_function_over_clause_window_frame(
        &mut self,
        window_frame: &WindowFrame,
    ) -> Self::Result;
    fn visit_function_over_clause_window_frame_bound(
        &mut self,
        window_frame_bound: &WindowFrameBound,
    ) -> Self::Result;
    fn visit_case_condition(&mut self, case_condition: &CaseCondition) -> Self::Result;
    fn visit_common_table_expression(&mut self, cte: &CommonTableExpression) -> Self::Result;
    fn visit_execute_statement_procedure_parameter(
        &mut self,
        param: &ProcedureParameter,
    ) -> Self::Result;
    fn visit_execute_statement_procedure_parameter_name(
        &mut self,
        name: &ProcedureParameterName,
    ) -> Self::Result;
    fn visit_local_variable(&mut self, local_variable: &LocalVariable) -> Self::Result;
}

pub trait VisitorResult {
    fn output() -> Self;
}

macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for element in $list.iter() {
            $method($visitor, element);
        }
    };
}

macro_rules! walk_opt_list {
    ($visitor: expr, $method: ident, $opt: expr) => {
        if let Some(o) = $opt {
            for element in o.iter() {
                $method($visitor, element);
            }
        }
    };
}

macro_rules! visit_opt {
    ($visitor: expr, $method: ident, $opt: ident) => {
        if let Some(o) = $opt {
            $visitor.$method(o);
        }
    };
}

macro_rules! walk_opt {
    ($visitor: expr, $method: ident, $opt: expr) => {
        if let Some(o) = $opt {
            $method($visitor, o);
        }
    };
}

impl VisitorResult for String {
    fn output() -> Self {
        // walk_list!("hello");
        Self::default()
    }
}

pub fn walk_query<V: Visitor>(visitor: &mut V, query: &Query) -> V::Result {
    visitor.visit_query(query);
    walk_list!(visitor, walk_statement, query.statements);
    V::Result::output()
}

pub fn walk_expression<V: Visitor>(visitor: &mut V, expression: &Expression) -> V::Result {
    V::Result::output()
}

pub fn walk_statement<V: Visitor>(visitor: &mut V, stmt: &Statement) -> V::Result {
    visitor.visit_statement(stmt);
    V::Result::output()
}

pub fn walk_insert_statement<V: Visitor>(visitor: &mut V, stmt: &InsertStatement) -> V::Result {
    visitor.visit_insert_statement(stmt);
    V::Result::output()
}

pub fn walk_select_statement<V: Visitor>(visitor: &mut V, stmt: &SelectStatement) -> V::Result {
    visitor.visit_select_statement(stmt);
    walk_keyword(visitor, &stmt.select);
    walk_opt!(visitor, walk_keyword, &stmt.distinct);
    walk_opt!(visitor, walk_keyword, &stmt.all);
    walk_opt!(visitor, walk_top_clause, &stmt.top);
    walk_list!(visitor, walk_select_item, &stmt.columns);
    V::Result::output()
}

pub fn walk_common_table_expression_statement<V: Visitor>(
    visitor: &mut V,
    stmt: &CommonTableExpressionStatement,
) -> V::Result {
    match stmt {
        CommonTableExpressionStatement::Select(s) => walk_select_statement(visitor, s),
        CommonTableExpressionStatement::Insert(i) => walk_insert_statement(visitor, i),
        // CommonTableExpressionStatement::Update(u) => todo!(),
        // CommonTableExpressionStatement::Delete(d) => todo!(),
    }
}

pub fn walk_span<V: Visitor>(visitor: &mut V, span: &Span) -> V::Result {
    V::Result::output()
}

pub fn walk_literal<V: Visitor>(visitor: &mut V, literal: &Literal) -> V::Result {
    visitor.visit_literal(literal);
    walk_span(visitor, &literal.location)
}

pub fn walk_comparison_operator<V: Visitor>(visitor: &mut V, op: &ComparisonOperator) -> V::Result {
    visitor.visit_comparison_operator(op);
    walk_span(visitor, &op.location);
    walk_comparison_operator_kind(visitor, &op.kind)
}

pub fn walk_comparison_operator_kind<V: Visitor>(
    visitor: &mut V,
    kind: &ComparisonOperatorKind,
) -> V::Result {
    visitor.visit_comparison_operator_kind(kind)
}

pub fn walk_arithmetic_operator<V: Visitor>(visitor: &mut V, op: &ArithmeticOperator) -> V::Result {
    visitor.visit_arithmetic_operator(op);
    walk_span(visitor, &op.location);
    walk_arithmetic_operator_kind(visitor, &op.kind)
}

pub fn walk_arithmetic_operator_kind<V: Visitor>(
    visitor: &mut V,
    kind: &ArithmeticOperatorKind,
) -> V::Result {
    visitor.visit_arithmetic_operator_kind(kind)
}

pub fn walk_unary_operator<V: Visitor>(visitor: &mut V, op: &UnaryOperator) -> V::Result {
    visitor.visit_unary_operator(op);
    walk_span(visitor, &op.location);
    walk_unary_operator_kind(visitor, &op.kind)
}

pub fn walk_unary_operator_kind<V: Visitor>(
    visitor: &mut V,
    kind: &UnaryOperatorKind,
) -> V::Result {
    visitor.visit_unary_operator_kind(kind)
}

pub fn walk_keyword<V: Visitor>(visitor: &mut V, keyword: &Keyword) -> V::Result {
    visitor.visit_keyword(keyword);
    walk_keyword_kind(visitor, keyword.kind);
    V::Result::output()
}

pub fn walk_keyword_kind<V: Visitor>(visitor: &mut V, keyword_kind: KeywordKind) -> V::Result {
    visitor.visit_keyword_kind(keyword_kind);
    V::Result::output()
}

pub fn walk_data_type<V: Visitor>(visitor: &mut V, data_type: DataType) -> V::Result {
    visitor.visit_data_type(&data_type);
    match data_type {
        DataType::Int(k)
        | DataType::BigInt(k)
        | DataType::TinyInt(k)
        | DataType::SmallInt(k)
        | DataType::Datetime(k)
        | DataType::Time(k)
        | DataType::Real(k)
        | DataType::Date(k)
        | DataType::Bit(k) => walk_keyword(visitor, &k),
        DataType::Decimal(k, ns) | DataType::Numeric(k, ns) => {
            walk_keyword(visitor, &k);
            walk_opt!(visitor, walk_data_type_numeric_size, &ns);
            V::Result::output()
        }
        DataType::Float(k, _) | DataType::Varchar(k, _) => walk_keyword(visitor, &k),
    }
}

pub fn walk_data_type_numeric_size<V: Visitor>(
    visitor: &mut V,
    numeric_size: &NumericSize,
) -> V::Result {
    visitor.visit_data_type_numeric_size(&numeric_size);
    V::Result::output()
}

pub fn walk_top_clause<V: Visitor>(visitor: &mut V, top_clause: &Top) -> V::Result {
    walk_keyword(visitor, &top_clause.top);
    walk_opt_list!(visitor, walk_keyword, &top_clause.with_ties);
    walk_opt!(visitor, walk_keyword, &top_clause.percent);
    walk_expression(visitor, &top_clause.quantity);

    V::Result::output()
}

pub fn walk_select_item<V: Visitor>(visitor: &mut V, select_item: &SelectItem) -> V::Result {
    visitor.visit_select_item(select_item);
    match select_item {
        SelectItem::Wildcard => {}
        SelectItem::Unnamed(e) => {
            walk_expression(visitor, e);
        }
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
            walk_expression(visitor, expression);
            walk_opt!(visitor, walk_keyword, as_kw);
            walk_expression(visitor, alias);
        }
        SelectItem::ReverseAliasAssign { alias, expression } => {
            walk_expression(visitor, alias);
            walk_expression(visitor, expression);
        }
    }
    V::Result::output()
}

pub fn walk_table_clause<V: Visitor>(visitor: &mut V, table_clause: &TableArg) -> V::Result {
    visitor.visit_table_clause(table_clause);
    walk_keyword(visitor, &table_clause.from);
    walk_table_source(visitor, &table_clause.table)
}

pub fn walk_where_clause<V: Visitor>(visitor: &mut V, where_clause: &WhereClause) -> V::Result {
    visitor.visit_where_clause(where_clause);
    walk_keyword(visitor, &where_clause.where_kw);
    walk_expression(visitor, &where_clause.expression)
}

pub fn walk_having_clause<V: Visitor>(visitor: &mut V, having_clause: &HavingClause) -> V::Result {
    visitor.visit_having_clause(having_clause);
    walk_keyword(visitor, &having_clause.having_kw);
    walk_expression(visitor, &having_clause.expression)
}

pub fn walk_order_by_clause<V: Visitor>(
    visitor: &mut V,
    order_by_clause: &OrderByClause,
) -> V::Result {
    visitor.visit_order_by_clause(order_by_clause);
    walk_list!(visitor, walk_keyword, &order_by_clause.order_by_kws);
    walk_list!(visitor, walk_order_by_arg, &order_by_clause.expressions);
    walk_opt!(
        visitor,
        walk_offset_fetch_clause,
        &order_by_clause.offset_fetch_clause
    );

    V::Result::output()
}

pub fn walk_table_source<V: Visitor>(visitor: &mut V, table_source: &TableSource) -> V::Result {
    visitor.visit_table_source(table_source);
    match table_source {
        TableSource::Table { name, alias } => {
            walk_expression(visitor, name);
            walk_opt!(visitor, walk_expression, alias);
        }
        TableSource::Derived { query, alias } => {
            walk_expression(visitor, query);
            walk_expression(visitor, alias);
        }
        TableSource::TableValuedFunction { function, alias } => {
            walk_expression(visitor, function);
            walk_opt!(visitor, walk_expression, alias);
        }
    }

    V::Result::output()
}

pub fn walk_table_join<V: Visitor>(visitor: &mut V, table_join: &Join) -> V::Result {
    visitor.visit_table_join(&table_join);
    walk_list!(visitor, walk_keyword, &table_join.join);
    walk_table_join_type(visitor, &table_join.join_type);
    walk_keyword(visitor, &table_join.on);
    walk_table_source(visitor, &table_join.table);
    walk_opt!(visitor, walk_expression, &table_join.condition);

    V::Result::output()
}

pub fn walk_table_join_type<V: Visitor>(visitor: &mut V, table_join_type: &JoinType) -> V::Result {
    visitor.visit_table_join_type(&table_join_type)
}

pub fn walk_order_by_arg<V: Visitor>(visitor: &mut V, order_by_arg: &OrderByArg) -> V::Result {
    visitor.visit_order_by_arg(&order_by_arg);
    walk_opt!(visitor, walk_keyword, &order_by_arg.order_kw);

    V::Result::output()
}

pub fn walk_order_by_offset_fetch_clause<V: Visitor>(
    visitor: &mut V,
    offset_fetch_clause: &OffsetFetchClause,
) -> V::Result {
    visitor.visit_order_by_offset_fetch_clause(&offset_fetch_clause);
    walk_order_by_offset_arg(visitor, &offset_fetch_clause.offset);
    walk_opt!(visitor, walk_order_by_fetch_arg, &offset_fetch_clause.fetch);

    V::Result::output()
}

pub fn walk_order_by_offset_arg<V: Visitor>(visitor: &mut V, offset_arg: &OffsetArg) -> V::Result {
    visitor.visit_order_by_offset_arg(&offset_arg);
    walk_keyword(visitor, &offset_arg.offset_kw);
    walk_expression(visitor, &offset_arg.value);
    walk_keyword(visitor, &offset_arg.row_or_rows_kw);
    walk_row_or_rows(visitor, offset_arg.row)
}

pub fn walk_order_by_fetch_arg<V: Visitor>(visitor: &mut V, fetch_arg: &FetchArg) -> V::Result {
    visitor.visit_order_by_fetch_arg(&fetch_arg);
    walk_keyword(visitor, &fetch_arg.fetch_kw);
    walk_expression(visitor, &fetch_arg.value);
    walk_keyword(visitor, &fetch_arg.first_or_next_kw);
    walk_first_or_next(visitor, fetch_arg.first);
    walk_keyword(visitor, &fetch_arg.row_or_rows_kw);
    walk_row_or_rows(visitor, fetch_arg.row);
    walk_keyword(visitor, &fetch_arg.only_kw)
}

pub fn walk_row_or_rows<V: Visitor>(visitor: &mut V, row_or_rows: RowOrRows) -> V::Result {
    visitor.visit_row_or_rows(row_or_rows)
}

pub fn walk_rows_or_range<V: Visitor>(visitor: &mut V, rows_or_range: RowsOrRange) -> V::Result {
    visitor.visit_rows_or_range(rows_or_range)
}

pub fn walk_first_or_next<V: Visitor>(visitor: &mut V, first_or_next: NextOrFirst) -> V::Result {
    visitor.visit_first_or_next(first_or_next)
}

pub fn walk_function_name<V: Visitor>(visitor: &mut V, fn_name: FunctionName) -> V::Result {
    visitor.visit_function_name(&fn_name);
    match fn_name {
        FunctionName::Builtin(k) => walk_keyword(visitor, &k),
        FunctionName::User(e) => walk_expression(visitor, &e),
    }
}

pub fn walk_function_over_clause<V: Visitor>(
    visitor: &mut V,
    over_clause: OverClause,
) -> V::Result {
    visitor.visit_function_over_clause(&over_clause);
    walk_keyword(visitor, &over_clause.over_kw);
    walk_opt_list!(visitor, walk_keyword, &over_clause.partition_by_kws);
    walk_list!(visitor, walk_expression, &over_clause.partition_by);
    walk_opt_list!(visitor, walk_keyword, &over_clause.order_by_kws);
    walk_list!(visitor, walk_order_by_arg, &over_clause.order_by);
    walk_opt!(
        visitor,
        walk_function_over_clause_window_frame,
        &over_clause.window_frame
    );

    V::Result::output()
}

pub fn walk_function_over_clause_window_frame<V: Visitor>(
    visitor: &mut V,
    window_frame: &WindowFrame,
) -> V::Result {
    visitor.visit_function_over_clause_window_frame(&window_frame);
    walk_rows_or_range(visitor, window_frame.rows_or_range);
    walk_keyword(visitor, &window_frame.rows_or_range_kw);
    walk_list!(visitor, walk_keyword, &window_frame.start_bound_keywords);
    walk_function_over_clause_window_frame_bound(visitor, &window_frame.start);
    walk_opt!(visitor, walk_keyword, &window_frame.between_kw);
    walk_opt!(visitor, walk_keyword, &window_frame.and_kw);
    walk_opt_list!(visitor, walk_keyword, &window_frame.end_bound_keywords);
    walk_opt!(
        visitor,
        walk_function_over_clause_window_frame_bound,
        &window_frame.end
    );

    V::Result::output()
}

pub fn walk_function_over_clause_window_frame_bound<V: Visitor>(
    visitor: &mut V,
    window_frame_bound: &WindowFrameBound,
) -> V::Result {
    visitor.visit_function_over_clause_window_frame_bound(&window_frame_bound);
    match window_frame_bound {
        WindowFrameBound::CurrentRow
        | WindowFrameBound::UnboundedPreceding
        | WindowFrameBound::UnboundedFollowing => V::Result::output(),
        WindowFrameBound::Preceding(e) | WindowFrameBound::Following(e) => {
            walk_expression(visitor, &e)
        }
    }
}

pub fn walk_case_condition<V: Visitor>(
    visitor: &mut V,
    case_condition: &CaseCondition,
) -> V::Result {
    visitor.visit_case_condition(&case_condition);
    match case_condition {
        CaseCondition::WhenCondition {
            when_kw,
            when_expression,
            then_kw,
            result_expression,
        } => {
            walk_keyword(visitor, when_kw);
            walk_expression(visitor, when_expression);
            walk_keyword(visitor, then_kw);
            walk_expression(visitor, result_expression);
        }
        CaseCondition::ElseCondition {
            else_kw,
            result_expression,
        } => {
            walk_keyword(visitor, else_kw);
            walk_expression(visitor, result_expression);
        }
    };

    V::Result::output()
}

pub fn walk_common_table_expression<V: Visitor>(
    visitor: &mut V,
    cte: &CommonTableExpression,
) -> V::Result {
    visitor.visit_common_table_expression(&cte);
    walk_expression(visitor, &cte.name);
    walk_opt_list!(visitor, walk_expression, &cte.columns);
    walk_keyword(visitor, &cte.as_kw);
    walk_select_statement(visitor, &cte.query)
}

pub fn walk_execute_statement_procedure_parameter<V: Visitor>(
    visitor: &mut V,
    param: &ProcedureParameter,
) -> V::Result {
    visitor.visit_execute_statement_procedure_parameter(&param);
    walk_expression(visitor, &param.value)
}

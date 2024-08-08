use crate::ast::{
    ArithmeticOperator, ArithmeticOperatorKind, CaseCondition, CommonTableExpression,
    CommonTableExpressionStatement, ComparisonOperator, ComparisonOperatorKind, DataType,
    Expression, FetchArg, FunctionName, HavingClause, InsertStatement, Join, JoinType, Keyword,
    Literal, LocalVariable, NumericSize, OffsetArg, OffsetFetchClause, OrderByArg, OrderByClause,
    OverClause, ProcedureParameter, ProcedureParameterName, Query, SelectItem, SelectStatement,
    Statement, TableArg, TableSource, Top, UnaryOperator, UnaryOperatorKind, WhereClause,
    WindowFrame, WindowFrameBound,
};

pub trait Visitor<T> {
    fn visit_literal(&self, literal: Literal) -> T;
    fn visit_comparison_operator(&self, op: ComparisonOperator) -> T;
    fn visit_comparison_operator_kind(&self, op: ComparisonOperatorKind) -> T;
    fn visit_arithmetic_operator(&self, op: ArithmeticOperator) -> T;
    fn visit_arithmetic_operator_kind(&self, op: ArithmeticOperatorKind) -> T;
    fn visit_unary_operator(&self, op: UnaryOperator) -> T;
    fn visit_unary_operator_kind(&self, op: UnaryOperatorKind) -> T;
    fn visit_keyword(&self, keyword: Keyword) -> T;
    fn visit_data_type(&self, data_type: DataType) -> T;
    fn visit_data_type_numeric_size(&self, numeric_size: NumericSize) -> T;
    fn visit_expression(&self, expr: Expression) -> T;
    fn visit_statement(&self, stmt: Statement) -> T;
    fn visit_select_statement(&self, stmt: SelectStatement) -> T;
    fn visit_top_clause(&self, top_clause: Top) -> T;
    fn visit_select_item(&self, select_item: SelectItem) -> T;
    fn visit_table_clause(&self, table_clause: TableArg) -> T;
    fn visit_where_clause(&self, where_clause: WhereClause) -> T;
    fn visit_having_clause(&self, having_clause: HavingClause) -> T;
    fn visit_order_by_clause(&self, order_by_clause: OrderByClause) -> T;
    fn visit_table_source(&self, table_source: TableSource) -> T;
    fn visit_table_join(&self, table_join: Join) -> T;
    fn visit_table_join_type(&self, table_join_type: JoinType) -> T;
    fn visit_order_by_arg(&self, order_by_arg: OrderByArg) -> T;
    fn visit_order_by_offset_fetch_clause(&self, offset_fetch_clause: OffsetFetchClause) -> T;
    fn visit_order_by_offset_arg(&self, offset_arg: OffsetArg) -> T;
    fn visit_order_by_fetch_arg(&self, fetch_arg: FetchArg) -> T;
    fn visit_function_name(&self, fn_name: FunctionName) -> T;
    fn visit_function_over_clause(&self, over_clause: OverClause) -> T;
    fn visit_function_over_clause_window_frame(&self, window_frame: WindowFrame) -> T;
    fn visit_function_over_clause_window_frame_bound(
        &self,
        window_frame_bound: WindowFrameBound,
    ) -> T;
    fn visit_case_condition(&self, case_condition: CaseCondition) -> T;
    fn visit_insert_statement(&self, stmt: InsertStatement) -> T;
    fn visit_common_table_expression_statement(&self, stmt: CommonTableExpressionStatement) -> T;
    fn visit_common_table_expression(&self, cte: CommonTableExpression) -> T;
    fn visit_execute_statement_procedure_parameter(&self, param: ProcedureParameter) -> T;
    fn visit_execute_statement_procedure_parameter_name(&self, name: ProcedureParameterName) -> T;
    fn visit_local_variable(&self, local_variable: LocalVariable) -> T;
    fn visit_query(&self, query: Query) -> T;
}

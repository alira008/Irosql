use crate::{
    ast::{
        CommonTableExpression, Expression, FetchArg, IntoArg, Join, JoinType, NextOrFirst,
        OffsetArg, OrderByArg, OverClause, Query, RowOrRows, RowsOrRange, SelectItem,
        SelectStatement, Statement, TableArg, TableSource, TopArg, WindowFrame, WindowFrameBound, DataType, LocalVariable, ExecOrExecute, ProcedureParameter, CommonTableExpressionStatement, InsertStatement, UpdateStatement, DeleteStatement, UpdateSet,
    },
    token::Token,
};

pub trait Visitor {
    fn visit_query(&mut self, query: &Query);
    fn visit_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_token(&mut self, _identifier: &Token) {}
    fn visit_binary_expression(&mut self, expression: &Expression) {
        walk_binary_expression(self, expression);
    }
    fn visit_unary_expression(&mut self, expression: &Expression) {
        walk_unary_expression(self, expression);
    }
    fn visit_group_expression(&mut self, expression: &Expression) {
        walk_group_expression(self, expression);
    }
    fn visit_statement(&mut self, statement: &Statement) {
        walk_statement(self, statement);
    }
    fn visit_select_query(&mut self, query: &SelectStatement) {
        self.visit_select_top_argument(&query.top);
        self.visit_select_columns(&query.columns);
        self.visit_select_into_table(&query.into_table);
        self.visit_select_table(&query.table);
        self.visit_select_where_clause(&query.where_clause);
        self.visit_select_group_by(&query.group_by);
        self.visit_select_having(&query.having);
        self.visit_select_order_by(&query.order_by);
        self.visit_select_offset(&query.offset);
        self.visit_select_fetch(&query.fetch);
    }
    fn visit_select_top_argument(&mut self, arg: &Option<TopArg>) {
        if let Some(top_arg) = arg {
            self.visit_expression(&top_arg.quantity);
        }
    }
    fn visit_select_columns(&mut self, columns: &[SelectItem]) {
        for column in columns {
            self.visit_select_item(column);
        }
    }
    fn visit_select_into_table(&mut self, arg: &Option<IntoArg>) {
        if let Some(into_arg) = arg {
            walk_expression(self, &into_arg.table);

            if let Some(file_group) = &into_arg.file_group {
                walk_expression(self, file_group);
            }
        }
    }
    fn visit_select_table(&mut self, arg: &Option<TableArg>) {
        if let Some(table_arg) = arg {
            self.visit_table_source(&table_arg.table);
            for join in &table_arg.joins {
                self.visit_table_join(join);
            }
        }
    }
    fn visit_select_where_clause(&mut self, where_clause: &Option<Expression>) {
        if let Some(where_clause) = where_clause {
            self.visit_expression(where_clause);
        }
    }
    fn visit_select_group_by(&mut self, group_by: &[Expression]) {
        for expression in group_by {
            self.visit_expression(expression);
        }
    }
    fn visit_select_having(&mut self, having: &Option<Expression>) {
        if let Some(having) = having {
            self.visit_expression(having);
        }
    }
    fn visit_select_order_by(&mut self, order_by_args: &[OrderByArg]) {
        for order_by in order_by_args {
            self.visit_expression(&order_by.column);
        }
    }
    fn visit_select_offset(&mut self, arg: &Option<OffsetArg>) {
        if let Some(offset_arg) = arg {
            self.visit_expression(&offset_arg.value);
            self.visit_select_offset_fetch_row_or_rows(offset_arg.row);
        }
    }
    fn visit_select_fetch(&mut self, arg: &Option<FetchArg>) {
        if let Some(fetch_arg) = arg {
            self.visit_expression(&fetch_arg.value);
            self.visit_select_offset_fetch_row_or_rows(fetch_arg.row);
            self.visit_select_fetch_next_or_first(fetch_arg.first);
        }
    }
    fn visit_select_wildcard(&mut self) {}
    fn visit_select_item(&mut self, select_item: &SelectItem) {
        match select_item {
            SelectItem::Wildcard => self.visit_select_wildcard(),
            SelectItem::Unnamed(expression) => walk_expression(self, expression),
            SelectItem::WithAlias { expression, .. } => self.visit_expression(expression),
            SelectItem::WildcardWithAlias { expression, .. } => self.visit_expression(expression),
        }
    }
    fn visit_table_source(&mut self, table: &TableSource) {
        match table {
            TableSource::Table { name, .. } => walk_expression(self, name),
            TableSource::TableValuedFunction { function, .. } => walk_expression(self, function),
            _ => unimplemented!(),
        }
    }
    fn visit_table_join(&mut self, join: &Join) {
        self.visit_table_join_type(&join.join_type);
        self.visit_table_source(&join.table);
        if let Some(condition) = &join.condition {
            walk_expression(self, condition);
        }
    }
    fn visit_table_join_type(&mut self, _join_type: &JoinType) {}
    fn visit_select_offset_fetch_row_or_rows(&mut self, _row_or_rows: RowOrRows) {}
    fn visit_select_fetch_next_or_first(&mut self, _next_or_first: NextOrFirst) {}
    fn visit_is_true_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_is_not_true_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_is_null_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_is_not_null_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_in_list_expression(&mut self, expression: &Expression) {
        if let Expression::InList {
            expression, list, ..
        } = expression
        {
            self.visit_expression(expression);
            for expression in list {
                self.visit_expression(expression);
            }
        }
    }
    fn visit_between_expression(&mut self, expression: &Expression) {
        if let Expression::Between { low, high, .. } = expression {
            self.visit_expression(low);
            self.visit_expression(high);
        }
    }
    fn visit_any_expression(&mut self, expression: &Expression) {
        if let Expression::Any { left, right, .. } = expression {
            self.visit_expression(left);
            self.visit_expression(right);
        }
    }
    fn visit_all_expression(&mut self, expression: &Expression) {
        if let Expression::All { left, right, .. } = expression {
            self.visit_expression(left);
            self.visit_expression(right);
        }
    }
    fn visit_some_expression(&mut self, expression: &Expression) {
        if let Expression::Some { left, right, .. } = expression {
            self.visit_expression(left);
            self.visit_expression(right);
        }
    }
    fn visit_exists_expression(&mut self, expression: &Expression) {
        self.visit_expression(expression);
    }
    fn visit_expression_list_expression(&mut self, expression: &[Expression]) {
        for expression in expression {
            self.visit_expression(expression);
        }
    }
    fn visit_function_expression(&mut self, expression: &Expression) {
        if let Expression::Function { name, args, over } = expression {
            walk_expression(self, name);
            walk_expression(self, args);

            if let Some(over) = over {
                self.visit_select_window_over_clause(over);
            }
        }
    }
    fn visit_select_window_over_clause(&mut self, over_clause: &OverClause) {
        for partition_by in &over_clause.partition_by {
            self.visit_expression(partition_by);
        }
        self.visit_select_order_by(&over_clause.order_by);
        if let Some(window_frame) = &over_clause.window_frame {
            self.visit_window_frame(window_frame);
        }
    }
    fn visit_window_frame(&mut self, window_frame: &WindowFrame) {
        self.visit_window_frame_rows_or_range(window_frame.rows_or_range);
        self.visit_window_frame_bound(&window_frame.start);
        if let Some(end) = &window_frame.end {
            self.visit_window_frame_bound(end);
        }
    }
    fn visit_window_frame_rows_or_range(&mut self, _rows_or_range: RowsOrRange) {}
    fn visit_window_frame_bound(&mut self, bound: &WindowFrameBound) {
        match bound {
            WindowFrameBound::Preceding(expression) => self.visit_expression(expression),
            WindowFrameBound::Following(expression) => self.visit_expression(expression),
            _ => (),
        }
    }
    fn visit_compound_literal(&mut self, tokens: &[Token]) {
        for token in tokens {
            self.visit_token(token);
        }
    }
    fn visit_cte_statement(&mut self, statement: &Statement) {
        if let Statement::CTE { ctes, statement } = statement {
            for cte in ctes {
                self.visit_cte(cte);
            }

            self.visit_common_table_expression_statement(statement);
        }
    }
    fn visit_common_table_expression_statement(&mut self, cte_statement: &CommonTableExpressionStatement) {
        match cte_statement {
            CommonTableExpressionStatement::Select(select) => self.visit_select_query(select),
            CommonTableExpressionStatement::Insert(_) => todo!(),
            CommonTableExpressionStatement::Update(_) => todo!(),
            CommonTableExpressionStatement::Delete(_) => todo!(),
        }
    }
    fn visit_cte(&mut self, cte: &CommonTableExpression) {
        self.visit_expression(&cte.name);

        for column in &cte.columns {
            self.visit_expression(column);
        }
        self.visit_select_query(&cte.query);
    }
    fn visit_subquery(&mut self, query: &SelectStatement) {
        self.visit_select_query(&query);
    }
    fn visit_cast(&mut self, expression: &Expression);
    fn visit_data_type(&mut self, data_type: &DataType);
    fn visit_exec_or_execute(&mut self, keyword: &ExecOrExecute);
    fn visit_declare_statement(&mut self, statement: &[LocalVariable]);
    fn visit_set_local_variable_statement(&mut self, statement: &Statement);
    fn visit_execute_statement(&mut self, statement: &Statement);
    fn visit_exec_parameter(&mut self, parameter: &ProcedureParameter);
    fn visit_insert_query(&mut self, query: &InsertStatement);
    fn visit_update_query(&mut self, query: &UpdateStatement);
    fn visit_delete_query(&mut self, query: &DeleteStatement);
    fn visit_update_set(&mut self, update_set: &UpdateSet);
    fn visit_column_list(&mut self, column_list: &[Expression]);
}

pub fn walk_query<V: Visitor + ?Sized>(visitor: &mut V, query: &Query) {
    visitor.visit_query(query);
}

pub fn walk_expression<V: Visitor + ?Sized>(visitor: &mut V, e: &Expression) {
    match e {
        Expression::Literal(identifier) => visitor.visit_token(identifier),
        Expression::Binary { .. } => visitor.visit_binary_expression(e),
        Expression::Unary { .. } => visitor.visit_unary_expression(e),
        Expression::CompoundLiteral(tokens) => visitor.visit_compound_literal(tokens),
        Expression::Grouping(e) => visitor.visit_unary_expression(e),
        Expression::Subquery(s) => visitor.visit_subquery(s),
        Expression::IsTrue(e) => visitor.visit_is_true_expression(e),
        Expression::IsNotTrue(e) => visitor.visit_is_not_true_expression(e),
        Expression::IsNull(e) => visitor.visit_is_null_expression(e),
        Expression::IsNotNull(e) => visitor.visit_is_not_null_expression(e),
        Expression::InList { .. } => visitor.visit_in_list_expression(e),
        Expression::Between { .. } => visitor.visit_between_expression(e),
        Expression::Any { .. } => visitor.visit_any_expression(e),
        Expression::All { .. } => visitor.visit_all_expression(e),
        Expression::Some { .. } => visitor.visit_some_expression(e),
        Expression::Exists(e) => visitor.visit_exists_expression(e),
        Expression::ExpressionList(e) => visitor.visit_expression_list_expression(e),
        Expression::Function { .. } => visitor.visit_function_expression(e),
        Expression::Cast { .. } => visitor.visit_cast(e),
    }
}

pub fn walk_binary_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &Expression) {
    if let Expression::Binary {
        left,
        right,
        operator,
    } = expression
    {
        walk_expression(visitor, left);
        visitor.visit_token(operator);
        walk_expression(visitor, right);
    }
}

pub fn walk_unary_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &Expression) {
    if let Expression::Unary { right, operator } = expression {
        visitor.visit_token(operator);
        walk_expression(visitor, right);
    }
}

pub fn walk_group_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &Expression) {
    walk_expression(visitor, expression)
}

pub fn walk_statement<V: Visitor + ?Sized>(visitor: &mut V, statement: &Statement) {
    match statement {
        Statement::Select(select_query) => visitor.visit_select_query(select_query),
        Statement::CTE { .. } => visitor.visit_cte_statement(statement),
        Statement::Declare (vars) => visitor.visit_declare_statement(vars),
        Statement::SetLocalVariable {..} => visitor.visit_set_local_variable_statement(statement),
        Statement::Execute {..} => visitor.visit_execute_statement(statement),
        Statement::Insert(insert) => visitor.visit_insert_query(insert),
        Statement::Update(update) => visitor.visit_update_query(update),
        Statement::Delete(delete) => visitor.visit_delete_query(delete),
    }
}

pub fn walk_select_query<V: Visitor + ?Sized>(visitor: &mut V, statement: &SelectStatement) {
    visitor.visit_select_query(statement);
}

pub fn walk_insert_query<V: Visitor + ?Sized>(visitor: &mut V, statement: &InsertStatement) {
    visitor.visit_insert_query(statement);
}

pub fn walk_delete_query<V: Visitor + ?Sized>(visitor: &mut V, statement: &DeleteStatement) {
    visitor.visit_delete_query(statement);
}

pub fn walk_update_query<V: Visitor + ?Sized>(visitor: &mut V, statement: &UpdateStatement) {
    visitor.visit_update_query(statement);
}

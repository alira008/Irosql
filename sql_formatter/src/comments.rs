use std::collections::HashMap;

use sql_lexer::Span;
use sql_parser::{
    ast::{Comment, Query},
    visitor::Visitor,
};

pub struct CommentMapper<'a> {
    pub input: &'a str,
    pub comments: Vec<Comment>,
    pub comment_map_before_line: Vec<(Span, Comment)>,
    pub comment_map_same_line: Vec<(Span, Comment)>,
    pub comment_distances_before_line: HashMap<Comment, Vec<(Span, i64)>>,
    pub comment_distances_same_line: HashMap<Comment, Vec<(Span, i64)>>,
}

impl<'a> CommentMapper<'a> {
    pub fn new(input: &'a str, comments: &'a [Comment]) -> Self {
        Self {
            input,
            comments: comments.to_owned(),
            comment_map_before_line: vec![],
            comment_map_same_line: vec![],
            comment_distances_before_line: HashMap::new(),
            comment_distances_same_line: HashMap::new(),
        }
    }

    pub fn map(&mut self, query: &Query) {
        self.visit_query(query);

        for comment in self.comments.iter() {
            if let Some(distances) = &self.comment_distances_same_line.get(comment) {
                if let Some(min_distance) = distances
                    .iter()
                    .min_by(|(_, dx), (_, dy)| dx.cmp(dy))
                {
                    let _ = self
                        .comment_map_same_line
                        .push((min_distance.0, comment.clone()));
                }
            }

            if let Some(distances) = &self.comment_distances_before_line.get(comment) {
                if let Some(min_distance) = distances
                    .iter()
                    .min_by(|(_, dx), (_, dy)| dx.cmp(dy))
                {
                    let _ = self
                        .comment_map_before_line
                        .push((min_distance.0, comment.clone()));
                }
            }
        }
    }

    fn position_distance_same_line(&self, comment_start: u32, node_start: u32) -> Option<i64> {
        let comment_pos = span_pos_to_line_col(self.input, comment_start);
        let node_pos = span_pos_to_line_col(self.input, node_start);

        let line_difference = comment_pos.0 as i64 - node_pos.0 as i64;
        if line_difference > 0 {
            return None;
        }

        if line_difference == 0 {
            return Some((comment_pos.1 as i64 - node_pos.1 as i64).abs());
        } else {
            return None;
        }
    }

    fn position_distance_before(&self, comment_start: u32, node_start: u32) -> Option<i64> {
        let comment_pos = span_pos_to_line_col(self.input, comment_start);
        let node_pos = span_pos_to_line_col(self.input, node_start);

        let line_difference = comment_pos.0 as i64 - node_pos.0 as i64;
        if line_difference > 0 {
            return None;
        }

        if line_difference == 0 {
            return None;
        } else {
            return Some(line_difference.abs());
        }
    }
}

fn span_pos_to_line_col(text: &str, pos: u32) -> (u32, u32) {
    let mut line_number: u32 = 1;
    let mut column_number: u32 = 1;

    for (i, c) in text.char_indices() {
        if i == pos as usize {
            break;
        }

        if c == '\n' {
            line_number += 1;
            column_number = 1;
        } else {
            column_number += 1;
        }
    }

    (line_number, column_number)
}

impl<'a> Visitor for CommentMapper<'a> {
    type Result = ();

    fn visit_span(&mut self, span: &Span) -> Self::Result {
        for comment in &self.comments {
            let distance_before_line =
                self.position_distance_before(comment.span.start, span.start);
            let distance_same_line =
                self.position_distance_same_line(comment.span.start, span.start);

            if let (Some(arr), Some(distance_same_line)) = (
                self.comment_distances_same_line.get_mut(comment),
                distance_same_line,
            ) {
                arr.push((*span, distance_same_line));
            } else if let Some(distance_same_line) = distance_same_line {
                let _ = self
                    .comment_distances_same_line
                    .insert(comment.clone(), vec![(*span, distance_same_line)]);
            }

            if self.comment_distances_same_line.contains_key(comment) {
                continue;
            }

            if let (Some(arr), Some(distance_before_line)) = (
                self.comment_distances_before_line.get_mut(comment),
                distance_before_line,
            ) {
                arr.push((*span, distance_before_line));
            } else if let Some(distance_before_line) = distance_before_line {
                let _ = self
                    .comment_distances_before_line
                    .insert(comment.clone(), vec![(*span, distance_before_line)]);
            }
        }
    }
}

macro_rules! define_keyword {
    ($ident:ident = $string_keyword:expr) => {
        pub const $ident: &'static str = $string_keyword;
    };
    // This is macro allows you to call define_keyword!(select)
    // then it will call define_keyword!(SELECT = "SELECT")
    ($ident:ident) => {
        define_keyword!($ident = stringify!($ident));
    };
}

macro_rules! define_keywords {
        ($($ident:ident $(= $string_keyword:expr)?),*) => {
            #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
            #[allow(non_camel_case_types)]
            pub enum Keyword {
                $($ident),*
            }

            // this holds all the enum Keywords in an array
            pub const ALL_KEYWORDS_INDEX: &[Keyword] = &[$(Keyword::$ident),*];

            // this holds all the string versions of the Keywords in an array
            $(define_keyword!($ident $(= $string_keyword)?);)*
            pub const ALL_KEYWORDS: &[&str] = &[$($ident),*];
        };
    }

// this has this be sorted alphabetically manually because we want to use binary search on
// these keywords
define_keywords!(
    ABS,
    ALL,
    ALTER,
    AND,
    ANY,
    AS,
    ASC,
    AUTOINCREMENT,
    AVG,
    BEGIN,
    BETWEEN,
    BIGINT,
    BIT,
    BY,
    CASCADE,
    CASE,
    CAST,
    CEIL,
    CHAR,
    COLUMN,
    COLUMNS,
    COMMIT,
    COMMITED,
    CONSTRAINT,
    CREATE,
    COUNT,
    DATE,
    DATETIME,
    DAY,
    DAYOFWEEK,
    DAYOFYEAR,
    DECIMAL,
    DECLARE,
    DFEAULT,
    DELETE,
    DENSE_RANK,
    DESC,
    DESCRIBE,
    DISTINCT,
    DO,
    DROP,
    ELSE,
    END,
    ENGINE,
    EXEC,
    EXECUTE,
    EXISTS,
    FALSE,
    FETCH,
    FIRST,
    FIRST_VALUE,
    FLOAT,
    FLOOR,
    FOREIGN,
    FROM,
    FUNCTION,
    GROUP,
    HAVING,
    HOUR,
    HOURS,
    IDENTITY,
    IF,
    IN,
    INCREMENT,
    INDEX,
    INNER,
    INSERT,
    INTEGER,
    INTERSECT,
    INTO,
    IS,
    JOIN,
    KEY,
    LAG,
    LAST,
    LAST_VALUE,
    LEFT,
    LIKE,
    LIMIT,
    MAX,
    MICROSECOND,
    MICROSECONDS,
    MILLISECOND,
    MILLISECONDS,
    MIN,
    MINUTE,
    MONTH,
    NANOSECOND,
    NANOSECONDS,
    NCHAR,
    NEXT,
    NOT,
    NULL,
    NULLIF,
    NUMERIC,
    NVARCHAR,
    OFFSET,
    ON,
    ONLY,
    OR,
    ORDER,
    OUTER,
    OVER,
    PARTITION,
    PASSWORD,
    PERCENT,
    PROCEDURE,
    RANK,
    RETURN,
    RETURNS,
    REVOKE,
    RIGHT,
    ROLE,
    ROLLBACK,
    ROW,
    ROWID,
    ROWS,
    ROW_NUMBER,
    SECOND,
    SELECT,
    SET,
    SMALLINT,
    SNAPSHOT,
    SOME,
    STAGE,
    START,
    STATISTICS,
    TABLE,
    TEMP,
    THEN,
    TIES,
    TIME,
    TIMESTAMP,
    TOP,
    TRANSACTION,
    TRIGGER,
    TRUE,
    TRUNCATE,
    UNBOUNDED,
    UNCOMMITTED,
    UNION,
    UNIQUE,
    UNLOCK,
    UPDATE,
    UPPER,
    USE,
    USER,
    UUID,
    VALUE,
    VALUES,
    VARBINARY,
    VARCHAR,
    WEEK,
    WHEN,
    WHERE,
    WINDOW,
    WITH,
    YEAR
);

pub fn lookup_keyword(keyword: &str) -> Option<Keyword> {
    let normalized_keyword = keyword.to_uppercase();
    match ALL_KEYWORDS.binary_search(&normalized_keyword.as_str()) {
        Ok(index) => Some(ALL_KEYWORDS_INDEX[index].clone()),
        Err(_) => None,
    }
}

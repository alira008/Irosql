use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DataType {
    Int,
    BigInt,
    TinyInt,
    SmallInt,
    Bit,
    Float(Option<u32>),
    Real,
    Date,
    Datetime,
    Time,
    Decimal(Option<NumericSize>),
    Numeric(Option<NumericSize>),
    Varchar(Option<u32>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NumericSize {
    pub precision: u32,
    pub scale: Option<u32>,
}

impl fmt::Display for NumericSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.precision)?;
        if let Some(scale) = self.scale {
            write!(f, ", {}", scale)?;
        }
        write!(f, "")
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::Int => write!(f, "INT"),
            DataType::BigInt => write!(f, "BIGINT"),
            DataType::TinyInt => write!(f, "TINYINT"),
            DataType::SmallInt => write!(f, "SMALLINT"),
            DataType::Bit => write!(f, "BIT"),
            DataType::Float(s) => {
                write!(f, "FLOAT")?;
                if let Some(p) = s {
                    write!(f, "({})", p)?;
                }
                Ok(())
            }
            DataType::Real => write!(f, "REAL"),
            DataType::Date => write!(f, "DATE"),
            DataType::Datetime => write!(f, "DATETIME"),
            DataType::Time => write!(f, "TIME"),
            DataType::Decimal(s) => {
                write!(f, "DECIMAL")?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
            DataType::Numeric(s) => {
                write!(f, "NUMERIC")?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
            DataType::Varchar(s) => {
                write!(f, "VARCHAR")?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
        }
    }
}

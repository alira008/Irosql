use std::fmt;

use super::Keyword;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DataType {
    Int(Keyword),
    BigInt(Keyword),
    TinyInt(Keyword),
    SmallInt(Keyword),
    Bit(Keyword),
    Float(Keyword, Option<u32>),
    Real(Keyword),
    Date(Keyword),
    Datetime(Keyword),
    Time(Keyword),
    Decimal(Keyword, Option<NumericSize>),
    Numeric(Keyword, Option<NumericSize>),
    Varchar(Keyword, Option<u32>),
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
            DataType::Int(k) => write!(f, "{}", k),
            DataType::BigInt(k) => write!(f, "{}", k),
            DataType::TinyInt(k) => write!(f, "{}", k),
            DataType::SmallInt(k) => write!(f, "{}", k),
            DataType::Bit(k) => write!(f, "{}", k),
            DataType::Float(k, s) => {
                write!(f, "{}", k)?;
                if let Some(p) = s {
                    write!(f, "({})", p)?;
                }
                Ok(())
            }
            DataType::Real(k) => write!(f, "{}", k),
            DataType::Date(k) => write!(f, "{}", k),
            DataType::Datetime(k) => write!(f, "{}", k),
            DataType::Time(k) => write!(f, "{}", k),
            DataType::Decimal(k, s) => {
                write!(f, "{}", k)?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
            DataType::Numeric(k, s) => {
                write!(f, "{}", k)?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
            DataType::Varchar(k, s) => {
                write!(f, "{}", k)?;
                if let Some(s) = s {
                    write!(f, "({})", s)?;
                }
                Ok(())
            }
        }
    }
}

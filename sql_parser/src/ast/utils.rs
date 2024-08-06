use core::fmt;

pub fn display_list_comma_separated<T>(list: &[T], f: &mut fmt::Formatter) -> fmt::Result
where
    T: fmt::Display,
{
    display_list_delimiter_separated(list, ", ", f)
}

pub fn display_list_delimiter_separated<T>(
    list: &[T],
    delimeter: &str,
    f: &mut fmt::Formatter,
) -> fmt::Result
where
    T: fmt::Display,
{
    for (i, item) in list.iter().enumerate() {
        write!(f, "{}", item)?;

        if i < list.len() - 1 {
            write!(f, "{delimeter}")?;
        }
    }
    Ok(())
}

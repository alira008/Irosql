use formatter::formatter::Formatter;
use formatter::settings::{FormatterSettings, KeywordCase};


#[test]
fn basic_local_variables() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Lower,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"exec usp_potato @param1 = 'heloo', 'yes' execute usp_dos 'no', @pa = @helovar
    Declare @hello varchar(255) = 'yessir', @testVar float = 532453.3515, @TestDos Date;
    SET @TestDos = cast(getdate() as date);";
    let expected = r"exec usp_potato @param1 = 'heloo', 'yes'

execute usp_dos 'no', @pa = @helovar

declare @hello varchar(255) = 'yessir'
    ,@testVar float = 532453.3515
    ,@TestDos date;

set @TestDos = cast(getdate() as date);";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}

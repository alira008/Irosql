use config::{Environment, File};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub(crate) host: String,
    pub(crate) port: u16,
    pub(crate) user: String,
    pub(crate) pass: String,
    pub(crate) trust_cert: bool,
}

pub fn get() -> Option<Config> {
    let mut config_file = dirs::config_dir().expect("Could not load user config dir.");
    config_file.push("ironsql/settings.json");

    let s = config::Config::builder()
        .add_source(File::with_name(config_file.to_str()?))
        .add_source(Environment::with_prefix("irosql"))
        .build()
        .ok()?;

    s.try_deserialize().ok()
}

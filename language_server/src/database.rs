use crate::config;

pub struct Database {
    stream: tokio::net::TcpStream,
}

pub async fn connect(user_config: &config::Config) -> Result<Database, String> {
    let mut config = tiberius::Config::new();
    config.host(&user_config.host);
    config.port(user_config.port);

    config.authentication(tiberius::AuthMethod::sql_server(
        &user_config.user,
        &user_config.pass,
    ));

    if user_config.trust_cert {
        config.trust_cert();
    }
    let stream = match tokio::net::TcpStream::connect(config.get_addr()).await {
        Ok(s) => s,
        Err(e) => return Err(e.to_string()),
    };

    Ok(Database { stream })
}

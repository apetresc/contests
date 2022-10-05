/// various log levels
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LogLevel {
    Info,
    Warning,
    Error,
    Debug
}

pub fn log(level: LogLevel, message: &str) -> String {
    let level = format!("{:?}", level);
    format!("[{}]: {}", level.to_uppercase(), message)
}
pub fn info(message: &str) -> String {
    log(LogLevel::Info, message)
}
pub fn warn(message: &str) -> String {
    log(LogLevel::Warning, message)
}
pub fn error(message: &str) -> String {
    log(LogLevel::Error, message)
}
pub fn debug(message: &str) -> String {
    log(LogLevel::Debug, message)
}

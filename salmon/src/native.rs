use std::time::{SystemTime, UNIX_EPOCH};

use miette::Result;

use crate::value::Value;

pub fn clock(_arg_count: usize, _args: &[Value]) -> Result<Value> {
    let unix_timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    Ok(Value::Number(unix_timestamp))
}

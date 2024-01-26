use std::{
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use miette::{miette, Result};

use crate::{object::Object, value::Value};

pub fn clock(_arg_count: usize, _args: &[Value]) -> Result<Value> {
    let unix_timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    Ok(Value::Number(unix_timestamp))
}

pub fn debug_closure(arg_count: usize, args: &[Value]) -> Result<Value> {
    if arg_count != 1 {
        return Err(miette!(
            "wrong number of arguments to debug_closure: expected 1, got {}",
            arg_count
        ));
    }

    match &args[0] {
        Value::Object(Object::Closure(closure_data)) => {
            let mut debug_string = format!(
                "Closure of: {}",
                Object::Function(Arc::clone(&closure_data.function)).show()
            );
            if closure_data.upvalue_count() > 0 {
                debug_string.push_str("\nOver:");
                for upvalue in closure_data.upvalues.iter() {
                    debug_string.push_str(&format!("\n    {}", upvalue.show_reference()));
                }
            }

            Ok(Value::Object(Object::string(&debug_string)))
        }
        v => Err(miette!(
            "invalid argument to debug_closure: expected a closure, got {:?}",
            v
        )),
    }
}

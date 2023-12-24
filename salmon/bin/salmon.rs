const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> () {
    println!("Welcome to Salmon v{}.", VERSION);
}

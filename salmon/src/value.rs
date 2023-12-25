pub struct Value(pub f64);

impl Value {
    pub fn show(&self) -> String {
        format!("{}", self.0)
    }
}

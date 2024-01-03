use std::sync::Arc;

use crate::{object::Object, value::Value};

const MAX_LOAD: f32 = 0.75;

pub struct Table {
    count: usize,
    entries: Vec<Entry>,
}

impl Table {
    pub fn new() -> Table {
        Table {
            count: 0,
            entries: Vec::with_capacity(0),
        }
    }

    pub fn get(&self, key: &Object) -> Option<Value> {
        if self.count == 0 {
            None
        } else {
            match self.find_entry_for(key) {
                Entry::Full { value, .. } => Some(value.clone()),
                _ => None,
            }
        }
    }

    pub fn set(&mut self, key: Arc<Object>, value: Value) -> bool {
        if (self.count + 1) as f32 > self.entries.len() as f32 * MAX_LOAD {
            self.adjust_capacity();
        }

        let entry = self.find_entry_for_mut(key.as_ref());
        match entry {
            Entry::Empty => {
                // If we insert into an empty cell, increment the count
                *entry = Entry::Full { key, value };
                self.count += 1;
                true
            }
            Entry::Tombstone => {
                // If we insert into a tombstone, leave the count alone
                *entry = Entry::Full { key, value };
                true
            }
            Entry::Full {
                key: old_key,
                value: old_value,
            } => {
                *old_key = key;
                *old_value = value;
                false
            }
        }
    }

    pub fn add_all(&mut self, other: &Table) -> () {
        for other_entry in other.entries.iter() {
            match other_entry {
                Entry::Full { key, value } => {
                    let _ = self.set(key.clone(), value.clone());
                }
                _ => (),
            }
        }
    }

    pub fn delete(&mut self, key: &Object) -> bool {
        if self.count == 0 {
            false
        } else {
            let entry = self.find_entry_for_mut(key);
            match entry {
                Entry::Empty => false,
                Entry::Tombstone => false,
                Entry::Full { .. } => {
                    *entry = Entry::Tombstone;
                    true
                }
            }
        }
    }

    fn find_entry_for(&self, key: &Object) -> &Entry {
        let entry_idx = self.find_index_for(key);
        &self.entries[entry_idx]
    }

    fn find_entry_for_mut(&mut self, key: &Object) -> &mut Entry {
        let entry_idx = self.find_index_for(key);
        &mut self.entries[entry_idx]
    }

    fn find_index_for(&self, key: &Object) -> usize {
        let capacity = self.entries.len();
        let mut index = key.hash as usize % capacity;
        let mut tombstone_idx = None;

        loop {
            let entry = &self.entries[index];
            match entry {
                Entry::Empty => return tombstone_idx.unwrap_or(index),
                Entry::Tombstone => {
                    let _ = tombstone_idx.get_or_insert(index);
                }
                Entry::Full {
                    key: current_key, ..
                } if key == current_key.as_ref() => return index,
                _ => (),
            }

            index = (index + 1) % capacity;
        }
    }

    fn adjust_capacity(&mut self) -> () {
        let old_capacity = self.entries.len();
        let new_capacity = if old_capacity < 8 {
            8
        } else {
            old_capacity * 2
        };
        let new_entries = std::iter::repeat(Entry::Empty)
            .take(new_capacity)
            .collect::<Vec<Entry>>();
        let old_entries = std::mem::replace(&mut self.entries, new_entries);

        let mut new_count = 0;
        for old_entry in old_entries {
            match &old_entry {
                // Copy all the full entries to new locations in the expanded table
                Entry::Full { key, .. } => {
                    let new_location = self.find_entry_for_mut(key.as_ref());
                    *new_location = old_entry;
                    new_count += 1;
                }
                _ => (),
            }
        }
        self.count = new_count;
    }
}

#[derive(Clone)]
enum Entry {
    Empty,
    Tombstone,
    Full { key: Arc<Object>, value: Value },
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, sync::Arc};

    use crate::{object::Object, value::Value};

    use super::Table;

    #[test]
    fn it_overwrites() {
        let mut table = Table::new();
        assert_eq!(
            true,
            table.set(Arc::new(Object::string("abc")), Value::Number(1.0))
        );
        assert_eq!(
            false,
            table.set(Arc::new(Object::string("abc")), Value::Number(2.0))
        );

        let current_value = table.get(&Object::string("abc"));
        assert_eq!(Some(Value::Number(2.0)), current_value);
    }

    #[test]
    fn it_deletes() {
        let mut table = Table::new();

        // Table::delete() should return false if the key wasn't in the table
        assert_eq!(false, table.delete(&Object::string("abc")));

        // Table::delete() should return true if the key was in the table
        table.set(Arc::new(Object::string("abc")), Value::Number(1.0));
        assert_eq!(true, table.delete(&Object::string("abc")));

        // After a delete, the key should no longer be in the table
        let current_value = table.get(&Object::string("abc"));
        assert_eq!(None, current_value);
        assert_eq!(false, table.delete(&Object::string("abc")));
    }

    #[test]
    fn it_drops_old_keys_on_overwrite() {
        let mut table = Table::new();
        let original_abc = Arc::new(Object::string("abc"));
        table.set(original_abc.clone(), Value::Number(1.0));

        // The table should hold a strong reference to the key
        assert_eq!(2, Arc::strong_count(&original_abc));

        table.set(Arc::new(Object::string("abc")), Value::Number(2.0));

        // The table should no longer have a reference to the key
        // It should hold a new reference to a different object that tests equal to "abc"
        assert_eq!(1, Arc::strong_count(&original_abc));
    }

    #[test]
    fn it_drops_old_keys_on_delete() {
        let mut table = Table::new();
        let original_abc = Arc::new(Object::string("abc"));
        table.set(original_abc.clone(), Value::Number(1.0));

        // The table should hold a strong reference to the key
        assert_eq!(2, Arc::strong_count(&original_abc));

        assert_eq!(true, table.delete(&Object::string("abc")));

        assert_eq!(1, Arc::strong_count(&original_abc));
    }

    #[test]
    fn it_handles_long_sequences_of_operations() {
        // FizzBuzz time! With n = 1..=25:
        //   insert [ "test${n}" => n ] into the table
        //   then if n is a multiple of 3:
        //     overwrite [ "test${n}" ] with 2*n
        //   then if n is a mutliple of 5:
        //     delete [ "test${n - 1}" ]
        // That should be enough operations to cause the table to resize more than once
        // Mirror the operations on a native HashMap and use that to validate the results
        let mut table = Table::new();
        let mut validator = HashMap::new();

        for n in 1..=25 {
            let raw_key = format!("test{}", n);
            let key = Arc::new(Object::string(&raw_key));
            let value = Value::Number(n as f64);
            table.set(key.clone(), value.clone());
            validator.insert(raw_key.clone(), Some(value));

            if n % 3 == 0 {
                let doubled = Value::Number((n * 2) as f64);
                table.set(key.clone(), doubled.clone());
                validator.insert(raw_key.clone(), Some(doubled));
            }

            if n % 5 == 0 {
                let raw_prev_key = format!("test{}", n - 1);
                let prev_key = Arc::new(Object::string(&raw_prev_key));
                table.delete(prev_key.as_ref());
                validator.insert(raw_prev_key.clone(), None);
            }
        }

        for (raw_key, expected) in validator {
            let key = Object::string(&raw_key);
            let actual = table.get(&key);
            assert_eq!(
                expected, actual,
                "expected validator[{raw_key}] == table[{raw_key}]"
            );
        }
    }
}

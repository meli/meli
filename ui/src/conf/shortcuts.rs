use types::Key;
//use std::any::TypeId;
use fnv::FnvHashMap;

#[macro_export]
macro_rules! key_values {
    (derive ($($derives:ident),*) : pub struct $name:ident { $($fname:ident : Key),* }) => {
        #[derive($($derives),*)]
        #[serde(default)]
        pub struct $name {
            $($fname : Key),*
        }

        impl $name {
            pub fn key_values(&self) -> FnvHashMap<&'static str, &Key> {
                let mut map: FnvHashMap<&'static str, &Key> = Default::default();
                $(map.insert(stringify!($fname),&(self.$fname));)* 
                  map
            }
        }
    }
}

key_values!{derive (Debug, Clone, Deserialize) :
pub struct CompactListingShortcuts {
    open_thread: Key,
    exit_thread: Key,
    prev_page: Key,
    next_page: Key,
    prev_folder: Key,
    next_folder: Key,
    prev_account: Key,
    next_account: Key,
    new_mail: Key
}
}


impl Default for CompactListingShortcuts {
    fn default() -> Self {
        CompactListingShortcuts {
            open_thread: Key::Char('\n'),
            exit_thread: Key::PageUp,
            prev_page: Key::PageDown,
            next_page: Key::Char('i'),
            prev_folder: Key::Char('J'),
            next_folder: Key::Char('K'),
            prev_account:Key::Char('h'),
            next_account:Key::Char('l'),
            new_mail: Key::Char('m'),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_macro() {
        panic!()


    }
}

use std::str::FromStr;

use crate::{ClasspadChar, ClasspadString};

impl ClasspadString {
    /// Attach another [`ClasspadString`] to the end of this string.
    ///
    /// ## Important!: moves all values out of `other` and leaves it empty.
    pub fn attach_classpad_str(&mut self, other: &mut ClasspadString) -> &mut Self {
        self.0.append(&mut other.0);
        self
    }
    /// Convert a &[`str`] to a [`ClasspadString`] then attach it to the end.
    pub fn attach_str(&mut self, other: &str) -> &mut Self {
        self.0
            .append(&mut ClasspadString::from_str(other).unwrap().0);
        self
    }
    /// Attach a char
    pub fn attach_classpad_char(&mut self, ch: usize) -> &mut Self {
        if let Some(ch) = ClasspadChar::from_classpad_char(ch) {
            self.0.push(ch);
        }
        self
    }

    // Attaching other signs

    /// Attach an integral sign
    pub fn attach_integral(&mut self) -> &mut Self {
        self.0.push(ClasspadChar::from_unicode_char('∫').unwrap());
        self
    }
}

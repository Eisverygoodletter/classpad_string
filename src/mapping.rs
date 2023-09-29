//! [`struct@UNICODE_MAP`] for bidirectional mapping between classpad character codes and unicode characters.
use lazy_static::lazy_static;
use bidir_map::{BidirMap, bidir_map};

lazy_static! {
    pub static ref UNICODE_MAP : BidirMap<usize, char> = {
        bidir_map!(
            259 => 'Â',
            260 => 'Ã',
            261 => 'Ä',
            // todo: huge number of character codes
            851 => '∫',
        )
    };
}
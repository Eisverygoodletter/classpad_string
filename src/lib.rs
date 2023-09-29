//! Read and write [`ClasspadString`]s that can be stored in XCP files and loaded onto the CASIO Classpad fx-400.
//!
//! This library provides the [`ClasspadChar`] (a single classpad character), [`ClasspadString`], and [`XCPFile`].
//! Here is how you can write "this is some text" into a XCP file.
//! ```rust
//! use classpad_string::*;
//! use std::str::FromStr;
//! let text = "this is some text";
//! let classpad_s = ClasspadString::from_str(text).unwrap();
//! let xcp = XCPFile {
//!     file_type: XCPFileType::Text,
//!     data: classpad_s,
//!     folder_name: "main".to_string(),
//!     var_name: "mytext".to_string()
//! };
//! let mut outputxcp = std::fs::File::create("mytext.xcp").unwrap();
//! xcp.write_to_file(&mut outputxcp);
//! ```
use std::{
    fs::File,
    io::{Read, Write},
    num::Wrapping,
};

pub mod mapping;
use mapping::*;
#[doc(hidden)]
pub mod attach;
pub use attach::*;

/**
 * Represents a character from the classpad's character code table.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClasspadChar {
    /// Characters that follow the ASCII table.
    SingleByte(u8),
    /// Characters starting from 257 that are 2 bytes.
    TwoByte(u8, u8),
}
impl ClasspadChar {
    /// Convert from the classpad character code table to a [`ClasspadChar`].
    pub fn from_classpad_char(code: usize) -> Option<Self> {
        if code < 32 {
            println!("invalid character code {}", code);
            return None;
        }
        if (32..=126).contains(&code) {
            Some(Self::SingleByte(code as u8))
        } else if (127..=256).contains(&code) {
            println!("invalid character code {}", code);
            None
        } else if (257..=486).contains(&code) {
            Some(Self::TwoByte(0xEC, (code - 256) as u8))
        } else if (513..=766).contains(&code) {
            Some(Self::TwoByte(0xED, (code - 512) as u8))
        } else if (769..=943).contains(&code) {
            Some(Self::TwoByte(0xEE, (code - 768) as u8))
        } else {
            println!("invalid character code {}", code);
            None
        }
    }
    /// Convert itself into a classpad character code value.
    pub fn to_classpad_char(&self) -> usize {
        match self {
            Self::SingleByte(v) => *v as usize,
            Self::TwoByte(a, b) => match a {
                0xEC => *b as usize + 256,
                0xED => *b as usize + 512,
                0xEE => *b as usize + 768,
                _ => panic!("invalid representation"),
            },
        }
    }
    /// Convert a unicode character ([`char`]) to [`ClasspadChar`].
    ///
    /// Internally uses the mapping defined by [`struct@UNICODE_MAP`].
    pub fn from_unicode_char(ch: char) -> Option<Self> {
        if (32..=126).contains(&(ch as u32)) {
            return Some(Self::SingleByte(ch as u8));
        }
        if let Some(classpad_char_code) = UNICODE_MAP.get_by_second(&ch) {
            return Self::from_classpad_char(*classpad_char_code);
        }
        None
    }
    /// Convert itself into a unicode [`char`].
    ///
    /// Internally uses the mapping defined by [`struct@UNICODE_MAP`].
    pub fn to_unicode_char(&self) -> Option<char> {
        match self {
            Self::SingleByte(ch) => Some(*ch as char),
            Self::TwoByte(_, _) => UNICODE_MAP.get_by_first(&self.to_classpad_char()).copied(),
        }
    }
}
impl PartialOrd for ClasspadChar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_classpad_char().partial_cmp(&other.to_classpad_char())
    }
}
impl Ord for ClasspadChar {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

/**
 * A string that can be stored into an XCP file for the classpad.
 *
 * To construct one, you can use `ClasspadString::from_str`, then chain `attach_*` functions on the next statement
 * to mutate its value.
 */
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
#[repr(transparent)]
pub struct ClasspadString(Vec<ClasspadChar>);
impl ClasspadString {
    /// Internal function used to decipher the XCP file's data block.
    pub(crate) fn from_xcp_data(bytes: &[u8]) -> Self {
        let mut chars: Vec<ClasspadChar> = vec![];
        let mut first_byte: Option<u8> = None;
        for byte in bytes {
            if let Some(first_byte_content) = first_byte {
                chars.push(ClasspadChar::TwoByte(first_byte_content, *byte));
                first_byte = None;
                continue;
            }
            if *byte == 0xEC_u8 {
                // code is within the 257 - 510 range.
                first_byte = Some(*byte);
                continue;
            }
            if *byte == 0xED_u8 {
                // code is within the 513 - 766 range
                first_byte = Some(*byte);
                continue;
            }
            if *byte == 0xEE_u8 {
                first_byte = Some(*byte);
                continue;
            }
            // just parse it normally
            chars.push(ClasspadChar::SingleByte(*byte));
        }
        Self(chars)
    }
    /// Converts a list of classpad character codes into a [`ClasspadString`].
    ///
    /// Refer to the classpad's character code list.
    pub fn from_classpad_chars(codes: &[usize]) -> Self {
        let a: Vec<ClasspadChar> = codes
            .iter()
            .filter_map(|code| ClasspadChar::from_classpad_char(*code))
            .collect();
        Self(a)
    }
    /// Convert itself into a list of bytes that can be embedded within an XCP file.
    pub(crate) fn to_bytes_vec(&self) -> Vec<u8> {
        let mut v: Vec<u8> = vec![];
        for char in &self.0 {
            match char {
                ClasspadChar::SingleByte(c) => v.push(*c),
                ClasspadChar::TwoByte(a, b) => {
                    v.push(*a);
                    v.push(*b);
                }
            }
        }
        v
    }
    /// Convert itself into a unicode string.
    pub fn to_unicode_str(&self) -> String {
        self.0.iter().filter_map(|c| c.to_unicode_char()).collect()
    }
}

impl std::str::FromStr for ClasspadString {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut c: Vec<ClasspadChar> = vec![];
        for char in s.replace("\r\n", "\r").chars() {
            if let Some(ch) = ClasspadChar::from_unicode_char(char) {
                c.push(ch);
            }
        }
        Ok(Self(c))
    }
}

/// The XCP file's file type. Currently, only text files are supported.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum XCPFileType {
    /// text files (you can convert them into program files on the classpad).
    Text,
}

/// An XCP file.
///
/// To read and write to a .xcp file, use [`XCPFile::read_from_file`] and [`XCPFile::write_to_file`].
/// Note that this library does not guarantee generated/parsed XCP files won't be corrupted. Use the library's
/// generated XCP files at your own risk.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct XCPFile {
    /// Currently only text files. This field is not important.
    pub file_type: XCPFileType,
    /// Contents of the data block.
    pub data: ClasspadString,
    /// Folder it extracts to in classpad.
    pub folder_name: String,
    /// name of the text file variable in classpad.
    pub var_name: String,
}
impl XCPFile {
    /// Write the contents of this [`XCPFile`] into a [`Vec<u8>`] buffer.
    ///
    /// This function is mostly reserved for internal use.
    pub fn write_to_vec(&self) -> Vec<u8> {
        let mut content: Vec<u8> = vec![];
        let mut checksum: Wrapping<u8> = Wrapping(0x00); // this checksum will be subtracted by every number. It will become important later
        fn append_bytes(content: &mut Vec<u8>, bytes: Vec<u8>) -> Wrapping<u8> {
            content.append(&mut bytes.clone());
            bytes
                .iter()
                .map(|b| Wrapping(*b))
                .sum::<Wrapping<u8>>()
        }
        fn push_byte(content: &mut Vec<u8>, byte: u8) -> Wrapping<u8> {
            content.push(byte);
            Wrapping(byte)
        }
        fn push_ascii_hex_num(content: &mut Vec<u8>, num: u8) -> Wrapping<u8> {
            let s = format!("{:02x}", num);
            content.append(&mut s.as_bytes().to_vec());
            Wrapping(num)
        }
        // 1.vcp
        let vcp_bytes = "VCP.XDATA".as_bytes().to_vec();
        checksum -= append_bytes(&mut content, vcp_bytes);
        checksum -= push_byte(&mut content, 0x00); // null terminator

        // longnumber
        content.append(&mut "5f4d4353".as_bytes().to_vec());
        checksum -= Wrapping(0x5f_u8) + Wrapping(0x4d_u8) + Wrapping(0x43_u8) + Wrapping(0x53_u8);

        // folname_len (ascii text) - 2 bytes
        checksum -= push_ascii_hex_num(&mut content, (self.folder_name.len() + 1) as u8);
        // folname
        checksum -= append_bytes(&mut content, self.folder_name.as_bytes().to_vec());
        checksum -= push_byte(&mut content, 0x00);

        // varname_len (ascii text)
        checksum -= push_ascii_hex_num(&mut content, (self.var_name.len() + 1) as u8);

        // varname
        checksum -= append_bytes(&mut content, self.var_name.as_bytes().to_vec());
        checksum -= push_byte(&mut content, 0x00);

        // block_31
        content.append(&mut "00000031".as_bytes().to_vec());
        checksum -= Wrapping(0x31_u8);

        // folname2 (padded with 0xff to make it 16 bytes)
        let mut folname2 = self.folder_name.as_bytes().to_vec();
        let diff = 16 - folname2.len();
        folname2.extend(std::iter::repeat(0xff).take(diff));
        checksum -= append_bytes(&mut content, folname2);

        // varname2 (padded with 0xff to make it 16 bytes)
        let mut varname2 = self.var_name.as_bytes().to_vec();
        let diff = 16 - varname2.len();
        varname2.extend(std::iter::repeat(0xff).take(diff));
        checksum -= append_bytes(&mut content, varname2);
        println!("checksum after varname2 {}", checksum);

        // start performing calculations for len1, len2 and data before entering them into content
        let data = self.data.to_bytes_vec();
        let len1_no_pad = 4 + 9 + data.len() + 2;
        let padding_len =
            (Wrapping(0_i32) - Wrapping(15_i32 + data.len() as i32)) & Wrapping(0x03_i32);
        let len1 = len1_no_pad + padding_len.0 as usize;
        let len2 = data.len() + 3;
        // error is somewhere starting here -----------------------------------------------------------
        // len1
        let len1_bytes = (len1 as u32).to_be_bytes(); // to make sure it is 4 bytes
        checksum -= append_bytes(&mut content, len1_bytes.to_vec());
        println!("len1 checksum {}", checksum);
        // block_guq
        let guq_bytes = "GUQ".as_bytes().to_vec();
        checksum -= append_bytes(&mut content, guq_bytes);
        let guq_padding_bytes = std::iter::repeat(0xff).take(10);
        checksum -= append_bytes(&mut content, guq_padding_bytes.collect());
        println!("block guq checksum {}", checksum);
        // len1asc
        let len1asc = format!("{:X}", len1).to_lowercase();
        let len1asc_bytes = format!("{:0>8}", len1asc).as_bytes().to_vec();
        content.append(&mut len1asc_bytes.clone());
        checksum -= Wrapping(len1 as u8); //temp
                                          // len2
        let len2_bytes = (len2 as u32).to_le_bytes();
        checksum -= append_bytes(&mut content, len2_bytes.to_vec());
        // block_zero
        content.extend(std::iter::repeat(0x00).take(9));
        println!("after block0 {}", checksum);

        // checksum isn't changed by 0x00
        // data
        checksum -= append_bytes(&mut content, data);

        // eof
        checksum -= push_byte(&mut content, 0x00);
        checksum -= push_byte(&mut content, 0xff);

        // padding (todo: check)
        checksum -= append_bytes(
            &mut content,
            std::iter::repeat(0x00)
                .take(padding_len.0 as usize)
                .collect(),
        );

        // checksum unaffected by padding
        let checksum_string = format!("{:02x}", checksum);
        content.append(&mut checksum_string.as_bytes().to_vec());
        content
    }
    /// Write the contents of the [`XCPFile`] into an actual [`File`].
    ///
    /// ```rust
    /// // create an [`XCPFile`] with the contents "abc", in the folder "main" with variable name "testvar".
    /// // Do note that var_name and folder_name provided should always be less than 8 bytes (characters).
    /// // Otherwise the function will panic.
    /// // This library does not handle non-single byte folder and variable names.
    /// use classpad_string::*;
    /// use std::str::FromStr;
    /// let xcp_file = XCPFile {
    ///     file_type: XCPFileType::Text,
    ///     data: ClasspadString::from_str("abc").unwrap(),
    ///     folder_name: "main".to_string(),
    ///     var_name: "testvar".to_string(),
    /// };
    /// let mut outfile = std::fs::File::create("./abc-output.xcp").unwrap();
    /// xcp_file.write_to_file(&mut outfile);
    /// // abc-output.xcp can be imported into the classpad.
    /// ```
    pub fn write_to_file(&self, file: &mut File) {
        let contents = self.write_to_vec();
        file.write_all(&contents).unwrap();
    }
    pub fn read_from_vec(content: Vec<u8>) -> Self {
        fn ascii_assert(input: &[u8], s: &str) {
            let read: String = input.iter().map(|by| *by as char).collect();
            assert_eq!(read, s);
        }
        // 1. vcp
        assert_eq!(&content[0..9], "VCP.XDATA".as_bytes());
        assert_eq!(&content[9], &(0x00_u8.to_le_bytes()[0]));
        // 2. longnumber (for 10..18, every byte needs to get converted into a ascii digit)
        ascii_assert(&content[10..18], "5f4d4353");
        // 3. folname_len (in ascii text again)
        // note: subtract 1 for folder_name_len so that we can skip the null terminator.
        let folder_name_digits: String = content[18..20]
            .iter()
            .map(|by| by.to_ascii_lowercase() as char)
            .collect();
        let folder_name_len: usize = folder_name_digits.as_str().parse::<usize>().unwrap() - 1;
        let mut active_byte = 20; // this will track which part of the file we are in.
                                  // 4. folname
        let folder_name: String = content[active_byte..(active_byte + folder_name_len)]
            .iter()
            .map(|by| by.to_ascii_lowercase() as char)
            .collect();
        active_byte += folder_name_len;
        active_byte += 1; // skip the null terminator 0x00
                          // 5. varname_len
        let var_name_digits: String = content[active_byte..(active_byte + 2)]
            .iter()
            .map(|by| by.to_ascii_lowercase() as char)
            .collect();
        let var_name_len: usize = var_name_digits.as_str().parse::<usize>().unwrap() - 1;
        active_byte += 2;
        // 6. varname
        let var_name: String = content[active_byte..(active_byte + var_name_len)]
            .iter()
            .map(|by| by.to_ascii_lowercase() as char)
            .collect();
        active_byte += var_name_len;
        active_byte += 1; // skip null terminator.
                          // 7. block_31
        ascii_assert(&content[active_byte..(active_byte + 8)], "00000031");
        active_byte += 8;
        // skip 32 bytes (it seems to be varname and folname again but with 0xff padding)
        active_byte += 32;
        // len1
        let len1 = u32::from_be_bytes(content[active_byte..(active_byte + 4)].try_into().unwrap());
        assert_eq!(len1 % 4, 0); // divisible by 4
        active_byte += 4;
        // block_guq
        ascii_assert(&content[active_byte..(active_byte + 3)], "GUQ");
        active_byte += 13; // 3 bytes of GUQ + 10 bytes of padding
                           // len1asc
                           // it's len1 again but this time with ascii text format. Skip it
        active_byte += 8;
        // len2
        let len2 = (u32::from_le_bytes(content[active_byte..(active_byte + 4)].try_into().unwrap())
            - 3) as usize;
        active_byte += 4;
        // block_zero (9 of 0x00s)
        active_byte += 9;
        // data
        let data = ClasspadString::from_xcp_data(&content[active_byte..(active_byte + len2)]);
        Self {
            file_type: XCPFileType::Text,
            data,
            folder_name,
            var_name,
        }
    }
    pub fn read_from_file(file: &mut File) -> Self {
        let mut content: Vec<u8> = vec![];
        let read_result = file.read_to_end(&mut content);
        let Ok(_) = read_result else {
            panic!("failed to read file.");
        };
        Self::read_from_vec(content)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn read_xcp() {
        // v-(Program).xcp has the variable `v` which is a text file with `abcdefg` inside.
        let mut file = File::open("./src/v-(Program).xcp").unwrap();
        let xcp = XCPFile::read_from_file(&mut file);
        let mut outfile = File::create("./src/v-output.xcp").unwrap();
        xcp.write_to_file(&mut outfile);
    }
    #[test]
    fn generate_xcp() {
        let file = XCPFile {
            file_type: XCPFileType::Text,
            data: ClasspadString::from_str("abc").unwrap(),
            folder_name: "main".to_string(),
            var_name: "testvar".to_string(),
        };
        let mut opened_file = File::create("./src/abc.xcp").unwrap();
        file.write_to_file(&mut opened_file);
    }
    #[test]
    fn write_integral() {
        let mut s = ClasspadString::from_str("").unwrap();
        s.attach_integral().attach_str("x");
        let file = XCPFile {
            file_type: XCPFileType::Text,
            data: s,
            folder_name: "main".to_string(),
            var_name: "integ".to_string(),
        };
        let mut opened_file = File::create("./src/integ.xcp").unwrap();
        file.write_to_file(&mut opened_file);
    }
}

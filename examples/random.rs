use std::{fs::File, str::FromStr};

use classpad_string::*;

fn main() {
    let s = "ASDFASDFWQqwrefasFDWQAETdsberyrjuiu99(){}#!@#$%^&*";
    let classpad_s = ClasspadString::from_str(s).unwrap();
    let xcp = XCPFile {
        file_type: XCPFileType::Text,
        data: classpad_s,
        folder_name: "main".to_string(),
        var_name: "rands".to_string(),
    };
    let mut outputxcp = File::create("./examples/output/random.xcp").unwrap();
    xcp.write_to_file(&mut outputxcp);
}

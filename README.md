# Classpad_string
Read and write `ClasspadString`s that can be stored in XCP files and loaded onto the CASIO Classpad fx-400.

This library provides the `ClasspadChar` (a single classpad character), `ClasspadString`, and `XCPFile`.
Here is how you can write "this is some text" into a XCP file.
```rust
use classpad_string::*;
use std::str::FromStr;
let text = "this is some text";
let classpad_s = ClasspadString::from_str(text).unwrap();
let xcp = XCPFile {
    file_type: XCPFileType::Text,
    data: classpad_s,
    folder_name: "main".to_string(),
    var_name: "mytext".to_string()
};
let mut outputxcp = std::fs::File::create("mytext.xcp").unwrap();
xcp.write_to_file(&mut outputxcp);
```

### This library is non-official; it does not guarantee that its generated/read XCP files won't be corrupted/have invalid data. Use at your own risk.
// This test case should be a freebie if the previous ones are already working.
// It shows that we can chain method calls on the builder.

use derive_builder::Builder;

#[derive(Builder, Debug)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

fn main() {
    let command = match Command::builder()
        .executable("cargo".to_owned())
        .args(vec!["build".to_owned(), "--release".to_owned()])
        .current_dir("..".to_owned())
        .env(vec![])
        .build() {
            Ok(c) => { println!("{:?}", c); c},
            Err(e) =>{ println!("{}", e); panic!("wtf")}
        };
        

    assert_eq!(command.executable, "cargo");
}

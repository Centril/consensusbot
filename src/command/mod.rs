mod ast;
mod parser;

pub fn main() {
    let _input = r#"
{@rfcbot poll a > a} abcd {@rfcbot merge}@rfcbot reviewed
@rfcbot merge lang
@rfcbot close
@rfcbot cancel
"#;
    //let _input = "@rfcbot concern";
    //let _input = "@rfcbot f? centril";

    match parser::parse(_input) {
        Ok(cmd) => println!("{:#?}", cmd),
        Err(err) => {
            //println!("{}\n", err);
            println!("{:#?}", err);
        },
    }
}

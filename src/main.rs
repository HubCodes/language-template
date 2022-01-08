use parser::parse;

fn main() {
    let code =
        "let x = function(y){ y * 2; };";
    let ast = parse(code);
    println!("{:?}", ast.unwrap())
}

use std::env;

#[path = "2016-01-1.rs"]
mod s_2016_01_1;
#[path = "2016-01-2.rs"]
mod s_2016_01_2;

fn main() {
    if let Some(solution) = env::args().nth(1) {
        println!();
        match solution.to_string().as_str() {
            "2016/01/1" => s_2016_01_1::solve(),
            "2016/01/2" => s_2016_01_2::solve(),
            _other => println!("solution not found: {}", solution),
        };
    } else {
        println!("no argument passed, expected a solution like 2016/01/1 as an argument");
    }
}
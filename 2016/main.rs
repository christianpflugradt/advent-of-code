use std::env;

#[path = "2016-01-1.rs"]
mod s_2016_01_1;
#[path = "2016-01-2.rs"]
mod s_2016_01_2;
#[path = "2016-02-1.rs"]
mod s_2016_02_1;
#[path = "2016-02-2.rs"]
mod s_2016_02_2;
#[path = "2016-03-1.rs"]
mod s_2016_03_1;
#[path = "2016-03-2.rs"]
mod s_2016_03_2;
#[path = "2016-04-1.rs"]
mod s_2016_04_1;
#[path = "2016-04-2.rs"]
mod s_2016_04_2;
#[path = "2016-05-1.rs"]
mod s_2016_05_1;
#[path = "2016-05-2.rs"]
mod s_2016_05_2;
#[path = "2016-06-1.rs"]
mod s_2016_06_1;
#[path = "2016-06-2.rs"]
mod s_2016_06_2;
#[path = "2016-07-1.rs"]
mod s_2016_07_1;
#[path = "2016-07-2.rs"]
mod s_2016_07_2;


fn main() {
    if let Some(solution) = env::args().nth(1) {
        println!();
        match solution.to_string().as_str() {
            "2016/01/1" => s_2016_01_1::solve(),
            "2016/01/2" => s_2016_01_2::solve(),
            "2016/02/1" => s_2016_02_1::solve(),
            "2016/02/2" => s_2016_02_2::solve(),
            "2016/03/1" => s_2016_03_1::solve(),
            "2016/03/2" => s_2016_03_2::solve(),
            "2016/04/1" => s_2016_04_1::solve(),
            "2016/04/2" => s_2016_04_2::solve(),
            "2016/05/1" => s_2016_05_1::solve(),
            "2016/05/2" => s_2016_05_2::solve(),
            "2016/06/1" => s_2016_06_1::solve(),
            "2016/06/2" => s_2016_06_2::solve(),
            "2016/07/1" => s_2016_07_1::solve(),
            "2016/07/2" => s_2016_07_2::solve(),
            _other => println!("solution not found: {}", solution),
        };
    } else {
        println!("no argument passed, expected a solution like 2016/01/1 as an argument");
    }
}
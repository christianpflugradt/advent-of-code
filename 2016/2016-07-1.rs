struct IPv7Address {
    supernet_sequences: Vec<String>,
    hypernet_sequences: Vec<String>,
}

impl IPv7Address {
    fn has_autonomous_bridge_bypass_annotation(&self) -> bool {
        Self::has_abba_in_list(&self.supernet_sequences) && !Self::has_abba_in_list(&self.hypernet_sequences)
    }

    fn has_abba_in_list(list: &Vec<String>) -> bool {
        for sequence in list {
            let chars: Vec<char> = sequence.chars().collect();
            for index in 0..(sequence.len() - 3) {
                if chars[index] == chars[index + 3] && chars[index + 1] == chars[index + 2] && chars[index] != chars[index + 1] {
                    return true
                }
            }
        }
        false
    }

}

fn parse_instruction(instruction: &str) -> IPv7Address {
    let mut address = IPv7Address {
        supernet_sequences: Vec::new(),
        hypernet_sequences: Vec::new(),
    };
    let mut is_hypernet = false;
    let segments: Vec<&str> = instruction.split(|c| vec!['[', ']'].contains(&c)).collect();
    for segment in segments {
        let list = if is_hypernet { &mut address.hypernet_sequences } else { &mut address.supernet_sequences };
        list.push(segment.to_string());
        is_hypernet = !is_hypernet;
    }
    address
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-07.txt").split("\n").collect();
    let result = input.iter()
        .map(|ipv7_data| parse_instruction(ipv7_data))
        .filter(|ipv7_address| ipv7_address.has_autonomous_bridge_bypass_annotation())
        .count();
    print!("{}\n", result);
}


struct IPv7Address {
    supernet_sequences: Vec<String>,
    hypernet_sequences: Vec<String>,
}

impl IPv7Address {
    fn supports_super_secret_listening(&self) -> bool {
        self.has_corresponding_byte_allocation_block(&self.get_area_broadcast_accessors())
    }

    fn get_area_broadcast_accessors(&self) -> Vec<String> {
        let mut area_broadcast_accessors: Vec<String> = Vec::new();
        for sequence in &self.supernet_sequences {
            let chars: Vec<char> = sequence.chars().collect();
            for index in 0..(sequence.len() - 2) {
                if chars[index] == chars[index + 2] && chars[index] != chars[index + 1] {
                    area_broadcast_accessors.push(format!("{}{}{}", chars[index], chars[index + 1], chars[index + 2]));
                }
            }
        }
        area_broadcast_accessors
    }

    fn has_corresponding_byte_allocation_block(&self, area_broadcast_accessors: &Vec<String>) -> bool {
        for area_broadcast_accessor in area_broadcast_accessors {
            let mut iterator = area_broadcast_accessor.chars();
            let a = iterator.nth(0).unwrap_or_else(|| panic!("unexpected string: {}", area_broadcast_accessor));
            let b = iterator.nth(0).unwrap_or_else(|| panic!("unexpected string: {}", area_broadcast_accessor));
            for sequence in &self.hypernet_sequences {
                let chars: Vec<char> = sequence.chars().collect();
                for index in 0..(sequence.len() - 2) {
                    if chars[index] == b && chars[index + 1] == a && chars[index + 2] == b {
                        return true
                    }
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
        .filter(|ipv7_address| ipv7_address.supports_super_secret_listening())
        .count();
    print!("{}\n", result);
}


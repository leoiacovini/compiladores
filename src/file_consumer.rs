use event_stream::EventStream;
use std::collections::VecDeque;
use line_consumer::LineConsumerEvent;
use line_consumer::LineEventType;

use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;


#[derive(Debug)]
pub struct FileConsumerEvent<'a> {
    pub filename: &'a str
}

pub fn consume_file(inp: FileConsumerEvent, state: &mut i32, output_stream: &mut EventStream<LineConsumerEvent>) {
    println!("FileConsumer!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    let f = match File::open(inp.filename) {
        Ok(file) => file,
        Err(_) => {
            panic!("File not found!");
        }
    };
    let file = BufReader::new(&f);
    for (num, file_line) in file.lines().enumerate() {
        *state = num as i32;
        match file_line {
            Ok(file_line) => output_stream.push_last(LineConsumerEvent{
                event_type: LineEventType::Line(file_line)
            }),
            Err(_) => panic!("File line not found!"),
        }
        output_stream.push_last(LineConsumerEvent{
            event_type: LineEventType::EndOfLine
        });
    }
    output_stream.push_last(LineConsumerEvent{
        event_type: LineEventType::EndOfFile
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::prelude::*;
    use std::fs::File;
    use std::fs::remove_file;
    fn create_test_file(filename: &str, mut file_contents: &[u8]) {
        let mut file = match File::create(filename) {
            Ok(file) => file,
            Err(e) => panic!(e),
        };
        match file.write_all(file_contents) {
            Ok(_) => println!("Writed"),
            Err(_) => panic!("Could not write file for test"),
        };
    }
    fn remove_test_file(filename: &str) {
        match remove_file(filename) {
            Ok(_) => println!("Deleted"),
            Err(_) => panic!("Could not delete test file"),
        };
    }
    #[test]
    fn must_read_lines() {
        let test_file_path ="/tmp/test.txt";
        create_test_file(test_file_path, b"Hello \n world!");

        let inp = FileConsumerEvent {
            filename: test_file_path
        };
        let mut state = 0i32;
        let mut lines: VecDeque<LineConsumerEvent> = VecDeque::new();

        consume_file(inp, &mut state, &mut lines);

        assert_eq!(lines, vec![LineConsumerEvent { event_type: LineEventType::Line(String::from("Hello ")) },
                               LineConsumerEvent { event_type: LineEventType::EndOfLine },
                               LineConsumerEvent { event_type: LineEventType::Line(String::from(" world!")) },
                               LineConsumerEvent { event_type: LineEventType::EndOfLine },
                               LineConsumerEvent { event_type: LineEventType::EndOfFile },]);

        remove_test_file(test_file_path);
    }
}

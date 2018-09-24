use event_stream::EventStream;
use std::collections::VecDeque;
use std::fmt;
//#[derive(Clone)]
pub struct EventMachine<'lt, Input:'lt, Output:'lt, State:'lt> where Input: fmt::Debug, Output: fmt::Debug, State: fmt::Debug{
    pub input: &'lt mut EventStream<Input>,
    pub output: &'lt mut EventStream<Output>,
    pub state: &'lt mut State,
    pub consume_fn: fn(Input, &mut State, &mut EventStream<Output>),
}

impl <'lt, Input:, Output, State> EventMachine<'lt, Input, Output, State>  where Input: fmt::Debug, Output: fmt::Debug, State: fmt::Debug {
    pub fn run(&mut self) {
        match self.input.consume_next() {
            Some(value) => (self.consume_fn)(value, self.state, self.output),
            None => println!("No Value!"),
        }
    }

    pub fn println(&self) {
        println!("EventMachine {{");
        println!("  input: {:?}", self.input);
        println!("  output: {:?}", self.output);
        println!("  state: {:?}", self.state);
        println!("}}");
    }

    pub fn run_all(&mut self) {
        while !self.input.is_empty() {
            self.run();
        }
    }
}
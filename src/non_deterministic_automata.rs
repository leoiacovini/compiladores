use std::collections::HashSet;

struct NonDeterministicFiniteAutomata<'lt, S> {
    pub states: &'lt mut HashSet<S>,
    pub accepting_states: &'lt HashSet<S>,
    pub initial_state: S,
    pub transition_function: fn(S) -> HashSet<S>,
}

struct NonDeterministicFiniteAutomataRun<'lt, NDFAutomata> {
    pub automata: &'lt NonDeterministicFiniteAutomata<'lt, NDFAutomata>,
    pub current_state: &'lt mut HashSet<S>,
}

impl <'lt, S> NonDeterministicFiniteAutomata<'lt, S> {
    fn new_run(&self) -> NonDeterministicFiniteAutomataRun<'lt, Self> {
        let mut current: HashSet<S> = HashSet::new();
        current.insert(self.initial_state);
        NonDeterministicFiniteAutomataRun {
            automata: self,
            current_state: &mut current
        }
    }
}

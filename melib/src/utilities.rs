
pub trait ProgressTracker {
    fn new(s: String, total_work: usize) -> Self;

    fn add_work(&mut self, n: usize) -> ();
    fn set_work(&mut self, n: usize) -> ();
    fn work(&mut self, n: usize) -> ();

    fn percentage(&self) -> usize;
    fn description(&self) -> &str;
}

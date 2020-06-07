use derive_builder::Builder;

#[derive(Builder)]
pub struct EmptyCommand();

#[derive(Builder)]
pub struct TupleCommand(String, Vec<String>, Vec<String>, String);

#[derive(Builder)]
pub enum EnumCommand {
    First,
    Second(i32),
}

fn main() {}

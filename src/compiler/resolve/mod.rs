pub mod plir;

trait TraversePLIR {
    type Output;

    fn traverse_plir(self) -> Self::Output;
}
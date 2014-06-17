import Foreign.Storable
import Numerical.HBLAS.BLAS
import Numerical.HBLAS.MatrixTypes

-- Generate the constant mutable square matrix of the given type and dimensions.
constMatrix :: Storable a => Int -> a -> IO (IODenseMatrix Row a)
constMatrix n k = generateMutableDenseMatrix SRow (n,n) (const k)

example_dgemm :: IO ()
example_dgemm = do
    left  <- constMatrix 2 (2 :: Double)
    right <- constMatrix 2 (3 :: Double)
    out   <- constMatrix 2 (0 :: Double)

    dgemm NoTranspose NoTranspose 1.0 1.0 left right out

    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting

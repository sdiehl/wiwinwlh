{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Text.PrettyPrint.Mainland
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.CUDA as Cuda

cuda_fun :: String -> Int -> Float -> C.Func
cuda_fun fn n a = [Cuda.cfun|

__global__ void $id:fn (float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if ( i<$n ) { y[i] = $a*x[i] + y[i]; }
}

|]

cuda_driver :: String -> Int -> C.Func
cuda_driver fn n = [Cuda.cfun|

void driver (float *x, float *y) {
  float *d_x, *d_y;

  cudaMalloc(&d_x, $n*sizeof(float));
  cudaMalloc(&d_y, $n*sizeof(float));

  cudaMemcpy(d_x, x, $n, cudaMemcpyHostToDevice);
  cudaMemcpy(d_y, y, $n, cudaMemcpyHostToDevice);

  $id:fn<<<($n+255)/256, 256>>>(d_x, d_y);

  cudaFree(d_x);
  cudaFree(d_y);
  return 0;
}

|]

makeKernel :: String -> Float -> Int -> [C.Func]
makeKernel fn a n = [
    cuda_fun fn n a
  , cuda_driver fn n
  ]

main :: IO ()
main = do
  let ker = makeKernel "saxpy" 2 65536
  mapM_ (print . ppr) ker

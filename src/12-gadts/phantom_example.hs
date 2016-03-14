import Data.Text

data Cryptotext
data Plaintext

data Msg a = Msg Text

encrypt :: Msg Plaintext -> Msg Cryptotext
encrypt = undefined

decrypt :: Msg Plaintext -> Msg Cryptotext
decrypt = undefined

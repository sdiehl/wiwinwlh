import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)
import Network.Socket.Types (SockAddr)

data MsgHdr
  = MsgHdr
      { msgName :: !(Ptr SockAddr),
        msgNameLen :: !CUInt,
        msgIov :: !(Ptr IOVec),
        msgIovLen :: !CSize
      }

instance Storable MsgHdr where
  sizeOf _ = (56)
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    name <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
    nameLen <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
    iov <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
    iovLen <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
    return $ MsgHdr name nameLen iov iovLen

  poke p mh = do
    zeroMemory p (56)
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (msgName mh)
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (msgNameLen mh)
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (msgIov mh)
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p (msgIovLen mh)

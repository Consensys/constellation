{-# LANGUAGE RecordWildCards #-}
module Constellation.Util.Argon2 where

-- Extract from argon2 by Ollie Charles: https://github.com/ocharles/argon2
-- Modified to allow user to define outLen in hash/hash'.

-- Copyright (c) 2016, Ollie Charles

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Ollie Charles nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Control.Exception
import Crypto.Argon2
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import qualified Crypto.Argon2.FFI as FFI
import qualified Data.ByteString as BS

-- | Encode a password with a given salt and 'HashOptions' and produce a stream
-- of bytes.
hash :: HashOptions -- ^ Options pertaining to how expensive the hash is to calculate.
     -> Int -- ^ Desired output length
     -> BS.ByteString -- ^ The password to hash. Must be less than 4294967295 bytes.
     -> BS.ByteString -- ^ The salt to use when hashing. Must be less than 4294967295 bytes.
     -> BS.ByteString -- ^ The un-encoded password hash.
hash options outLen password salt =
  unsafePerformIO (hash' options outLen password salt FFI.argon2i_hash_raw FFI.argon2d_hash_raw)

variant :: a -> a -> Argon2Variant -> a
variant a _ Argon2i = a
variant _ b Argon2d = b
{-# INLINE variant #-}

type Argon2Unencoded = Word32 -> Word32 -> Word32 -> CString -> CSize -> CString -> CSize -> CString -> CSize -> IO Int32

hash' :: HashOptions
      -> Int
      -> BS.ByteString
      -> BS.ByteString
      -> Argon2Unencoded
      -> Argon2Unencoded
      -> IO BS.ByteString
hash' options@HashOptions{..} outLen password salt argon2i argon2d =
  do let saltLen = fromIntegral (BS.length salt)
         passwordLen = fromIntegral (BS.length password)
     out <- mallocBytes outLen
     res <-
       BS.useAsCString password $
       \password' ->
         BS.useAsCString salt $
         \salt' ->
           argon2 hashIterations
                  hashMemory
                  hashParallelism
                  password'
                  passwordLen
                  salt'
                  saltLen
                  out
                  (fromIntegral outLen)
     handleSuccessCode res options password salt
     BS.packCStringLen (out, outLen)
  where argon2 = variant argon2i argon2d hashVariant

handleSuccessCode :: Int32
                  -> HashOptions
                  -> BS.ByteString
                  -> BS.ByteString
                  -> IO ()
handleSuccessCode res HashOptions{..} password salt =
  let saltLen = fromIntegral (BS.length salt)
      passwordLen = fromIntegral (BS.length password)
  in case res of
       a
         | a `elem` [FFI.ARGON2_OK] -> return ()
         | a `elem` [FFI.ARGON2_SALT_TOO_SHORT,FFI.ARGON2_SALT_TOO_LONG] ->
           throwIO (Argon2SaltLengthOutOfRange saltLen)
         | a `elem` [FFI.ARGON2_PWD_TOO_SHORT,FFI.ARGON2_PWD_TOO_LONG] ->
           throwIO (Argon2PasswordLengthOutOfRange passwordLen)
         | a `elem` [FFI.ARGON2_TIME_TOO_SMALL,FFI.ARGON2_TIME_TOO_LARGE] ->
           throwIO (Argon2IterationCountOutOfRange hashIterations)
         | a `elem` [FFI.ARGON2_MEMORY_TOO_LITTLE,FFI.ARGON2_MEMORY_TOO_MUCH] ->
           throwIO (Argon2MemoryUseOutOfRange hashMemory)
         | a `elem`
             [FFI.ARGON2_LANES_TOO_FEW
             ,FFI.ARGON2_LANES_TOO_MANY
             ,FFI.ARGON2_THREADS_TOO_FEW
             ,FFI.ARGON2_THREADS_TOO_MANY] ->
           throwIO (Argon2ParallelismOutOfRange hashParallelism)
         | otherwise -> throwIO (Argon2Exception a)

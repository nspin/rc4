# rc4

RC4 stream cipher in pure Haskell, using the strict state monad (`ST`).

The `RC4` type is a context, composed of two `STRef` indicies, and on `STArray`, and is parameterized by a state token.
It's actual implementation is not exposed, but you can:

*   Schedule a context with a key:

```haskell
schedule :: ByteString -> ST s (RC4 s)
```
*   Generate a byte from the context:

```haskell
generate :: RC4 s -> ST s Word8
```

Various convenience functions are also provided.

*NOTE:* This package uses strict bytestrings.

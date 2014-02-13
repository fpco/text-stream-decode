text-stream-decode
==================

Streaming decoding functions for UTF encodings.

[![Build Status](https://travis-ci.org/fpco/text-stream-decode.png?branch=master)](https://travis-ci.org/fpco/text-stream-decode)

The `text` package provides high performance functions for decoding strict and
lazy `ByteString`s into `Text`. However, these functions present two issues for
streaming data libraries: they throw exceptions from pure code, and are not
designed for incremental consumption. This library addresses both issues with a
unified API for UTF-8, -16LE, -16BE, -32LE, and -32BE. It is intended for use
by high level streaming data libraries, such as conduit, enumerator, iteratee,
and pipes.

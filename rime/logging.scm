#!r6rs
(library (rime logging)
  (export logger
          log-level
          :source-location
          :who
          :level
          :trace
          :info
          :debug
          :warning
          :error
          :critical)
  (import (rime logging __logger)))

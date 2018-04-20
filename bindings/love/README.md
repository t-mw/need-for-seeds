# Urn Löve2D Bindings

## Using
Clone the repository and add it to your include path by passing `-i` to the urn compiler.

## Usage Example
```common-lisp
(import love/love (defevent))
(import love/graphics)

(defevent :init ())
(defevent :draw ()
  (love/graphics/print "Hello World!" 64 64))
```

Most of Löve2D maps rather intuitively to Urn. Simply replace all table indexing `.`s with `/`, and camelCase with lisp-case. However, if you'd like a more complete 
example nonetheless, you may have a look at [this implementation of Pong](https://gitlab.com/Lignum/urn-pong) using the bindings.

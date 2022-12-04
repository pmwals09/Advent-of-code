# Advent of Code 2022

Trying things in Rust! BlAzInGlY fAsT. Typed up the wazoo. Traits instead of interfaces.

Notes as I go:
- [Use references to stack-allocated data structures and the compiler will coerce the heap-allocated version to the stack-allocated version, but not vice-versa](https://stackoverflow.com/questions/40006219/why-is-it-discouraged-to-accept-a-reference-to-a-string-string-vec-vec-o)
- Takes longer to get to a solution because I'm fighting weird compiler stuff I don't get, but so far most of my solutions have been right on the first go
- Wish a standard library construct had that one extra method that would make your problem really easy? Write your own trait and implement it on that item! I imagine in a larger project it could be a problem, like the issues with extending JavaScript in 2016 because everyone used MooTools and changed the Array prototype.

# 들어가며

- [Build Your Own Lisp - Learn C and build your own programming language in 1000 lines of code!](https://www.buildyourownlisp.com/)
  - [github](https://github.com/orangeduck/BuildYourOwnLisp/)


Build Your Own Lisp가 c로 구현되어 있는데, zig로 한번 구현해보자.

``` sh
$ zig version
0.15.2
```

## 0.15.x

- https://dev.to/bkataru/zig-0151-io-overhaul-understanding-the-new-readerwriter-interfaces-30oe
- ArrayList에 init(allocator) 부분이 삭제됨
- addExecutable에서 .root_source_file 안됨
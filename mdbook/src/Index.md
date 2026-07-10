# 들어가며

- [Build Your Own Lisp - Learn C and build your own programming language in 1000 lines of code!](https://www.buildyourownlisp.com/)
  - [github](https://github.com/orangeduck/BuildYourOwnLisp/)


Build Your Own Lisp가 c로 구현되어 있는데, zig로 한번 구현해보자.

``` sh
$ zig version
0.16.0
```

CFFI(C Foreign Function Interface) 연습겸, 구현부를 제외한 mpc/libedit/string 부분은 그대로 쓰기로함. zig는 c연동을 신경써서 그런지 비교적 무난하게 적용할 수 있었음.

## 0.15.x

- https://dev.to/bkataru/zig-0151-io-overhaul-understanding-the-new-readerwriter-interfaces-30oe
- ArrayList에 init(allocator) 부분이 삭제됨
- addExecutable에서 .root_source_file 안됨

## 0.16.0

- https://ziglang.org/download/0.16.0/release-notes.html
- 크게 바꿨네
- b.addTranslateC
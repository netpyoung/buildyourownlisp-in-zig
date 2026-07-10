# appendix_a_hand_rolled_parser

- [origin](https://www.buildyourownlisp.com/appendix_a_hand_rolled_parser)
  - [hand_rolled_parser.c](https://github.com/orangeduck/BuildYourOwnLisp/blob/master/src/hand_rolled_parser.c)

## 0x00

- mpc를 사용한 것에 대한 불만에 대한 반박
- 여기서는 mpc를 걷어내고 파서를 직접 제작한다.

## zig로 포팅을 마치며 - 후기

- c로 된걸 zig로 포팅하며 zig에서 괜찮은점/별로인 점을 보다 직접적으로 느끼게 됨
- 따라가기 식이지만 완성하니 드디어 끝냈다라는 안도감.
- 저자의 mpc를 zig로 구현해보면 어떨까.
- lval.zig, lenv.zig식으로 나누어 봤는데 lazy import라 순환 임포트도 어느정도 커버되는.
  - 다만 파일이 늘어나 관리가 힘드니 해보고 롤백.
- mdbook의 highlight.js에 아직 zig를 지원안해 shiki로 대체

## src/hand_rolled_parser.zig

``` zig
{{#include ../../lispy-zig/hand_rolled_parser/src/hand_rolled_parser.zig}}
```

## src/c_libedit.h

``` zig
{{#include ../../lispy-zig/hand_rolled_parser/src/c_libedit.h}}
```

## src/c_string.h

``` zig
{{#include ../../lispy-zig/hand_rolled_parser/src/c_string.h}}
```

## build.zig

``` zig
{{#include ../../lispy-zig/hand_rolled_parser/build.zig}}
```

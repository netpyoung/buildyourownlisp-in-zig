# https://just.systems

[private]
default:
    @just --list


[group('mdbook')]
run:
    mdbook serve --open --port 3000 mdbook/

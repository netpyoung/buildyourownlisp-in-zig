const std = @import("std");

const c_mpc = @cImport({
    @cInclude("mpc.h");
});

const fpt_Free = *const fn (?*anyopaque) callconv(.c) void;

fn doge_code() void {
    const Adjective: ?*c_mpc.mpc_parser_t = c_mpc.mpc_or(
        4,
        c_mpc.mpc_sym("wow"),
        c_mpc.mpc_sym("many"),
        c_mpc.mpc_sym("so"),
        c_mpc.mpc_sym("such"),
    );
    const Noun: ?*c_mpc.mpc_parser_t = c_mpc.mpc_or(
        5,
        c_mpc.mpc_sym("lisp"),
        c_mpc.mpc_sym("language"),
        c_mpc.mpc_sym("book"),
        c_mpc.mpc_sym("build"),
        c_mpc.mpc_sym("c"),
    );

    const free: fpt_Free = c_mpc.free;

    const Phrase: ?*c_mpc.mpc_parser_t = c_mpc.mpc_and(
        2,
        c_mpc.mpcf_strfold,
        Adjective,
        Noun,
        free,
    );

    const Doge: ?*c_mpc.mpc_parser_t = c_mpc.mpc_many(
        c_mpc.mpcf_strfold,
        Phrase,
    );

    // 여기서 파싱 작업 수행 가능

    c_mpc.mpc_delete(Doge);
}

fn doge_grammar() void {
    const Adjective = c_mpc.mpc_new("adjective");
    const Noun = c_mpc.mpc_new("noun");
    const Phrase = c_mpc.mpc_new("phrase");
    const Doge = c_mpc.mpc_new("doge");

    _ = c_mpc.mpca_lang(c_mpc.MPCA_LANG_DEFAULT,
        \\ adjective : "wow" | "many"
        \\           | "so" | "such";
        \\ noun      : "lisp" | "language"
        \\           | "book" | "build" | "c";
        \\ phrase    : <adjective> <noun>;
        \\ doge      : <phrase>*;
    , Adjective, Noun, Phrase, Doge);

    // 여기서 파싱 작업 수행 가능

    _ = c_mpc.mpc_cleanup(4, Adjective, Noun, Phrase, Doge);
}

pub fn main() !void {
    doge_code();
    doge_grammar();
}

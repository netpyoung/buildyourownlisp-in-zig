const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_module = b.createModule(.{
        .root_source_file = b.path("src/ch11.zig"),
        .target = target,
        .optimize = optimize,
    });

    { // library: editline
        if (target.result.os.tag != .windows) {
            const translate_libedit = b.addTranslateC(.{
                .root_source_file = b.path("src/c_libedit.h"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            });

            const libedit_module = translate_libedit.createModule();

            if (target.result.os.tag == .linux) {
                libedit_module.addIncludePath(.{ .cwd_relative = "/usr/include/" });
            } else if (target.result.os.tag == .macos) {
                libedit_module.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
                libedit_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
            }
            libedit_module.linkSystemLibrary("edit", .{});
            root_module.addImport("libedit", libedit_module);
        }
    }
    { // library: mpc
        const translate_mpc = b.addTranslateC(.{
            .root_source_file = b.path("src/c_mpc.h"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        });

        const mpc_module = translate_mpc.createModule();
        translate_mpc.addIncludePath(.{ .cwd_relative = "../mpc" });
        mpc_module.addCSourceFile(.{ .file = b.path("../mpc/mpc.c") });
        root_module.addImport("c_mpc", mpc_module);
    }

    root_module.link_libc = true;

    const exe = b.addExecutable(.{
        .name = "ch11",
        .root_module = root_module,
    });

    // zig-out/bin/ 아래에 실행파일을 복사해 줌.
    b.installArtifact(exe);

    // zig build <원하는 스탭이름>을 하면 바이너리를 실행함.
    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);
}

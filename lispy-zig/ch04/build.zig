const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_module = b.createModule(.{
        .root_source_file = b.path("src/ch04.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "ch04",
        .root_module = root_module,
    });

    { // library: editline
        if (target.result.os.tag != .windows) {
            if (target.result.os.tag == .linux) {
                exe.addIncludePath(.{ .cwd_relative = "/usr/include/" });
            } else if (target.result.os.tag == .macos) {
                exe.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
                exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
            }
            exe.linkSystemLibrary("edit");
        }
    }
    exe.linkLibC();

    // zig-out/bin/ 아래에 실행파일을 복사해 줌.
    b.installArtifact(exe);

    // zig build <원하는 스탭이름>을 하면 바이너리를 실행함.
    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);
}

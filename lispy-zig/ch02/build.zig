const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_module = b.createModule(.{
        .root_source_file = b.path("src/ch02.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "ch02",
        .root_module = root_module,
    });

    // zig-out/bin/ 아래에 실행파일을 복사해 줌.
    b.installArtifact(exe);

    // zig build <원하는 스탭이름>을 하면 바이너리를 실행함.
    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);
}

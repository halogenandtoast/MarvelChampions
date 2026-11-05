"""Exercise cache recovery with a fake Stack; no Docker or Haskell compiler needed."""
import os
from pathlib import Path
import subprocess
import tempfile
import time
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "docker-build-backend.sh"


class CacheRecoveryTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.work = self.root / "work"
        self.work.mkdir()
        self.objects = self.work / "dist/x86_64-linux/ghc-9.14.1/build"
        self.objects.mkdir(parents=True)
        self.stack = self.root / "stack"
        self.stack.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$MARVEL_BUILD_WORK_DIR/args"\nexit "${FAKE_EXIT:-0}"\n')
        self.stack.chmod(0o755)
        self.env = dict(os.environ, PATH=str(self.root) + os.pathsep + os.environ["PATH"], MARVEL_BUILD_WORK_DIR=str(self.work))

    def build(self, exit_code=0):
        return subprocess.run(["sh", str(SCRIPT)], env=dict(self.env, FAKE_EXIT=str(exit_code)), capture_output=True, text=True)

    def artifact(self, name, modified):
        path = self.objects / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"nonempty, possibly truncated")
        os.utime(path, (modified, modified))
        return path

    def test_success_preserves_warm_objects(self):
        obj = self.artifact("Marvel/Old.o", time.time() - 20)
        for _ in range(2):
            self.assertEqual(self.build().returncode, 0)
            self.assertTrue(obj.exists())
            self.assertTrue((self.work / ".docker-build-complete").exists())
            self.assertFalse((self.work / ".docker-build-in-progress").exists())
        self.assertNotIn("--force-dirty", (self.work / "args").read_text())

    def test_interrupted_build_repairs_nonzero_artifacts_and_preserves_older_objects(self):
        stamp = self.work / ".docker-build-in-progress"
        stamp.touch()
        before = stamp.stat().st_mtime - 10
        after = stamp.stat().st_mtime + 10
        older = self.artifact("Marvel/Old.o", before)
        touched = [self.artifact("Marvel/New" + suffix, after) for suffix in [".o", ".hi", ".dyn_o", ".dyn_hi", ".hi-boot", ".o-boot", ".dyn_hi-boot", ".dyn_o-boot", ".o.tmp"]]
        archive = self.artifact("libHSMarvel.a", after)
        executable = self.artifact("marvel-champions/marvel-champions", before)
        self.assertEqual(self.build().returncode, 0)
        self.assertTrue(older.exists())
        for path in touched + [archive, executable]:
            self.assertFalse(path.exists(), path)
        self.assertIn("--force-dirty", (self.work / "args").read_text())

    def test_compiler_failure_preserves_finished_modules_but_drops_incomplete_pairs(self):
        self.stack.write_text("""#!/bin/sh
printf '%s\\n' "$@" > "$MARVEL_BUILD_WORK_DIR/args"
if [ "${FAKE_EXIT:-0}" != 0 ]; then
  directory="$MARVEL_BUILD_WORK_DIR/dist/x86_64-linux/ghc-9.14.1/build/Marvel"
  mkdir -p "$directory"
  printf interface > "$directory/Finished.hi"
  printf object > "$directory/Finished.o"
  printf interface > "$directory/Incomplete.hi"
  : > "$directory/Incomplete.o"
fi
exit "${FAKE_EXIT:-0}"
""")
        # Installed interfaces have no adjacent .o files and must be retained.
        installed = self.work / "install/abi/lib/Marvel/Installed.hi"
        installed.parent.mkdir(parents=True)
        installed.write_bytes(b"installed interface")
        stale_object = self.artifact("Marvel/Stale.o", time.time() - 20)
        stale_interface = self.artifact("Marvel/Stale.hi", time.time() - 10)
        self.assertEqual(self.build(17).returncode, 17)
        self.assertTrue(installed.exists())
        self.assertFalse(stale_object.exists())
        self.assertFalse(stale_interface.exists())
        finished = self.objects / "Marvel/Finished.o"
        self.assertTrue(finished.exists())
        self.assertFalse((self.objects / "Marvel/Incomplete.hi").exists())
        self.assertFalse((self.objects / "Marvel/Incomplete.o").exists())
        self.assertEqual(self.build().returncode, 0)
        self.assertTrue(finished.exists())

    def test_failed_stack_keeps_the_cache_marked_dirty(self):
        complete = self.work / ".docker-build-complete"
        complete.touch()
        self.assertEqual(self.build(17).returncode, 17)
        self.assertFalse(complete.exists())
        self.assertTrue((self.work / ".docker-build-in-progress").exists())
        self.assertEqual(self.build().returncode, 0)
        self.assertIn("--force-dirty", (self.work / "args").read_text())
        self.assertFalse((self.work / ".docker-build-in-progress").exists())


if __name__ == "__main__":
    unittest.main()

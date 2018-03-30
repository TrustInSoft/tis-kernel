# tis-interpreter

This is TIS Interpreter,
an interpreter of C for detecting undefined behavior.

tis-interpreter detects subtle bugs in C programs that may not have
eye-visible effects when executing the same programs compiled in the
traditional way. Some of the bugs that are discovered lead to security
vulnerabilities. Fortunately, most don’t.

tis-interpreter works by interpreting C programs statement by
statement from beginning to end, verifying at each statement whether
the program can invoke undefined behavior. This makes it comparable to
Valgrind and C compiler sanitizers (UBSan, ASan, …). The recommended
use is to apply tis-interpreter to existing tests for
security-sensitive C code in which a bug could have dramatic
consequences. tis-interpreter can detect violations of the C standard
even when applied to regression tests that have never revealed any
problem.

At this stage, the best uses for tis-interpreter are pure C libraries
with as few dependencies as possible and existing tests. After
compilation (or after downloading a binary snapshot), you can
experiment with examples of increasing difficulty (see the
[EXAMPLES.md](Examples.md) file)

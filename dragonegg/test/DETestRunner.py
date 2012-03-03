import os
import StringIO
import Test
import TestRunner
import Util

def describeFailure(output, cmd, out, err, exitCode):
    print >>output, "Command:",
    print >>output, ' '.join(cmd)
    print >>output, "Failed with exit code: %r" % exitCode
    if out:
        print >>output, "Command output (stdout):"
        print >>output, "--"
        output.write(out)
        print >>output, "--"
    if err:
        print >>output, "Command output (stderr):"
        print >>output, "--"
        output.write(err)
        print >>output, "--"


def compareCommands(cmds, args, cwd=None):
    def executeOne(cmd):
        return TestRunner.executeCommand(cmd + args, cwd)

    results = map(executeOne, cmds)
    failed = False

    # Check that all commands exited with the same code.
    out,err,commonExitCode = results[0]
    for result in results:
        out,err,exitCode = result
        if exitCode != commonExitCode:
            failed = True
            break

    # TODO: Consider comparing command output as well as the exit codes.
    if not failed:
        return (Test.PASS, None)

    # Failure.
    output = StringIO.StringIO()
    for cmd,result in zip(cmds,results):
       out,err,exitCode = result
       if exitCode != 0:
           describeFailure(output, cmd + args, out, err, exitCode)
    return (Test.FAIL, output.getvalue())


def executeCompilatorTest(test, litConfig, compilers, flags, suffix_flags,
                          skip, xfails):
    test_path = '/'.join(test.path_in_suite)

    # Skip this test if requested to do so.
    if test_path in skip:
        return (Test.UNSUPPORTED, None)

    # Create the output directory if it does not already exist.
    execpath = test.getExecPath()
    execdir,execbase = os.path.split(execpath)
    tmpDir = os.path.join(execdir, 'Output')
    tmpDir = os.path.join(tmpDir, execbase)
    Util.mkdir_p(tmpDir)

    # Is this test expected to fail?
    isXFail = test_path in xfails

    # The file should be compiled to assembler.
    common_args = ['-S', test.getSourcePath()]

    # Add any file specific flags.
    srcbase,srcext = os.path.splitext(test.getSourcePath())
    if srcext in suffix_flags:
      common_args += suffix_flags[srcext]

    # Compile the test.
    for args in flags:
        result,output = compareCommands(compilers, common_args + args, tmpDir)
        if result != Test.PASS:
            return (Test.XFAIL if isXFail else result,output)
    return (Test.XPASS if isXFail else Test.PASS, None)

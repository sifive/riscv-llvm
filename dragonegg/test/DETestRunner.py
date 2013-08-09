import os
import StringIO
import DEUtils

from lit import Test
import lit.util

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
        return DEUtils.executeCommand(cmd + args, cwd)

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


def generateFortranModules(cmd, srcPath, OutputDir):
    # Fortran 90 code often fails to compile because it needs modules defined by
    # other files in the same directory.  If this seems to be happening then try
    # to generate all of the required modules by compiling every Fortran file in
    # the same directory.
    srcDir,srcBase = os.path.split(srcPath)
    cmd = cmd + ['-I', srcDir, '-fsyntax-only']

    # If the file compiles OK or isn't failing because of lacking modules then
    # there is no point in trying to generate modules.
    out,err,exitCode = DEUtils.executeCommand(cmd + [srcPath], OutputDir)
    if exitCode == 0 or err is None or "Can't open module file" not in err:
        return

    # Drat, it fails to compile.  Generate modules for every Fortran file in the
    # source directory.
    fortranSuffixes = DEUtils.getSuffixesForLanguage('fortran')
    filesToCompile = []
    for filename in os.listdir(srcDir):
        filepath = os.path.join(srcDir, filename)
        if not os.path.isdir(filepath):
            base,ext = os.path.splitext(filename)
            if ext in fortranSuffixes:
                filesToCompile.append(filepath)

    # Compile every file, returning triumphantly once the original file manages
    # to compile, or giving up miserably if no progress is being made.
    newFilesToCompile = []
    while filesToCompile != newFilesToCompile:
        newFilesToCompile = []
        # Compile each file in turn.
        for path in filesToCompile:
            out,err,exitCode = DEUtils.executeCommand(cmd + [path], OutputDir)
            if exitCode != 0 and err is not None and "Can't open module file" in err:
                # It failed to compile due to a missing module.  Remember it for
                # the next round.
                newFilesToCompile.append(path);
            elif path == srcPath:
                # The original file compiled, or at least didn't fail to compile
                # due to a lacking module.  Return triumphantly!
                return
        # Arrange for the next iteration to compile the files that were missing
        # modules this time round.
        filesToCompile, newFilesToCompile = newFilesToCompile, filesToCompile

    # The set of files missing modules didn't change, give up miserably.
    return


def executeCompilatorTest(test, litConfig, compilers, flags, language_flags,
                          skip, xfails):
    test_path = '/'.join(test.path_in_suite)

    # Skip this test if requested to do so.
    if test_path in skip:
        return (Test.UNSUPPORTED, None)

    # Create the output directory if it does not already exist.
    execPath = test.getExecPath()
    execDir,execBase = os.path.split(execPath)
    tmpDir = os.path.join(execDir, 'Output')
    tmpDir = os.path.join(tmpDir, execBase)
    lit.util.mkdir_p(tmpDir)

    # Is this test expected to fail?
    isXFail = test_path in xfails

    # The file should be compiled to assembler.
    srcPath = test.getSourcePath();
    common_args = ['-S', srcPath]

    # Look for headers and such-like in the directory containing the source.
    srcDir,srcBase = os.path.split(srcPath)
    common_args += ['-I', srcDir]

    # Add any file specific flags.
    srcBase,srcExt = os.path.splitext(srcPath)
    language = DEUtils.getLanguageForSuffix(srcExt)
    if language in language_flags:
      common_args += language_flags[language]

    # Fortran files may not compile because they need modules provided by other
    # Fortran files.  Workaround this by generating missing modules if possible.
    if language == 'fortran':
      generateFortranModules(compilers[0], srcPath, tmpDir)

    # Compile the test.
    for args in flags:
        result,output = compareCommands(compilers, common_args + args, tmpDir)
        if result != Test.PASS:
            return (Test.XFAIL if isXFail else result,output)
    return (Test.XPASS if isXFail else Test.PASS, None)

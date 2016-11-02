from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.testsuite.result import Result
from gnatpython.ex import Run, STDOUT, PIPE
from gnatpython.fileutils import mkdir, rm
import os
import platform

"""
This file provides various test drivers.
They read the following common properties from the test.yaml
file. These properties are specified here with their default
value.

    project: null
    --  The name of the project to use (for gprbuild, gnatprove,...)
    --  If this is specified, it must be the name of a project file
    --  in the test directory. Otherwise, a temporary "auto_<testname>.gpr"
    --  file is created and deleted on exit, unless "-k" is specified.

    mains: ['mains.adb']
    --  List of main units for the project

    manual: false
    --  If true, this test is not run automatically when "./testsuite.py"
    --  is run. It is only run when specified explicitly on the command
    --  line. This can be used for long running tests.

    baseline: 'test.out'
    --  The name of the file to use as the expected output.
    --  If this is unspecified, the expected output is either the
    --  contents of 'test.out', if it exists, or the empty string.
    --  When the output is expected to be empty, an empty string
    --  can be specified. Otherwise, it should be the name of a file
    --  in the test directory.

    sort_output : false
    --  This indicates that the output of the test must be sorted
    --  before it is compared with the baseline

    pre: []
    --  A list of strings, the commands to execute before running the
    --  test driver. The project has already been created at this point.

Run the testsuite with
    BUILD=Debug ./testsuite.py
to compile the tests with assertions, and thus run pre and post conditions.

"""


class BuildError(Exception):
    pass


class Disabled(Exception):
    pass


class AbstractDriver(TestDriver):

    def create_project_if_needed(self, mains):
        """
        Create a standard project if none exists in the test directory.
        `self.project` must be set

        :param List(str) mains: list of main units
        """
        self.project_is_tmp = False
        fromenv = self.test_env.get('project')

        if fromenv is not None:
            # File must exist, don't check
            self.project = os.path.join(self.working_dir, fromenv)
            return

        # Create a new file automatically. It has a name specific to the
        # test, so that an aggregate project shows meaningful info.
        defaultname = "Auto_%s" % (self.test_env['test_name'].title(), )
        defaultfile = defaultname.lower() + '.gpr'
        self.project = os.path.join(self.working_dir, defaultfile)
        self.project_is_tmp = True
        file(self.project, "w").write("""
with "containers_shared";
with "containers";
with "gnatcoll";
project %(name)s is
   for Source_Dirs use (".", "../shared/");
   for Main use (%(mains)s);
   for Object_Dir use "obj";
   package Compiler renames Containers_Shared.Compiler;
   package Builder renames Containers_Shared.Builder;
   package Binder renames Containers_Shared.Binder;
   package Linker renames Containers_Shared.Linker;
end %(name)s;""" % {'name': defaultname,
                    'mains': ", ".join('"%s"' % m for m in mains)})

    def gprbuild(self, mode='Debug'):
        """
        Runs gprbuild on `self.project`

        :param str mode: the build mode, either Debug or Production
        """
        p = Run(cmds=['gprbuild', '-q', '-p', '-P', self.project,

                      # Don't want to use -gnatp, we need the checks for the
                      # testsuite
                      '-XBUILD=%s' % mode],
                error=STDOUT,
                cwd=self.working_dir)
        self.result.actual_output += p.out
        if p.status != 0:
            self.result.set_status('FAILED', 'Compilation failed')
            raise BuildError()

    def gnatprove(self, sources=[]):
        """
        Runs gnatprove on `self.project`
        """
        if sources:
            switches = ['-u'] + sources
        else:
            switches = ['-U']

        # Use Debug mode so that optimization switches like Disable Overflow
        # are not used for gnatprove
        p = Run(
            cmds=['gnatprove', '-j0', '-f', '-q',
                  '--level=2', '--dbg-proof-only',
                  '-P%s' % self.project] + switches,
            error=STDOUT,
            env={"BUILD": "Debug"},
            ignore_environ=False,
            cwd=self.working_dir)
        self.result.actual_output += p.out
        if p.status != 0:
            self.result.set_status('FAILED', 'gnatprove failed')
            raise BuildError()

    def set_expected_output(self):
        """
        Set the expected output in `self.result`
        """
        baseline = self.test_env.get('baseline')
        if baseline == '':
            self.result.expected_output = ''
        elif baseline is None:
            # Default is test.out if it exists, empty string otherwise
            baseline = os.path.join(self.working_dir, 'test.out')
            if os.path.isfile(baseline):
                self.result.expected_output = file(baseline).read()
            else:
                self.result.expected_output = ''
        else:
            # File specified by the user
            baseline = os.path.join(self.working_dir, baseline)
            self.result.expected_output = file(baseline).read()

    def check_if_must_run(self):
        """
        Check whether the test should be run:
        this is true for tests that do not have a 'manual:true' setting in
        their test.yaml. For the other tests, they are only run if specified
        explicitly on the command line.
        """
        manual = self.test_env.get('manual', False)
        if manual:
            cmdline = self.global_env['containers']['test_on_command_line']

            # Some shells, when using completion, will add a final '/' after
            # directory names, so we end up running "./testsuite.py perfs/".
            # Handle this case as well.
            if (self.test_env['test_name'] not in cmdline and
               (self.test_env['test_name'] + '/') not in cmdline):
                self.result.set_status(
                    'DEAD', 'Must be specified on command line')
                raise Disabled()

    def resolve_backtraces(self, executable, out):
        """
        Resolve GNAT backtraces in `out` to symbolic.

        :param str executable: the binary
        :return: the modified output that includes symbolic backtraces
        """
        result = []
        next_is_backtrace = False
        for line in out.splitlines():
            result.append(line)
            if 'Call stack traceback locations:' in line:
                next_is_backtrace = True
            elif next_is_backtrace:
                next_is_backtrace = False

                if platform.system() == 'Darwin':
                    p = Run(['atos', '-o', executable] + line.split())
                    result.append(p.out)
                elif platform.system() == 'Linux':
                    p = Run(['addr2line', '--functions', '--demangle=gnat',
                             '--basenames', '--inlines', '-e', executable] +
                            line.split())
                    result.append(p.out)

        return '\n'.join(result)

    def run_exec(self, cmds):
        options = self.global_env['options']
        prefix = []
        stdout = PIPE
        if options.valgrind:
            prefix = ['valgrind', '--max-stackframe=3800000']
        elif options.leaks:
            prefix = ['valgrind', '--leak-check=full',
                      '--max-stackframe=3800000',
                      '--show-reachable=yes']
        elif options.gdb:
            prefix = ['gdb', '--args']
            stdout = None

        p = Run(cmds=prefix + cmds, output=stdout, error=STDOUT,
                cwd=self.working_dir)
        self.result.actual_output += self.resolve_backtraces(cmds[0], p.out)
        if p.status != 0:
            self.result.set_status('FAILED', 'Run failed')
            raise BuildError()

    def analyze(self):
        if self.test_env.get('sort_output', False):
            a = self.result.actual_output.split('\n')
            a.sort()
            self.result.actual_output = '\n'.join(a)

        return self.analyze_diff()

    def tear_up(self):
        super(AbstractDriver, self).tear_up()
        self.register_subst(os.getcwd(), '<pwd>')
        self.working_dir = os.path.join(
            self.global_env['test_dir'],
            self.test_env['test_name'])
        self.set_expected_output()

        self.result.actual_output = ''

    def tear_down(self):
        keep_project = self.global_env['options'].keep_project
        create_only = self.global_env['options'].create_projects
        if self.project_is_tmp and not keep_project and not create_only:
            rm(self.project)

    def run(self):
        try:
            self.create_project_if_needed(
                mains=self.test_env.get('mains', ['main.adb']))

            if self.global_env['options'].create_projects:
                self.result.set_status("DEAD", "only creating projects")
                return

            self.check_if_must_run()

            pre = self.test_env.get('pre', [])
            for p in pre:
                self.run_exec(p.split())

            self.do_run()
        except Disabled:
            pass
        except KeyboardInterrupt:   # make sure that tear_down() is run
            pass
        except Exception:
            raise


class BuildAndExec(AbstractDriver):
    """
    Builds a project, and run an executable. Compare its output to a baseline
    test.yaml should contains any of the following (the values given here are
    the default)::
        driver: 'build_and_exec'    # Mandatory
        exec: 'obj/main'            # or a list
        mode: 'Debug'               # build mode
    """

    def do_run(self):
        self.gprbuild(mode=self.test_env.get('mode', 'Debug'))

        execs = self.test_env.get('exec', 'obj/main')
        if not isinstance(execs, list):
            execs = [execs]

        for e in execs:
            execname = os.path.join(self.working_dir, e)
            self.run_exec([execname])


class Prove(AbstractDriver):
    """
    Prove all source code for a project. The test.yaml file should
    contain any of the following (the values given here are the default)::
        driver: 'prove'
        sources: []   # If unspecified, prove all
    """

    def do_run(self):
        self.gnatprove(sources=self.test_env.get('sources', []))


class ContainerTestsuite(Testsuite):
    TEST_SUBDIR = '.'
    DRIVERS = {'build_and_exec': BuildAndExec,
               'prove': Prove}
    default_driver = 'build_and_exec'

    def add_options(self):
        gr = self.main.create_option_group('Containers testuite')
        self.main.add_option_group(gr)

        gr.add_option(
            "-k", "--keep-project",
            default=False,
            action="store_true",
            help="Do not delete the project files created automatically for"
            " the tests")

        gr.add_option(
            '-c', '--create-projects',
            default=False,
            action='store_true',
            help='If set, only create all missing projects, but do not run')

        gr.add_option(
            '--gdb',
            default=False,
            action='store_true',
            help='Run tests with gdb. You should first compile with'
            '"make BUILD=Debug" to get better source locations')

        gr.add_option(
            '--valgrind',
            default=False,
            action='store_true',
            help='Run tests with valgrind. You should first compile with'
            '"make BUILD=Debug" to get better source locations')

        gr.add_option(
            '--leaks',
            default=False,
            action='store_true',
            help='Check for memory leaks in the tests')

    def tear_up(self):
        super(ContainerTestsuite, self).tear_up()

        # Add local projects to the path. In the nightly tests, this is
        # overridden by preseting the project path
        os.environ['GPR_PROJECT_PATH'] = \
            os.environ.get('GPR_PROJECT_PATH', '') + os.sep + \
            os.path.join(self.global_env['root_dir'], '..', 'src')

    def tear_down(self):
        # Print the testsuite result on the terminal for the convenience
        # of developers.
        super(ContainerTestsuite, self).tear_down()
        if not self.global_env['options'].create_projects:
            print("\n")
            print(file("out/new/report").read())

    def get_test_list(self, sublist):
        self.global_env.setdefault('containers', {})
        self.global_env['containers']['test_on_command_line'] = sublist
        return super(ContainerTestsuite, self).get_test_list(sublist)

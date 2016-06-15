from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.testsuite.result import Result
from gnatpython.ex import Run, STDOUT
from gnatpython.fileutils import mkdir, rm
import os


class BuildError(Exception):
    pass


class Disabled(Exception):
    pass


class AbstractDriver(TestDriver):
    def __init__(self, *args, **kwargs):
        super(AbstractDriver, self).__init__(*args, **kwargs)
        self.project_is_tmp = False
        self.project = None

    def create_project_if_needed(self):
        """
        Create a standard project if none exists in the test directory.
        `self.project` must be set
        """
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
with "../../src/shared";
with "../../src/conts";
project %(name)s is
   for Main use ("main.adb");
   for Object_Dir use "obj";
   package Compiler renames Shared.Compiler;
   package Builder renames Shared.Builder;
   package Binder renames Shared.Binder;
   package Linker renames Shared.Linker;
end %(name)s;""" % {'name': defaultname})

    def gprbuild(self):
        """
        Runs gprbuild on `self.project`
        """
        p = Run(cmds=['gprbuild', '-q', '-p', '-P', self.project],
                error=STDOUT,
                cwd=self.working_dir)
        self.result.actual_output = p.out
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
        self.result.actual_output = p.out
        if p.status != 0:
            self.result.set_status('FAILED', 'gnatprove failed')
            raise BuildError()

    def set_expected_output(self):
        """
        Set the expected output in `self.result`
        This is read from test.yaml:
            baseline: 'test.out'
        If it is the empty string or unspecified and test.out does not
        exists, no output is expected.
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
            if self.test_env['test_name'] not in cmdline:
                self.result.set_status(
                    'DEAD', 'Must be specified on command line')
                raise Disabled()

    def run_exec(self, cmds):
        p = Run(cmds=cmds, error=STDOUT, cwd=self.working_dir)
        self.result.actual_output = p.out
        if p.status != 0:
            self.result.set_status('FAILED', 'Run failed')
            raise BuildError()

    def analyze(self):
        return self.analyze_diff()

    def tear_up(self):
        super(AbstractDriver, self).tear_up()
        self.register_subst(os.getcwd(), '<pwd>')
        self.working_dir = os.path.join(
            self.global_env['test_dir'],
            self.test_env['test_name'])
        self.set_expected_output()

    def tear_down(self):
        keep_project = self.global_env['options'].keep_project
        create_only = self.global_env['options'].create_projects
        if self.project_is_tmp and not keep_project and not create_only:
            rm(self.project)

    def run(self):
        try:
            self.create_project_if_needed()

            if self.global_env['options'].create_projects:
                self.result.set_status("DEAD", "only creating projects")
                return

            self.check_if_must_run()
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
        pre: [],   # Commands to execute before the build
        project: 'default.gpr'
        exec: 'obj/main'
        baseline: 'test.out'
        manual: False  # true if only executed when specified as argument
    """

    def do_run(self):
        pre = self.test_env.get('pre', [])
        for p in pre:
            self.run_exec(p.split())

        self.gprbuild()

        execname = os.path.join(
            self.working_dir,
            self.test_env.get('exec', 'obj/main'))
        self.run_exec([execname])


class Prove(AbstractDriver):
    """
    Prove all source code for a project. The test.yaml file should
    contain any of the following (the values given here are the default)::
        driver: 'prove'
        project: 'default.gpr'
        sources: []   # If unspecified, prove all
        manual: False  # true if only executed when specified as argument
    """

    def do_run(self):
        self.gnatprove(sources=self.test_env.get('sources', []))


class ContainerTestsuite(Testsuite):
    TEST_SUBDIR = '.'
    DRIVERS = {'build_and_exec': BuildAndExec,
               'prove': Prove}
    default_driver = 'build_and_exec'

    def add_options(self):
        self.main.add_option(
            "-k", "--keep-project",
            default=False,
            action="store_true",
            help="Do not delete the project files created automatically for"
            " the tests")

        self.main.add_option(
            '-c', '--create-projects',
            default=False,
            action='store_true',
            help='If set, only create all missing projects, but do not run')

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

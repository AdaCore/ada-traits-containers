from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.testsuite.result import Result
from gnatpython.ex import Run, STDOUT
# from gnatpython.env import Env
from gnatpython.fileutils import mkdir, rm
import os


class BuildError(Exception):
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
        if os.path.isfile(self.project):
            return

        self.project_is_tmp = True
        file(self.project, "w").write("""
with "../../src/shared";
with "../../src/conts";
project Default is
   for Main use ("main.adb");
   for Object_Dir use "obj";
   package Compiler renames Shared.Compiler;
   package Builder renames Shared.Builder;
   package Binder renames Shared.Binder;
   package Linker renames Shared.Linker;
end Default;""")

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
        self.project = os.path.join(
            self.working_dir,
            self.test_env.get('project', 'default.gpr'))

    def tear_down(self):
        if self.project_is_tmp:
            rm(self.project)


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
    """

    def run(self):
        baseline = os.path.join(
            self.working_dir,
            self.test_env.get('baseline', 'test.out'))
        self.result.expected_output = file(baseline).read()

        self.create_project_if_needed()

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
    """

    def run(self):
        sources = self.test_env.get('sources', [])

        self.result.expected_output = ""
        self.create_project_if_needed()

        self.gnatprove(sources)


class ContainerTestsuite(Testsuite):
    TEST_SUBDIR = '.'
    DRIVERS = {'build_and_exec': BuildAndExec,
               'prove': Prove}
    default_driver = 'build_and_exec'

    def tear_up(self):
        pass

    def tear_down(self):
        # Print the testsuite result on the terminal for the convenience
        # of developers.
        print("\n")
        print(file("out/new/report").read())

#! /usr/bin/env python
"""usage: ./testsuite [OPTIONS]

Run Containers Testsuite"""

import os
from support import ContainerTestsuite

if __name__ == '__main__':
    ContainerTestsuite(os.path.dirname(__file__)).testsuite_main()

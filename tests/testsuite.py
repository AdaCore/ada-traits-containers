#! /usr/bin/env python
"""usage: ./testsuite [OPTIONS]

Run Containers Testsuite

Use option
   -k    do not delete the temporary project files created by the tests

"""

import os
from support import ContainerTestsuite

if __name__ == '__main__':
    try:
        ContainerTestsuite(os.path.dirname(__file__)).testsuite_main()
    except KeyboardInterrupt:
        print " interrupted !"

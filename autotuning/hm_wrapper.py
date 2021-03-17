#!/usr/bin/python

import os
import sys
from subprocess import Popen, PIPE
import argparse
from pathlib import Path

# global variables
PATH = Path().absolute()

parser = argparse.ArgumentParser()
parser.add_argument('--config_file', type=str)

config_file = ''

def main(config_file):
    """
    This method implements the communication protocol between the Chakong and Haimes function and HyperMapper.
    The protocol is specified in the HyperMapper wiki and it is basically an exchange of data via
    stdin and stdout using a csv-like format.
    """

    # change directory to ~
    os.chdir(os.path.expanduser("~"))

    cmd = [
        ".local/bin/hypermapper",
        str(config_file)
    ]
    # print("comanndon")
    print(cmd)  # Command to launch HyperMapper
    p = Popen(
        cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, encoding="utf-8"
    )  # Create a subprocess and launch HyperMapper


    i = 0
    while p.poll() is None:  # Check if the process is still running
        request = p.stdout.readline()
        p.stdout.flush()  # The first line is the request in the form: Request #_of_evaluation_requests
        if "End of HyperMapper" in request:  # This means that HyperMapper ended
            print(request)
            break
        elif "Best point found" in request:
            headers = p.stdout.readline()
            parameters_values = p.stdout.readline()
            p.stdout.readline()
            p.stdout.flush()
            sys.stderr.write("Best point found\n")
            sys.stderr.write(f"Headers: {headers}")
            sys.stderr.write(f"Values: {parameters_values}")
            continue
        elif "warning" in request:
            continue

        # write request information to parent process
        sys.stdout.write(request)
        sys.stdout.flush()

        # read in header for str to hypermapper from parent process
        str_to_hypermapper = sys.stdin.readline()

        num_of_eval_requests = int(
            request.split(" ")[1]
        )  # Get the #_of_evaluation_requests
        headers = p.stdout.readline()
        p.stdin.flush()  # The second line contains the header in the form: x1,x2
        sys.stdout.write(headers)

        for row in range(
            num_of_eval_requests
        ):  # Go through the rest of the eval requests
            parameters_values = (
                p.stdout.readline()
            )  # This is an eval request in the form: number_x1, number_x2

            # write parameter request to parent process
            sys.stdout.write(parameters_values)
            sys.stdout.flush()

            # receive str to hypermapper from parent process
            str_to_hypermapper += sys.stdin.readline()


        # write str to hypermapper to hypermapper
        p.stdin.write(str_to_hypermapper)
        p.stdin.flush()  # Reply to HyperMapper with all the evaluations

        i += 1



if __name__ == "__main__":
    # parse config file
    args = parser.parse_args()
    parameters_file = str(PATH) + "/" + str(args.config_file)

# call main
    main(parameters_file)
    



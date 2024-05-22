import ml_dtypes
import numpy as np
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('in_dtype')
parser.add_argument('out_dtype')
parser.add_argument('--n_samples', default=-1, help='-1 is exhaustive')
subparsers = parser.add_subparsers(dest = 'op')
mul = subparsers.add_parser('mul')
conv = subparsers.add_parser('conv')

args = parser.parse_args()

def uint_dtype(dtype):
    return np.dtype(f'uint{dtype.itemsize * 8}')

def gen_inputs(dtype, n_operands, n_samples):
    if n_samples == -1:
        bytes = dtype.itemsize * n_operands
        bits = bytes * 8
        for i in range(2**bits):
            yield np.frombuffer(i.to_bytes(bytes, byteorder='little'), dtype=dtype)
    else:
        for _ in range(n_samples):
            udtype = uint_dtype(dtype)
            yield np.random.randint(np.iinfo(udtype).max, size=n_operands, dtype = udtype).view(dtype)

in_dtype = np.dtype(args.in_dtype)
out_dtype = np.dtype(args.out_dtype)

if args.op == 'mul':
    for inputs in gen_inputs(in_dtype, 2, args.n_samples):
        res = (inputs[0].astype(np.float128) * inputs[1].astype(np.float128)).astype(out_dtype)
        print(f"{inputs[0].view(uint_dtype(in_dtype)):x} {inputs[1].view(uint_dtype(in_dtype)):x} {res.view(uint_dtype(out_dtype)):x}")
elif args.op == 'conv':
    for inputs in gen_inputs(in_dtype, 1, args.n_samples):
        res = inputs[0].astype(out_dtype)
        print(f"{inputs[0].view(uint_dtype(in_dtype)):x} {res.view(uint_dtype(out_dtype)):x}")
else:
    parser.print_usage()

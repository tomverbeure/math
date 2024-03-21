import ml_dtypes
import numpy as np
import sys

if sys.argv[1] == "f8*f8=b16":
    for i in range(2**16):
        val_l = np.uint8(i & 0xFF).view(np.dtype('float8_e5m2fnuz'))
        val_r = np.uint8(i >> 8).view(np.dtype('float8_e5m2fnuz'))
        res = val_l.astype(np.dtype('bfloat16')) * val_r.astype(np.dtype('bfloat16'))
        print(f"{val_l.view(np.uint8):x} {val_r.view(np.uint8):x} {res.view(np.uint16):x}")
elif sys.argv[1] == "float8_e5m2fnuz_to_bfloat16":
    for i in range(2**8):
        val = np.uint8(i).view(np.dtype('float8_e5m2fnuz'))
        res = val.astype(np.dtype('bfloat16'))
        print(f"{val.view(np.uint8):x} {res.view(np.uint16):x}")
elif sys.argv[1] == "bfloat16_to_float8_e5m2fnuz":
    for i in range(2**16):
        val = np.uint16(i).view(np.dtype('bfloat16'))
        res = val.astype(np.dtype('float8_e5m2fnuz'))
        print(f"{val.view(np.uint16):x} {res.view(np.uint8):x}")

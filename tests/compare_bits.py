import numpy as np
import sys


def data_from_file(filepath: str) -> tuple[int, np.ndarray]:
    with open(filepath, "rb") as f:
        n: int = np.fromfile(f, dtype=np.intc, count=1)[0]
        x: np.ndarray = np.fromfile(f, dtype=np.int64, count=n)
    return n, x


path_1 = sys.argv[1]
path_2 = sys.argv[2]

for fun in [
    "cos",
    "sin",
    "exp",
    "log",
    "atan",
]:
    _, x1 = data_from_file(path_1 + f"/x_{fun}.bin")
    _, y1 = data_from_file(path_1 + f"/y_{fun}.bin")
    _, x2 = data_from_file(path_2 + f"/x_{fun}.bin")
    _, y2 = data_from_file(path_2 + f"/y_{fun}.bin")

    assert np.size(x1) == np.size(x2), f"br_{fun}: X input not same size"
    assert np.size(y1) == np.size(y2), f"br_{fun}: Y outnot same size"
    assert np.array_equal(x1, x2), f"br_{fun}: X inputs not equal"
    assert np.array_equal(y1, y2), f"br_{fun}: Y outputs not equal"

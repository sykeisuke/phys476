"""2D convolution (as in Keras Conv2D, 'valid', stride 1) and 2x2 max pooling."""
import numpy as np


def conv2d(img, k):
    H, W = img.shape; kh, kw = k.shape
    out = np.zeros((H - kh + 1, W - kw + 1))
    for i in range(out.shape[0]):
        for j in range(out.shape[1]):
            out[i, j] = np.sum(img[i:i + kh, j:j + kw] * k)   # same k everywhere
    return out


def maxpool2(x):
    H, W = x.shape[0] // 2 * 2, x.shape[1] // 2 * 2
    return x[:H, :W].reshape(H // 2, 2, W // 2, 2).max(axis=(1, 3))


def params_dense(n_in, n_out):
    return n_in * n_out + n_out


def params_conv(kh, kw, c_in, filters):
    return kh * kw * c_in * filters + filters


if __name__ == "__main__":
    img = np.zeros((6, 6)); img[:, 3:] = 1          # vertical edge
    k = np.array([[-1, 0, 1]] * 3)                  # vertical-edge filter
    fm = conv2d(img, k)
    pooled = maxpool2(fm)
    print(fm); print(pooled)
    print("Dense 784->200:", params_dense(784, 200), " Conv 3x3x1, 32 filters:",
          params_conv(3, 3, 1, 32))
    ok = (fm.shape == (4, 4) and pooled.shape == (2, 2) and fm[:, 1:3].min() == 3
          and params_dense(784, 200) == 157000 and params_conv(3, 3, 1, 32) == 320)
    print("PASS" if ok else "FAIL")

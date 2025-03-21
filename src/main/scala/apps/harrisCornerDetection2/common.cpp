#include <stdlib.h>
#include <stdio.h>
#include <random>
#include <cmath>
#include <algorithm>
#include <vector>

extern "C" {
#include "ocl/ocl.h"
AccessFlags operator|(AccessFlags a, AccessFlags b) {
    return static_cast<AccessFlags>(static_cast<int>(a) | static_cast<int>(b)); }
}

// TODO: pass these in from Scala?
const int bd_h = 16;
const int bd_w = 32;

struct ErrorStats {
    float min_val;
    float max_val;
    double min;
    double max;
    double max_mse;
};

void init_error_stats(ErrorStats* es) {
    es->min_val = 1.f / 0.f;
    es->max_val = -1.f / 0.f;
    es->min = 1.f / 0.f;
    es->max = 0.f;
    es->max_mse = 0.f;
}

void accumulate_error_stats(ErrorStats* es, float* a, float* b, int h, int w) {
    double square_sum = 0.f;
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            es->min_val = std::min(es->min_val, b[y*w + x]);
            es->max_val = std::max(es->max_val, b[y*w + x]);
            double delta = a[y*w + x] - b[y*w + x];
            double d_abs = abs(delta);
            es->min = std::min(es->min, d_abs);
            es->max = std::max(es->max, d_abs);
            square_sum += d_abs * d_abs;
        }
    }
    es->max_mse = std::max(es->max_mse, square_sum / (h * w));
}

void finish_error_stats(ErrorStats* es, float tolerated_per_pixel, float tolerated_mse) {
    fprintf(stderr, "errors: [%.4lf - %.4lf] with %.4lf MSE\n",
        es->min, es->max, es->max_mse);
    if (es->max > tolerated_per_pixel || es->max_mse > tolerated_mse) {
        fprintf(stderr, "maximum tolerated error: %.4f per pixel and %.4f MSE\n",
            tolerated_per_pixel, tolerated_mse);
        fprintf(stderr, "value range: [%.4f - %.4f]\n", es->min_val, es->max_val);
        exit(EXIT_FAILURE);
    }
}

void conv3x3_gold(float* out,
                  int h, int w,
                  const float* in,
                  const float* weights)
{
    for (int y = 0; y < (h - 2*bd_h); y++) {
        int r0 = (y + bd_h - 1) * w;
        int r1 = (y + bd_h) * w;
        int r2 = (y + bd_h + 1) * w;
        for (int x = 0; x < (w - 2*bd_w); x++) {
            int c0 = x + (bd_w - 1);
            int c1 = x + bd_w;
            int c2 = x + (bd_w + 1);
            out[y*(w - 2*bd_w)+x] = (
                weights[0]*in[r0+c0] + weights[1]*in[r0+c1] + weights[2]*in[r0+c2] +
                weights[3]*in[r1+c0] + weights[4]*in[r1+c1] + weights[5]*in[r1+c2] +
                weights[6]*in[r2+c0] + weights[7]*in[r2+c1] + weights[8]*in[r2+c2]
            );
        }
    }
}

void sobelX_gold(float* out,
                 int h, int w,
                 const float* in)
{
    float weights[9] = {
        -1.f/8.f, 0.f, 1.f/8.f,
        -2.f/8.f, 0.f, 2.f/8.f,
        -1.f/8.f, 0.f, 1.f/8.f
    };
    conv3x3_gold(out, h, w, in, weights);
}

void sobelY_gold(float* out,
                 int h, int w,
                 const float* in)
{
    float weights[9] = {
        -1.f/8.f, -2.f/8.f, -1.f/8.f,
         0.f/8.f,  0.f/8.f,  0.f/8.f,
         1.f/8.f,  2.f/8.f,  1.f/8.f
    };
    conv3x3_gold(out, h, w, in, weights);
}

void binomial_gold(float* out,
                   int h, int w,
                   const float* in)
{
    float weights[9] = {
        1.f/16.f, 2.f/16.f, 1.f/16.f,
        2.f/16.f, 4.f/16.f, 2.f/16.f,
        1.f/16.f, 2.f/16.f, 1.f/16.f
    };
    conv3x3_gold(out, h, w, in, weights);
}

void mul_gold(float* out,
              int h, int w,
              const float* a,
              const float* b)
{
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            out[y*w + x] = a[y*w + x] * b[y*w + x];
        }
    }
}

void coarsity_gold(float* out,
                   int h, int w,
                   const float* sxx,
                   const float* sxy,
                   const float* syy,
                   float kappa)
{
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            float det = sxx[y*w + x] * syy[y*w + x] - sxy[y*w + x] * sxy[y*w + x];
            float trace = sxx[y*w + x] + syy[y*w + x];
            out[y*w + x] = det - kappa * trace * trace;
        }
    }
}

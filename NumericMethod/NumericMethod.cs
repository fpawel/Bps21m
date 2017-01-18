using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NumericMethod
{
    public class GaussInterpolation
    {
        static decimal[,] makeMatrix(decimal[] x, decimal[] y)
        {
            int degree = x.Length;
            var matr = new decimal[degree, degree + 1];
            for (int i = 0; i < degree; ++i)
            {
                decimal s = x[i];
                decimal r = 1;
                matr[i, degree] = y[i];
                for (int j = 0; j < degree; ++j)
                {
                    matr[i, j] = r;
                    r *= s;
                }
            }
            return matr;
        }

        public static decimal[] calculate(decimal[] x, decimal[] y)
        {
            int degree = x.Length;
            System.Diagnostics.Debug.Assert(degree > 0 && degree == y.Length);
            var matr = makeMatrix(x, y);
            var koefs = new decimal[degree];
            decimal s, r;
            for (int k = 0; k < degree; ++k)
            {
                int k1 = k + 1;
                s = matr[k, k];
                int j = k;
                for (int i = k1; i < degree; ++i)
                {
                    r = matr[i, k];
                    if (Math.Abs(r) > Math.Abs(s))
                    {
                        s = r;
                        j = i;
                    }
                }
                if (j != k) for (int i = k; i < degree + 1; ++i)
                    {
                        r = matr[k, i];
                        matr[k, i] = matr[j, i];
                        matr[j, i] = r;
                    }
                if (s == 0) return koefs;

                for (int j_ = k1; j_ < degree + 1; ++j_) matr[k, j_] /= s;
                for (int i = k1; i < degree; ++i)
                {
                    r = matr[i, k];
                    for (int j_ = k1; j_ < degree + 1; ++j_)
                        matr[i, j_] = matr[i, j_] - matr[k, j_] * r;
                }
            }
            for (int i = degree - 1; i > -1; --i)
            {
                s = matr[i, degree];
                for (int j = i + 1; j < degree; ++j)
                    s = s - matr[i, j] * koefs[j];
                koefs[i] = s;
            }
            return koefs;
        }
    }


    public class LinearAlgebraicEquationsSystem
    {
        public static decimal[] Solv(int n, decimal[,] A)
        {
            decimal R;
            decimal s = 0m;
            int i;
            int j;
            int k;
            int k1;
            int n1;
            n1 = n + 1;
            for (k = 1; k <= n; k++)
            {
                k1 = k + 1;
                s = A[k, k];
                j = k;
                for (i = k + 1; i <= n; i++)
                {
                    R = A[i, k];
                    if (Math.Abs(R) > Math.Abs(s))
                    {
                        s = R;
                        j = i;
                    }
                }
                if (s == 0)
                {
                    return new decimal[0];
                }
                if (j != k)
                {
                    for (i = k; i <= n1; i++)
                    {
                        R = A[k, i];
                        A[k, i] = A[j, i];
                        A[j, i] = R;
                    }
                }
                for (j = k1; j <= n1; j++)
                {
                    A[k, j] = A[k, j] / s;
                }
                for (i = k1; i <= n; i++)
                {
                    R = A[i, k];
                    for (j = k1; j <= n1; j++)
                    {
                        A[i, j] = A[i, j] - A[k, j] * R;
                    }
                }
            }


            if (s == 0)
            {
                return new decimal[0];
            }
            else
            {
                var X = new decimal[n + 2];
                for (i = n; i >= 1; i--)
                {
                    s = A[i, n1];
                    for (j = i + 1; j <= n; j++)
                    {
                        s = s - A[i, j] * X[j];
                    }
                    X[i] = s;
                }
                return X;
            }
        }

    }

}

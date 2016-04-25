/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with this
 * work for additional information regarding copyright ownership. The ASF
 * licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
 * or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package stats;

import java.util.Arrays;
import java.util.Comparator;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.exception.DimensionMismatchException;
import org.apache.commons.math3.linear.BlockRealMatrix;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.util.FastMath;
import org.apache.commons.math3.util.Pair;

/**
 * Implementation of Kendall's Tau-b rank correlation</a>.
 * <p/>
 * A pair of observations (x<sub>1</sub>, y<sub>1</sub>) and (x<sub>2</sub>,
 * y<sub>2</sub>) are considered <i>concordant</i> if x<sub>1</sub> &lt;
 * x<sub>2</sub> and y<sub>1</sub> &lt; y<sub>2</sub> or x<sub>2</sub> &lt;
 * x<sub>1</sub> and y<sub>2</sub> &lt; y<sub>1</sub>. The pair is
 * <i>discordant</i> if x<sub>1</sub> &lt; x<sub>2</sub> and y<sub>2</sub> &lt;
 * y<sub>1</sub> or x<sub>2</sub> &lt; x<sub>1</sub> and y<sub>1</sub> &lt;
 * y<sub>2</sub>. If either x<sub>1</sub> = x<sub>2</sub> or y<sub>1</sub> =
 * y<sub>2</sub>, the pair is neither concordant nor discordant.
 * <p/>
 * Kendall's Tau-b is defined as:
 * <p/>
 * <pre>
 * tau<sub>b</sub> = (n<sub>c</sub> - n<sub>d</sub>) / sqrt((n<sub>0</sub> - n<sub>1</sub>) * (n<sub>0</sub> - n<sub>2</sub>))
 * </pre>
 * <p/>
 * where:
 * <ul>
 * <li>n<sub>0</sub> = n * (n - 1) / 2</li>
 * <li>n<sub>c</sub> = Number of concordant pairs</li>
 * <li>n<sub>d</sub> = Number of discordant pairs</li>
 * <li>n<sub>1</sub> = sum of t<sub>i</sub> * (t<sub>i</sub> - 1) / 2 for all i</li>
 * <li>n<sub>2</sub> = sum of u<sub>j</sub> * (u<sub>j</sub> - 1) / 2 for all j</li>
 * <li>t<sub>i</sub> = Number of tied values in the i<sup>th</sup> group of ties
 * in x</li>
 * <li>u<sub>j</sub> = Number of tied values in the j<sup>th</sup> group of ties
 * in y</li>
 * </ul>
 * <p/>
 * This implementation uses the O(n log n) algorithm described in William R.
 * Knight's 1966 paper "A Computer Method for Calculating Kendall's Tau with
 * Ungrouped Data" in the Journal of the American Statistical Association.
 *
 * @version $Id$
 * @see <a
 * href="http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient">
 * Kendall tau rank correlation coefficient (Wikipedia)</a>
 * @see <a href="http://www.jstor.org/stable/2282833">A Computer Method for
 * Calculating Kendall's Tau with Ungrouped Data</a>
 * @since 3.3
 */
@SuppressWarnings("all")
final class KendallsCorrelation {

    /**
     * correlation matrix
     */
    private final RealMatrix correlationMatrix;

    /**
     * Create a KendallsCorrelation instance without data.
     */
    public KendallsCorrelation() {
        correlationMatrix = null;
    }

    /**
     * Create a KendallsCorrelation from a rectangular array whose columns
     * represent values of variables to be correlated.
     *
     * @param data rectangular array with columns representing variables
     * @throws IllegalArgumentException if the input data array is not rectangular with at least two rows
     *                                  and two columns.
     */
    public KendallsCorrelation(double[][] data) {
        this(MatrixUtils.createRealMatrix(data));
    }

    /**
     * Create a KendallsCorrelation from a RealMatrix whose columns represent
     * variables to be correlated.
     *
     * @param matrix matrix with columns representing variables to correlate
     */
    public KendallsCorrelation(RealMatrix matrix) {
        correlationMatrix = computeCorrelationMatrix(matrix);
    }

    /**
     * Returns the correlation matrix.
     *
     * @return correlation matrix
     */
    public RealMatrix getCorrelationMatrix() {
        return correlationMatrix;
    }

    /**
     * Computes the Kendall's Tau rank correlation matrix for the columns of the
     * input matrix.
     *
     * @param matrix matrix with columns representing variables to correlate
     * @return correlation matrix
     */
    public RealMatrix computeCorrelationMatrix(final RealMatrix matrix) {
        int nVars = matrix.getColumnDimension();
        RealMatrix outMatrix = new BlockRealMatrix(nVars, nVars);
        for (int i = 0; i < nVars; i++) {
            for (int j = 0; j < i; j++) {
                double corr = correlation(matrix.getColumn(i), matrix.getColumn(j)).getFirst();
                outMatrix.setEntry(i, j, corr);
                outMatrix.setEntry(j, i, corr);
            }
            outMatrix.setEntry(i, i, 1d);
        }
        return outMatrix;
    }

    /**
     * Computes the Kendall's Tau rank correlation matrix for the columns of the
     * input rectangular array. The columns of the array represent values of
     * variables to be correlated.
     *
     * @param matrix matrix with columns representing variables to correlate
     * @return correlation matrix
     */
    public RealMatrix computeCorrelationMatrix(final double[][] matrix) {
        return computeCorrelationMatrix(new BlockRealMatrix(matrix));
    }

    /**
     * Computes the Kendall's Tau rank correlation coefficient between the two
     * arrays.
     *
     * @param xArray first data array
     * @param yArray second data array
     * @return Returns Kendall's Tau rank correlation coefficient for the two
     * arrays
     * @throws DimensionMismatchException if the arrays lengths do not match
     */
    public Pair<Double, Double> correlation(final double[] xArray, final double[] yArray) throws DimensionMismatchException {

        if (xArray.length != yArray.length) {
            throw new DimensionMismatchException(xArray.length, yArray.length);
        }

        final int n = xArray.length;
        final double numPairs = ((double) n) * (n - 1) / 2; // to avoid overflow

        @SuppressWarnings("unchecked") Pair<Double, Double>[] pairs = new Pair[n];
        for (int i = 0; i < n; i++) {
            pairs[i] = new Pair<Double, Double>(xArray[i], yArray[i]);
        }

        Arrays.sort(pairs, new Comparator<Pair<Double, Double>>() {
            @Override
            public int compare(Pair<Double, Double> pair1, Pair<Double, Double> pair2) {
                int compareFirst = pair1.getFirst().compareTo(pair2.getFirst());
                return compareFirst != 0 ? compareFirst : pair1.getSecond().compareTo(pair2.getSecond());
            }
        });
        long tiedXPairs = 0;
        long tiedXYPairs = 0;
        long vt = 0;
        long consecutiveXTies = 1;
        long consecutiveXTiesSecondOrder = 1;
        long consecutiveXYTies = 1;
        Pair<Double, Double> prev = pairs[0];
        for (int i = 1; i < n; i++) {
            final Pair<Double, Double> curr = pairs[i];
            if (curr.getFirst().equals(prev.getFirst())) {
                consecutiveXTies++;
                if (curr.getSecond().equals(prev.getSecond())) {
                    consecutiveXYTies++;
                } else {
                    tiedXYPairs += consecutiveXYTies * (consecutiveXYTies - 1) / 2;
                    consecutiveXYTies = 1;
                }
            } else {
                tiedXPairs += consecutiveXTies * (consecutiveXTies - 1) / 2;
                consecutiveXTiesSecondOrder += consecutiveXTies * (consecutiveXTies - 1) * (consecutiveXTies - 2);
                vt += consecutiveXTies * (consecutiveXTies - 1) * (2 * consecutiveXTies + 5);
                consecutiveXTies = 1;
                tiedXYPairs += consecutiveXYTies * (consecutiveXYTies - 1) / 2;
                consecutiveXYTies = 1;
            }
            prev = curr;
        }
        tiedXPairs += consecutiveXTies * (consecutiveXTies - 1) / 2;
        vt += consecutiveXTies * (consecutiveXTies - 1) * (2 * consecutiveXTies + 5);
        consecutiveXTiesSecondOrder += consecutiveXTies * (consecutiveXTies - 1) * (consecutiveXTies - 2);
        tiedXYPairs += consecutiveXYTies * (consecutiveXYTies - 1) / 2;

        double swaps = 0;
        @SuppressWarnings("unchecked") Pair<Double, Double>[] pairsDestination = new Pair[n];
        for (int segmentSize = 1; segmentSize < n; segmentSize <<= 1) {
            for (int offset = 0; offset < n; offset += 2 * segmentSize) {
                int i = offset;
                final int iEnd = FastMath.min(i + segmentSize, n);
                int j = iEnd;
                final int jEnd = FastMath.min(j + segmentSize, n);

                int copyLocation = offset;
                while (i < iEnd || j < jEnd) {
                    if (i < iEnd) {
                        if (j < jEnd) {
                            if (pairs[i].getSecond().compareTo(pairs[j].getSecond()) <= 0) {
                                pairsDestination[copyLocation] = pairs[i];
                                i++;
                            } else {
                                pairsDestination[copyLocation] = pairs[j];
                                j++;
                                swaps += iEnd - i;
                            }
                        } else {
                            pairsDestination[copyLocation] = pairs[i];
                            i++;
                        }
                    } else {
                        pairsDestination[copyLocation] = pairs[j];
                        j++;
                    }
                    copyLocation++;
                }
            }
            final Pair<Double, Double>[] pairsTemp = pairs;
            pairs = pairsDestination;
            pairsDestination = pairsTemp;
        }

        long tiedYPairs = 0;
        long vu = 0;
        long consecutiveYTies = 1;
        long consecutiveYTiesSecondOrder = 0;
        prev = pairs[0];
        for (int i = 1; i < n; i++) {
            final Pair<Double, Double> curr = pairs[i];
            if (curr.getSecond().equals(prev.getSecond())) {
                consecutiveYTies++;
            } else {
                tiedYPairs += consecutiveYTies * (consecutiveYTies - 1) / 2;
                consecutiveYTiesSecondOrder += consecutiveYTies * (consecutiveYTies - 1) * (consecutiveYTies - 2);
                vu += consecutiveYTies * (consecutiveYTies - 1) * (2 * consecutiveYTies + 5);
                consecutiveYTies = 1;
            }
            prev = curr;
        }
        tiedYPairs += consecutiveYTies * (consecutiveYTies - 1) / 2;
        consecutiveYTiesSecondOrder += consecutiveYTies * (consecutiveYTies - 1) * (consecutiveYTies - 2);
        vu += consecutiveYTies * (consecutiveYTies - 1) * (2 * consecutiveYTies + 5);

        double concordantMinusDiscordant = numPairs - tiedXPairs - tiedYPairs + tiedXYPairs - 2 * swaps;
        double denom = FastMath.sqrt((numPairs - tiedXPairs) * (numPairs - tiedYPairs));
        double v0 = n * (n - 1) * (2 * n + 5);
        double v1 = (double)tiedXPairs * tiedYPairs / numPairs;
        double v2 = n < 3 ? 0 : consecutiveXTiesSecondOrder * consecutiveYTiesSecondOrder / (9.0 * n * (n - 1) * (n - 2));
        double v = ((v0 - vt - vu) / 18) + v1 + v2;
        double zB = concordantMinusDiscordant / FastMath.sqrt(v);
        return new Pair(denom != 0 ? concordantMinusDiscordant / denom : 0., 2 * new NormalDistribution().cumulativeProbability(-1 * Math.abs(zB)));
    }
}
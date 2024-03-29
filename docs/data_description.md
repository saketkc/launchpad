# train.csv

## There are multiple radar observations over the course of an hour, but only one gauge observation. Hence there are multiple rows with same id.

- Id:  A unique number for the set of observations over an hour at a gauge.
- minutes_past:  For each set of radar observations, the minutes past the top of the hour that the radar observations were carried out.  Radar observations are snapshots at that point in time.
- radardist_km:  Distance of gauge from the radar whose observations are being reported.
- Ref:  Radar reflectivity in km
- Ref_5x5_10th:   10th percentile of reflectivity values in 5x5 neighborhood around the gauge.
- Ref_5x5_50th:   50th percentile
- Ref_5x5_90th:   90th percentile
- RefComposite:  Maximum reflectivity in the vertical column above gauge.  In dBZ.
- RefComposite_5x5_10th
- RefComposite_5x5_50th
- RefComposite_5x5_90th
- RhoHV:  Correlation coefficient (unitless)
- RhoHV_5x5_10th
- RhoHV_5x5_50th
- RhoHV_5x5_90th
- Zdr:    Differential reflectivity in dB
- Zdr_5x5_10th
- Zdr_5x5_50th
- Zdr_5x5_90th
- Kdp:  Specific differential phase (deg/km)
- Kdp_5x5_10th
- Kdp_5x5_50th
- Kdp_5x5_90th
- Expected:  Actual gauge observation in mm at the end of the hour.

